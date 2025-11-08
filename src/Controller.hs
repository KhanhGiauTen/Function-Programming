module Controller
  ( initialStocks
  , initialPortfolio
  , stepPrice
  , priceGenerator
  , dispatcher
  , applyOrderSTM
  , controllerLoop
  ) where

import Types
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import System.Random (randomRIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime)
import Brick.BChan (BChan, writeBChan) -- Cần import BChan

-- ========== Config & Init ==========

initialStocks :: IO [Stock]
initialStocks = pure
  [ Stock "AAA" 100.0 0.02 0.0
  , Stock "BBB" 50.0  0.03 0.0
  , Stock "CCC" 200.0 0.015 0.0
  , Stock "DDD" 10.0  0.05  0.0
  ]

initialPortfolio :: Portfolio
initialPortfolio = Portfolio { cash = 10000.0, holdings = Map.empty }

-- ========== Random walk generator ==========

stepPrice :: Double -> Double -> Price -> IO Price
stepPrice vol trend p = do
  r <- randomRIO (-1.0, 1.0)
  let delta = vol * r + trend
      p' = p * (1.0 + delta)
  pure $ max 0.01 p'

-- priceGenerator now respects a shutdown TVar; when shutdown becomes True it exits
priceGenerator :: TVar Bool -> TChan PriceEvent -> Stock -> IO ()
priceGenerator shutdown outChan stock0 = do
  let sid = sId stock0
      loop sprice = do
        shouldStop <- readTVarIO shutdown
        if shouldStop
          then pure ()
          else do
            now <- getCurrentTime
            p' <- stepPrice (sVolatility stock0) (sTrend stock0) sprice
            let pe = PriceEvent sid p' now
            atomically $ putTChan outChan pe
            n <- randomRIO (200000, 800000) -- sleep 200-800ms
            threadDelay n
            loop p'
  loop (sPrice stock0)

-- dispatcher: read from price TChan -> update tvStocks -> write to bchan
dispatcher :: TChan PriceEvent -> BChan AppEvent -> TVar (Map StockId Stock) -> IO ()
dispatcher tchan bchan tvStocks = forever $ do
  pe <- atomically $ readTChan tchan
  let sid = peStock pe
      p = pePrice pe
  -- Cập nhật snapshot
  atomically $ modifyTVar' tvStocks $ Map.adjust (\s -> s { sPrice = p }) sid
  -- Gửi cho UI
  writeBChan bchan (EPrice pe)

-- ========== Controller: process Orders ==========

applyOrderSTM :: TVar Portfolio -> Map StockId Stock -> Order -> STM (Either String Trade)
applyOrderSTM pVar _snap ord = do -- _snap không được dùng, nhưng giữ để tương thích
  p <- readTVar pVar
  let curPrice = oPrice ord
      total = realToFrac (oQty ord) * curPrice
  case oType ord of
    Buy -> if cash p >= total
           then do
             let newCash = cash p - total
                 h = Map.insertWith (+) (oStock ord) (oQty ord) (holdings p)
                 trade = Trade (oStock ord) (oQty ord) curPrice (oTime ord) Buy
             writeTVar pVar (Portfolio newCash h)
             pure (Right trade)
           else pure (Left "Insufficient cash")
    Sell -> let curHold = Map.findWithDefault 0 (oStock ord) (holdings p)
            in if curHold >= oQty ord
               then do
                 let newHold = curHold - oQty ord
                     h' = if newHold == 0 then Map.delete (oStock ord) (holdings p) else Map.insert (oStock ord) newHold (holdings p)
                     newCash = cash p + total
                     trade = Trade (oStock ord) (oQty ord) curPrice (oTime ord) Sell
                 writeTVar pVar (Portfolio newCash h')
                 pure (Right trade)
               else pure (Left "Insufficient holdings")

-- controller loop reads Orders from TChan and applies them
controllerLoop :: TChan Order -> TVar Portfolio -> TVar [Trade] -> TVar (Map StockId Stock) -> IO ()
controllerLoop orderChan pVar tradeLog tvStocks = forever $ do
  -- Chờ lệnh từ UI
  ord <- atomically $ readTChan orderChan
  
  -- Lấy giá thị trường hiện tại từ snapshot
  snap <- readTVarIO tvStocks
  now <- getCurrentTime
  
  case Map.lookup (oStock ord) snap of
    Nothing -> putStrLn $ "Unknown stock in order: " ++ oStock ord
    Just stock -> do
      -- Tạo lệnh hoàn chỉnh với giá và thời gian (giả sử là lệnh thị trường)
      let fullOrd = ord { oPrice = sPrice stock, oTime = now }
      
      -- Thực thi lệnh trong STM
      res <- atomically $ applyOrderSTM pVar snap fullOrd
      
      case res of
        Left err -> putStrLn $ "Order failed: " ++ err
        Right trade -> do
          putStrLn $ "Executed trade: " ++ show trade
          -- Cập nhật trade log
          atomically $ modifyTVar' tradeLog (take 50 . (trade:))