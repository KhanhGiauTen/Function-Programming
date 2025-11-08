module Controller
  ( initialStocks
  , initialPortfolio
  , stepPrice
  , priceGenerator
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
import Data.Time.Clock (getCurrentTime, UTCTime)
-- DÒNG BỊ LỖI 'import Brick.BChan' ĐÃ ĐƯỢC XÓA

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

priceGenerator :: TChan PriceEvent -> Stock -> IO ()
priceGenerator outChan stock0 = do
  let sid = sId stock0
      loop sprice = do
        now <- getCurrentTime
        p' <- stepPrice (sVolatility stock0) (sTrend stock0) sprice
        let pe = PriceEvent sid p' now
        atomically $ writeTChan outChan pe
        n <- randomRIO (200000, 800000) -- sleep 200-800ms
        threadDelay n
        loop p'
  loop (sPrice stock0)

-- ========== Controller: process Orders ==========

-- --- BẮT ĐẦU THAY ĐỔI LỚN ---

-- Dùng ClientMessage thay vì Order
applyOrderSTM :: TVar Portfolio -> Map StockId Stock -> ClientMessage -> UTCTime -> STM (Either String Trade)
applyOrderSTM pVar snap msg time = do
  p <- readTVar pVar
  
  case Map.lookup (oStock msg) snap of
    Nothing -> pure (Left "Unknown stock")
    Just stock -> do
      let curPrice = sPrice stock
          orderQty = oQty msg
          totalCost = realToFrac orderQty * curPrice
          stockId = oStock msg

      case oType msg of
        Buy -> if cash p >= totalCost
               then do
                 let newCash = cash p - totalCost
                 
                 -- Tính toán giá vốn trung bình mới
                 let oldHolding = Map.findWithDefault (HoldingData 0 0) stockId (holdings p)
                     oldQty = hQty oldHolding
                     oldAvgCost = hAvgCost oldHolding
                     
                     newTotalQty = oldQty + orderQty
                     -- Tính giá trị tổng mới (total value)
                     newTotalValue = (fromIntegral oldQty * oldAvgCost) + (fromIntegral orderQty * curPrice)
                     -- Tính giá trung bình mới
                     newAvgCost = if newTotalQty == 0 then 0 else newTotalValue / fromIntegral newTotalQty
                     
                     newHoldings = Map.insert stockId (HoldingData newTotalQty newAvgCost) (holdings p)
                     trade = Trade stockId orderQty curPrice time Buy
                     
                 writeTVar pVar (Portfolio newCash newHoldings)
                 pure (Right trade)
               else pure (Left "Insufficient cash")
               
        Sell -> do
          let oldHolding = Map.findWithDefault (HoldingData 0 0) stockId (holdings p)
              oldQty = hQty oldHolding

          if oldQty >= orderQty
             then do
               let newCash = cash p + totalCost
                   newTotalQty = oldQty - orderQty
                   
                   -- Khi bán, giá vốn trung bình không đổi
                   newAvgCost = if newTotalQty == 0 then 0 else hAvgCost oldHolding
                   
                   -- Cập nhật hoặc xóa cổ phiếu khỏi danh mục
                   newHoldings = if newTotalQty == 0
                                 then Map.delete stockId (holdings p)
                                 else Map.insert stockId (HoldingData newTotalQty newAvgCost) (holdings p)
                                 
                   trade = Trade stockId orderQty curPrice time Sell
                   
               writeTVar pVar (Portfolio newCash newHoldings)
               pure (Right trade)
             else pure (Left "Insufficient holdings")

-- --- KẾT THÚC THAY ĐỔI LỚN ---

-- Controller loop đọc từ TChan ClientMessage (thay vì Order)
controllerLoop :: TChan ClientMessage -> TVar Portfolio -> TVar [Trade] -> TVar (Map StockId Stock) -> IO ()
controllerLoop orderChan pVar tradeLog tvStocks = forever $ do
  msg <- atomically $ readTChan orderChan
  snap <- readTVarIO tvStocks
  now <- getCurrentTime
  
  res <- atomically $ applyOrderSTM pVar snap msg now
  
  case res of
    Left err -> putStrLn $ "Order failed: " ++ err
    Right trade -> do
      putStrLn $ "Executed trade: " ++ show trade
      atomically $ modifyTVar' tradeLog (take 50 . (trade:))