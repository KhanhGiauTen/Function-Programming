{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-} -- ĐÃ THÊM
module Main where

import Types
import Controller
import Control.Monad (forever, void)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map -- ĐÃ SỬA: Thêm import Map
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import Data.Aeson (encode, decode)
import Control.Exception (finally)
import Network.HTTP.Types.Status (status400) -- ĐÃ SỬA: Thêm import status400

main :: IO ()
main = do
  putStrLn "Starting stock simulator server on ws://127.0.0.1:9160"

  -- 1. Khởi tạo State và Channels
  stocksList <- initialStocks
  let stockMap = Map.fromList $ map (\s -> (sId s, s)) stocksList
  
  tvStocks <- newTVarIO stockMap
  pVar <- newTVarIO initialPortfolio
  tradeLog <- newTVarIO []
  
  orderChan <- newTChanIO :: IO (TChan ClientMessage)
  priceChan <- newTChanIO :: IO (TChan PriceEvent)
  
  -- 2. Khởi chạy các Threads "Backend"
  mapM_ (void . forkIO . priceGenerator priceChan) stocksList
  void $ forkIO $ controllerLoop orderChan pVar tradeLog tvStocks
  
  -- 3. Khởi chạy Máy chủ WebSocket
  Warp.run 9160 $ WaiWS.websocketsOr WS.defaultConnectionOptions
    (webSocketApp priceChan orderChan pVar tradeLog tvStocks)
    httpApp

-- Xử lý các kết nối WebSocket
-- ĐÃ SỬA: Sửa 'Map' thành 'Map.Map'
webSocketApp :: TChan PriceEvent -> TChan ClientMessage -> TVar Portfolio -> TVar [Trade] -> TVar (Map.Map StockId Stock) -> WS.ServerApp
webSocketApp priceChan orderChan pVar tradeLog tvStocks pending = do
  conn <- WS.acceptRequest pending
  
  priceChanDup <- atomically $ dupTChan priceChan
  
  putStrLn "Client connected"

  -- Gửi trạng thái ban đầu cho client
  sendInitialState conn
  
  let sendUpdates = forever $ do
        priceEv <- atomically $ readTChan priceChanDup
        atomically $ modifyTVar' tvStocks $ Map.adjust (\s -> s { sPrice = pePrice priceEv }) (peStock priceEv)
        WS.sendTextData conn $ encode (PriceUpdate priceEv)
        
        p <- readTVarIO pVar
        t <- readTVarIO tradeLog
        WS.sendTextData conn $ encode (PortfolioUpdate p)
        WS.sendTextData conn $ encode (TradeLogUpdate t)
        
  let receiveOrders = forever $ do
        msg <- WS.receiveData conn
        case decode msg of
          Just (orderMsg :: ClientMessage) -> do -- Đã thêm kiểu tường minh
            putStrLn $ "Received order: " ++ show orderMsg
            atomically $ writeTChan orderChan orderMsg
          Nothing -> putStrLn "Received invalid message"

  (void $ forkIO sendUpdates) `finally` (receiveOrders `finally` putStrLn "Client disconnected")
  
  where
    sendInitialState conn = do
      s <- readTVarIO tvStocks
      p <- readTVarIO pVar
      t <- readTVarIO tradeLog
      WS.sendTextData conn $ encode (FullStockUpdate s)
      WS.sendTextData conn $ encode (PortfolioUpdate p)
      WS.sendTextData conn $ encode (TradeLogUpdate t)

-- Ứng dụng HTTP dự phòng
httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS
    status400 -- ĐÃ SỬA: Bỏ 'Wai.'
    [("Content-Type", "text/plain")]
    "This is a WebSocket server. Please connect using a WebSocket client."