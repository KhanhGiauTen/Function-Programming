{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Controller
import UI
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM (TVar, newTVarIO, writeTVarIO)
import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V
import Control.Monad (void, forever)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  -- 1. Khởi tạo Kênh (Channels)
  priceTChan <- newTChanIO -- Kênh giá (Generators -> Dispatcher)
  orderTChan <- newTChanIO -- Kênh lệnh (UI -> Controller)
  bchan <- newBChan 1000    -- Kênh sự kiện của Brick (Dispatcher -> UI)

  -- 2. Khởi tạo Trạng thái (State)
  stocksList <- initialStocks
  let stockMap = Map.fromList $ map (\s -> (sId s, s)) stocksList

  tvStocks <- newTVarIO stockMap    -- Snapshot giá cổ phiếu
  pVar <- newTVarIO initialPortfolio -- Danh mục đầu tư
  tradeLog <- newTVarIO []         -- Lịch sử giao dịch

  -- 3. Khởi chạy các Threads "Backend"
  
  -- Shutdown TVar to signal threads to exit cleanly
  shutdown <- newTVarIO False

  -- Chạy các price generators (mỗi cổ phiếu 1 thread)
  mapM_ (\s -> void $ forkIO (priceGenerator shutdown priceTChan s)) stocksList

  -- Chạy dispatcher (đọc từ priceTChan, cập nhật tvStocks, ghi vào bchan)
  void $ forkIO $ dispatcher priceTChan bchan tvStocks

  -- Chạy controller (đọc từ orderTChan, xử lý lệnh)
  void $ forkIO $ controllerLoop orderTChan pVar tradeLog tvStocks

  -- 4. Khởi tạo Trạng thái UI ban đầu
  initStocksSnapshot <- readTVarIO tvStocks
  initPortfolio <- readTVarIO pVar
  initTrades <- readTVarIO tradeLog
  let ui0 = UiState initStocksSnapshot 0 initPortfolio initTrades ById

  -- 5. Chạy Thread "Tick" cho UI
  -- Thread này định kỳ gửi sự kiện ETick để UI refresh portfolio và trades
  void $ forkIO $ forever $ do
    threadDelay 500000 -- 0.5 giây
    writeBChan bchan ETick

  -- 6. Chạy Brick UI
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  _finalState <- customMain initialVty buildVty (Just bchan) (app orderTChan pVar tradeLog) ui0

  -- Signal shutdown to background threads and give them a moment to exit
  writeTVarIO shutdown True
  threadDelay 200000

  putStrLn "Exiting..."