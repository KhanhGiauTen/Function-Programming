{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

type StockId = String
type Price = Double

data Stock = Stock
  { sId :: StockId
  , sPrice :: Price
  , sVolatility :: Double
  , sTrend :: Double
  } deriving (Show, Generic)
instance ToJSON Stock

data PriceEvent = PriceEvent
  { peStock :: StockId
  , pePrice :: Price
  , peTime  :: UTCTime
  } deriving (Show, Generic)
instance ToJSON PriceEvent

data OrderType = Buy | Sell deriving (Eq, Show, Generic)
instance ToJSON OrderType
instance FromJSON OrderType

-- Thông điệp UI gửi đến server
data ClientMessage
  = PlaceOrder { oStock :: StockId, oType :: OrderType, oQty :: Int }
  deriving (Show, Generic)
instance FromJSON ClientMessage

data Trade = Trade
  { tStock :: StockId
  , tQty   :: Int
  , tPrice :: Price
  , tTime  :: UTCTime
  , tType  :: OrderType
  } deriving (Show, Generic)
instance ToJSON Trade

-- --- BẮT ĐẦU THAY ĐỔI ---

-- Dữ liệu mới để lưu trữ trong portfolio
data HoldingData = HoldingData
  { hQty :: Int     -- Số lượng
  , hAvgCost :: Price -- Giá mua trung bình
  } deriving (Show, Generic)
instance ToJSON HoldingData
instance FromJSON HoldingData -- Cần cho 'decode'

data Portfolio = Portfolio
  { cash :: Double
  , holdings :: Map StockId HoldingData -- Đã thay đổi từ 'Int' sang 'HoldingData'
  } deriving (Show, Generic)
instance ToJSON Portfolio

-- --- KẾT THÚC THAY ĐỔI ---

-- Thông điệp Server gửi đến UI
data ServerMessage
  = PriceUpdate PriceEvent
  | PortfolioUpdate Portfolio
  | TradeLogUpdate [Trade]
  | FullStockUpdate (Map StockId Stock)
  deriving (Show, Generic)
instance ToJSON ServerMessage