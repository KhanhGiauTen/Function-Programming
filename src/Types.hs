module Types where

import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime)

type StockId = String
type Price = Double

data Stock = Stock
  { sId :: StockId
  , sPrice :: Price
  , sVolatility :: Double
  , sTrend :: Double
  } deriving (Show)

data PriceEvent = PriceEvent
  { peStock :: StockId
  , pePrice :: Price
  , peTime  :: UTCTime
  } deriving (Show)

data OrderType = Buy | Sell deriving (Eq, Show)

data Order = Order
  { oStock :: StockId
  , oType  :: OrderType
  , oQty   :: Int
  , oPrice :: Price
  , oTime  :: UTCTime
  } deriving (Show)

data Trade = Trade
  { tStock :: StockId
  , tQty   :: Int
  , tPrice :: Price
  , tTime  :: UTCTime
  , tType  :: OrderType
  } deriving (Show)

data Portfolio = Portfolio
  { cash :: Double
  , holdings :: Map StockId Int
  } deriving (Show)

-- Brick custom events (arriving into the UI)
data AppEvent = EPrice PriceEvent | ETick deriving (Show)
