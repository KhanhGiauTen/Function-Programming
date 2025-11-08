module Helpers
  ( SortMode(..)
  , sortStockIds
  , attrSelected
  , attrMapDefault
  ) where

import Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
-- ĐÃ SỬA: Import chính xác
import Brick.AttrMap (AttrMap, attrMap, attrName)
import qualified Graphics.Vty as V

data SortMode = ById | ByPriceAsc | ByPriceDesc deriving (Eq, Show)

-- Return stock ids sorted according to mode
sortStockIds :: SortMode -> Map StockId Stock -> [StockId]
sortStockIds ById mp = Map.keys mp
sortStockIds ByPriceAsc mp = map fst $ sortOn snd $ map (\t -> (fst t, sPrice (snd t))) (Map.toList mp)
sortStockIds ByPriceDesc mp = reverse (sortStockIds ByPriceAsc mp)

attrSelected :: V.Attr
attrSelected = V.withForeColor V.defAttr V.yellow

attrMapDefault :: AttrMap
-- ĐÃ SỬA: Thêm attrName
attrMapDefault = attrMap V.defAttr [(attrName "selected", attrSelected)]