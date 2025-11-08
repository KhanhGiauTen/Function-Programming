module UI
  ( UiState(..)
  , app
  ) where

import Types
import Helpers
import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core ((<=>))
import Brick.Widgets.Core (withAttr)
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Control.Monad (when)
import Data.Time.Clock (getCurrentTime)
-- Trạng thái của UI
data UiState = UiState
  { uiStocks    :: Map StockId Stock -- Bản snapshot giá mới nhất
  , uiSelected  :: Int               -- Cổ phiếu đang chọn
  , uiPortfolio :: Portfolio         -- Snapshot danh mục
  , uiTrades    :: [Trade]           -- Snapshot lịch sử giao dịch
  , uiSort      :: SortMode          -- Sorting mode
  }

-- Widget Name (không dùng nhưng cần cho Brick)
type Name = ()

-- Brick App
app :: TChan Order -> TVar Portfolio -> TVar [Trade] -> App UiState AppEvent Name
app orderChan pVar tradeLog = App
  { appDraw = drawUiState
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent orderChan pVar tradeLog
  , appStartEvent = return
  , appAttrMap = const attrMapDefault
  }

-- Vẽ UI
drawUiState :: UiState -> [Widget Name]
drawUiState s =
  let ids = sortStockIds (uiSort s) (uiStocks s)
      indexed = zip [0..] ids
      rows = map (renderRow (uiStocks s) (uiSelected s)) indexed
      
      help = str "Keys: Up/Down - move, b - buy, s - sell, o - cycle sort, q - quit"
      header = borderWithLabel (str "Market (press 'o' to change sort)") help

      stocksBox = borderWithLabel (str "Market") $ vBox rows
      
      p = uiPortfolio s
      portBox = borderWithLabel (str "Portfolio")
        $ padAll 1
        $ vBox [ str $ "Cash: " ++ printf "%.2f" (cash p)
               , str "Holdings:"
               , vBox $ map (\(k,v) -> str $ k ++ " : " ++ show v) (Map.toList (holdings p))
               ]
               
      logBox = borderWithLabel (str "Trades (recent)") $
               vLimit 10 $
               vBox (map (str . simpleShowTrade) (uiTrades s))
               
  in [ header, center $ stocksBox <+> (portBox <=> logBox) ]
  where
    renderRow mp sel (i, sid) =
      let s' = Map.lookup sid mp
          pstr = maybe "N/A" (printf "%.2f" . sPrice) s'
          markerW = if i == sel then withAttr "selected" (str "> ") else str "  "
      in markerW <+> str (sid ++ ": ") <+> str pstr
      
    simpleShowTrade t =
      printf "[%s] %s %d @ %.2f" (show (tType t)) (tStock t) (tQty t) (tPrice t)

-- Xử lý sự kiện
handleEvent :: TChan Order -> TVar Portfolio -> TVar [Trade] -> UiState -> BrickEvent Name AppEvent -> EventM Name (Next UiState)
handleEvent orderChan pVar tradeLog s ev =
  case ev of
    -- Sự kiện giá mới từ dispatcher
    AppEvent (EPrice pe) -> do
      let mp' = Map.adjust (\st -> st { sPrice = pePrice pe }) (peStock pe) (uiStocks s)
      continue s { uiStocks = mp' }

    -- Sự kiện Tick (dùng để refresh Portfolio và Trades)
    AppEvent ETick -> do
      p <- liftIO $ readTVarIO pVar
      t <- liftIO $ readTVarIO tradeLog
      continue s { uiPortfolio = p, uiTrades = t }

    -- Phím bấm
    VtyEvent (V.EvKey V.KUp []) ->
      continue (s { uiSelected = max 0 (uiSelected s - 1) })
    VtyEvent (V.EvKey V.KDown []) ->
      continue (s { uiSelected = min (Map.size (uiStocks s) - 1) (uiSelected s + 1) })
    VtyEvent (V.EvKey (V.KChar 'q') []) ->
      halt s
    
    -- Gửi lệnh Mua
    VtyEvent (V.EvKey (V.KChar 'b') []) -> do
      let ids = Map.keys (uiStocks s)
      when (not (null ids) && uiSelected s < length ids) $ do
        let sid = ids !! uiSelected s
        -- Gửi một lệnh "rỗng" (giá và thời gian sẽ do Controller điền)
        liftIO $ atomically $ putTChan orderChan (Order sid Buy 1 0.0 undefined)
      continue s
      
    -- Gửi lệnh Bán
    VtyEvent (V.EvKey (V.KChar 's') []) -> do
      let ids = Map.keys (uiStocks s)
      when (not (null ids) && uiSelected s < length ids) $ do
        let sid = ids !! uiSelected s
        liftIO $ atomically $ putTChan orderChan (Order sid Sell 1 0.0 undefined)
      continue s
    -- Cycle sort mode
    VtyEvent (V.EvKey (V.KChar 'o') []) ->
      let next = case uiSort s of
                   ById -> ByPriceAsc
                   ByPriceAsc -> ByPriceDesc
                   ByPriceDesc -> ById
      in continue s { uiSort = next }
      
    _ -> continue s