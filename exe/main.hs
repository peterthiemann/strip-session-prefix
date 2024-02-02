{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import BSession.Parse
import BSession.Prefix
import Brick
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import Data.Void
import Graphics.Vty qualified as V
import Lens.Micro.Platform
import Text.Megaparsec qualified as M

data Two a = Two a a
  deriving stock (Eq)
  deriving stock (Functor, Foldable, Traversable)

_S :: Lens' (Two a) a
_S = lens (\(Two a _) -> a) (\(Two _ b) a -> Two a b)

_P :: Lens' (Two a) a
_P = lens (\(Two _ b) -> b) (\(Two a _) b -> Two a b)

type Event = Void

data Name
  = EditorS
  | EditorP
  deriving stock (Eq, Ord, Show)

data St = St
  { _stEditorS :: Editor T.Text Name,
    _stEditorP :: Editor T.Text Name,
    _stFocus :: FocusRing Name,
    _stResult :: Widget Name
  }

makeLenses ''St

draw :: St -> [Widget Name]
draw st = [ui]
  where
    ui =
      vBox
        [ drawEditor "session" (st ^. stEditorS),
          drawEditor "prefix" (st ^. stEditorP),
          st ^. stResult
        ]
    drawEditor name ed =
      borderWithLabel (str "╴" <+> txt name <+> str "╶" <+> hBorder) $
        withFocusRing (st ^. stFocus) (renderEditor (txt . T.unlines)) ed

handleEvent :: BrickEvent Name Event -> EventM Name St ()
handleEvent = \case
  VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
  VtyEvent (V.EvKey (V.KChar '\t') []) -> stFocus %= focusNext
  VtyEvent (V.EvKey (V.KChar '\t') [V.MShift]) -> stFocus %= focusPrev
  ev -> do
    fr <- use stFocus
    case focusGetCurrent fr of
      Just EditorS -> zoom stEditorS $ handleEditorEvent ev
      Just EditorP -> zoom stEditorP $ handleEditorEvent ev
      Nothing -> pure ()
    when (isJust (focusGetCurrent fr)) do
      modify reparse

reparse :: St -> St
reparse st = st & stResult .~ widget
  where
    parsedS = parseSession "session" $ T.intercalate "\n" $ getEditContents $ st ^. stEditorS
    parsedP = parseSession "prefix" $ T.intercalate "\n" $ getEditContents $ st ^. stEditorP
    widget = case (parsedS, parsedP) of
      (Left e1, Left e2) -> parseErrorWidget e1 <=> parseErrorWidget e2
      (_, Left e) -> parseErrorWidget e
      (Left e, _) -> parseErrorWidget e
      (Right s, Right p) -> case stripPrefix s p of
        Left e -> str $ show e
        Right r -> str $ show r
    parseErrorWidget = padBottom (Pad 1) . str . M.errorBundlePretty

main :: IO ()
main = void do
  let app =
        App
          { appDraw = draw,
            appHandleEvent = handleEvent,
            appStartEvent = pure (),
            appChooseCursor = focusRingCursor _stFocus,
            appAttrMap = const $ attrMap V.defAttr []
          }
  let initialState =
        reparse
          St
            { _stEditorS = editorText EditorS (Just 3) "!Int . !Int . ?Int . end",
              _stEditorP = editorText EditorP (Just 3) "..",
              _stFocus = focusRing [EditorS, EditorP],
              _stResult = emptyWidget
            }
  defaultMain app initialState
