{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Brick
import Brick.Focus
import Brick.Widgets.Edit
import Control.Monad
import Data.Text qualified as T
import Data.Void
import Graphics.Vty qualified as V
import Lens.Micro.Platform

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
  { _stEditors :: Two (Editor T.Text Name),
    _stFocus :: FocusRing Name
  }

makeLenses ''St

draw :: St -> [Widget Name]
draw st = [ui]
  where
    ui = vBox $ st ^.. stEditors . folded . to drawEditor
    drawEditor =
      withFocusRing (st ^. stFocus) (renderEditor (txt . T.unlines))

handleEvent :: BrickEvent Name Event -> EventM Name St ()
handleEvent = \case
  VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
  VtyEvent (V.EvKey (V.KChar '\t') []) -> stFocus %= focusNext
  VtyEvent (V.EvKey (V.KChar '\t') [V.MShift]) -> stFocus %= focusPrev
  ev -> do
    fr <- use stFocus
    case focusGetCurrent fr of
      Just EditorS -> zoom (stEditors . _S) $ handleEditorEvent ev
      Just EditorP -> zoom (stEditors . _P) $ handleEditorEvent ev
      Nothing -> pure ()

main :: IO ()
main = void do
  defaultMain
    App
      { appDraw = draw,
        appHandleEvent = handleEvent,
        appStartEvent = pure (),
        appChooseCursor = focusRingCursor _stFocus,
        appAttrMap = const $ attrMap V.defAttr []
      }
    St
      { _stEditors =
          Two
            (editorText EditorS (Just 3) "!Int . !Int . ?Int . end")
            (editorText EditorP (Just 3) ".."),
        _stFocus = focusRing [EditorS, EditorP]
      }
