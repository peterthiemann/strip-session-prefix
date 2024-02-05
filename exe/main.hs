{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

import BSession.Parse
import BSession.Prefix
import Brick
import Brick.Focus
import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Edit
import Control.Category ((>>>))
import Control.Monad
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe
import Data.Text qualified as T
import Data.These
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

hBorderStart, hBorderEnd :: Widget n
hBorderStart = str "╶"
hBorderEnd = str "╴"

hBorderPretty :: Widget n
hBorderPretty = hBorderStart <+> Brick.hBorder <+> hBorderEnd

borderWithLabel :: Widget n -> Widget n -> Widget n
borderWithLabel lbl =
  Brick.borderWithLabel (hBorderEnd <+> lbl <+> hBorderStart <+> Brick.hBorder)

(<++>), (<==>) :: Widget n -> Widget n -> Widget n
a <++> b = a <+> raw (V.backgroundFill 1 1) <+> b
a <==> b = a <=> raw (V.backgroundFill 1 1) <=> b

rootAttr :: AttrName
rootAttr = attrName "bsession"

keyAttr :: AttrName
keyAttr = rootAttr <> attrName "key"

sessionAttr :: AttrName
sessionAttr = rootAttr <> attrName "session"

errorAttr :: AttrName
errorAttr = rootAttr <> attrName "error"

keyTable :: [(T.Text, T.Text)] -> Widget n
keyTable =
  nonEmpty >>> \case
    Nothing -> emptyWidget
    Just ks ->
      vBox
        [ hBorderPretty,
          foldr1 (<++>) $ renderKey <$> ks,
          hBorderPretty
        ]
  where
    renderKey (k, desc) = padLeft (Pad 1) . padRight Max $ do
      withAttr keyAttr (txt k) <++> txt desc

draw :: St -> [Widget Name]
draw st = [ui]
  where
    ui =
      vBox
        [ drawEditor "Session" (st ^. stEditorS),
          drawEditor "Prefix" (st ^. stEditorP),
          st ^. stResult,
          padTop Max $ keyTable [("TAB", "change focus"), ("C-c", "quit")]
        ]
    drawEditor name ed = borderWithLabel (txt name) do
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
    parsed =
      mergeErrors
        (parse "session" stEditorS)
        (parse "prefix" stEditorP)
    widget = case parsed of
      Left e -> padRight Max . padLeftRight 1 $ do
        mergeTheseWith parseErrorWidget parseErrorWidget (<==>) e
      Right (s, p) -> case stripPrefix s p of
        Left e -> prefixErrorWidget e
        Right r -> borderWithLabel (txt "Result") . padRight Max $ do
          withAttr sessionAttr $ str $ show r
    parse name field =
      parseSession name $ T.intercalate "\n" $ getEditContents $ st ^. field
    prefixErrorWidget =
      padLeftRight 1 . withAttr errorAttr . str . show
    parseErrorWidget =
      withAttr errorAttr . str . M.errorBundlePretty

mergeErrors :: Either e1 a -> Either e2 b -> Either (These e1 e2) (a, b)
mergeErrors (Left e1) (Left e2) = Left (These e1 e2)
mergeErrors (Left e) _ = Left (This e)
mergeErrors _ (Left e) = Left (That e)
mergeErrors (Right a) (Right b) = Right (a, b)

main :: IO ()
main = void do
  let attrs =
        [ (keyAttr, fg V.cyan),
          (sessionAttr, style V.bold),
          (errorAttr, fg V.red)
        ]
  let app =
        App
          { appDraw = draw,
            appHandleEvent = handleEvent,
            appStartEvent = pure (),
            appChooseCursor = focusRingCursor _stFocus,
            appAttrMap = const $ attrMap V.defAttr attrs
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
