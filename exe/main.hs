{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import BSession.Nat
import BSession.Parse
import BSession.Prefix
import BSession.Syntax
import Brick
import Brick.Focus
import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Border.Style qualified as Border
import Brick.Widgets.Edit
import Control.Category ((>>>))
import Control.Monad
import Data.Bifunctor
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe
import Data.Text qualified as T
import Data.Void
import Graphics.Vty qualified as V
import Lens.Micro.Platform
import Text.Megaparsec qualified as M

data Two a = Two a a
  deriving stock (Eq, Functor, Foldable, Traversable)

twice :: a -> Two a
twice a = Two a a

_S :: Lens' (Two a) a
_S = lens (\(Two a _) -> a) (\(Two _ b) a -> Two a b)

_P :: Lens' (Two a) a
_P = lens (\(Two _ b) -> b) (\(Two a _) b -> Two a b)

type Selector = forall x. SimpleGetter (Two x) x

type Event = Void

data Name
  = EditorS
  | EditorP
  deriving stock (Eq, Ord, Show)

data StripResult
  = StripGood !(Session Z END)
  | StripOld !(Session Z END)
  | StripError !Error

ageStripResult :: StripResult -> StripResult
ageStripResult = \case
  StripGood s -> StripOld s
  r -> r

data St = St
  { _stEditors :: Two (Editor T.Text Name),
    _stParseErrors :: Two (Widget Name),
    _stStripResult :: !StripResult,
    _stFocus :: !(FocusRing Name)
  }

makeLenses ''St

hBorderStart, hBorderEnd :: Widget n
hBorderStart = Brick.borderElem \case
  Border.BorderStyle {Border.bsHorizontal = '━'} -> '╺'
  _ -> '╶'
hBorderEnd = Brick.borderElem \case
  Border.BorderStyle {Border.bsHorizontal = '━'} -> '╸'
  _ -> '╴'

hBorderPretty :: Widget n
hBorderPretty = hBorderStart <+> Brick.hBorder <+> hBorderEnd

borderWithLabel :: Widget n -> Widget n -> Widget n
borderWithLabel lbl =
  Brick.borderWithLabel (hBorderEnd <+> lbl <+> hBorderStart <+> Brick.hBorder)

(<++>), (<==>) :: Widget n -> Widget n -> Widget n
a <++> b = a <+> raw (V.backgroundFill 1 1) <+> b
a <==> b = a <=> raw (V.backgroundFill 1 1) <=> b

_unused :: Widget n -> Widget n -> Widget n
_unused = (<==>)

rootAttr :: AttrName
rootAttr = attrName "bsession"

keyAttr :: AttrName
keyAttr = rootAttr <> attrName "key"

sessionAttr :: AttrName
sessionAttr = rootAttr <> attrName "session"

errorAttr :: AttrName
errorAttr = rootAttr <> attrName "error"

errorHeadingAttr :: AttrName
errorHeadingAttr = errorAttr <> attrName "heading"

focusedBorderAttr :: AttrName
focusedBorderAttr = Brick.borderAttr <> attrName "focused"

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
        [ drawSection _S,
          drawSection _P,
          borderWithLabel (txt "Result") do
            drawStripResult $ st ^. stStripResult,
          padTop Max $ keyTable [("TAB", "change focus"), ("C-c", "quit")]
        ]

    drawStripResult :: StripResult -> Widget Name
    drawStripResult =
      padRight Max . \case
        StripGood r -> withAttr sessionAttr $ str $ show r
        StripOld r -> withAttr errorAttr $ str $ show r
        StripError e -> withAttr errorAttr case e of
          ErrorNotAPrefix -> withAttr errorHeadingAttr $ str "Invalid prefix."
          ErrorNoCont -> withAttr errorHeadingAttr $ str "No continuation."
          ErrorContConflict s1 s2 ->
            vBox
              [ withAttr errorHeadingAttr $ str "Conflicting continuations:",
                padLeft (Pad 4) $ str $ show s1,
                padLeft (Pad 4) $ str $ show s2
              ]

    drawSection :: Selector -> Widget Name
    drawSection sel =
      drawEditor (Two "Session" "Prefix" ^. sel) (st ^. stEditors . sel)
        <=> (st ^. stParseErrors . sel)

    drawEditor name ed = withFocusRing (st ^. stFocus) (editorBorder (txt name)) ed do
      withFocusRing (st ^. stFocus) (renderEditor (txt . T.unlines)) ed
    editorBorder label hasFocus _
      | hasFocus =
          withBorderStyle Border.unicodeBold
            . withAttr focusedBorderAttr
            . borderWithLabel label
      | otherwise = borderWithLabel label

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
    when (isJust (focusGetCurrent fr)) do
      modify reparse

reparse :: St -> St
reparse st =
  st
    & stParseErrors .~ Two errorS errorP
    & updateStripped
  where
    (!errorS, sessionS) = checkRenderParse $ parse "session" _S
    (!errorP, sessionP) = checkRenderParse $ parse "prefix" _P

    stripped = stripPrefix <$> sessionS <*> sessionP
    updateStripped = case stripped of
      Nothing -> stStripResult %~ ageStripResult
      Just (Left e) -> stStripResult .~ StripError e
      Just (Right r) -> stStripResult .~ StripGood r

    checkRenderParse :: Either ParseError (Session Z a) -> (Widget Name, Maybe (Session Z a))
    checkRenderParse =
      first (vLimit 7 . padBottom Max . padLeftRight 1 . withAttr errorAttr) . \case
        Right s | contractive s -> (fill ' ', Just s)
        Right _ -> (str "session type is not contractive", Nothing)
        Left pe -> (str (M.errorBundlePretty pe), Nothing)

    parse :: (ParseEnd a) => String -> Selector -> Either ParseError (Session Z a)
    parse name field =
      parseSession name $ T.intercalate "\n" $ getEditContents $ st ^. stEditors . field

main :: IO ()
main = void $ defaultMain app initialState
  where
    attrs =
      [ (keyAttr, fg V.cyan),
        (sessionAttr, style V.bold),
        (errorAttr, fg V.red),
        (errorHeadingAttr, style V.bold),
        (focusedBorderAttr, style V.bold)
      ]
    app =
      App
        { appDraw = draw,
          appHandleEvent = handleEvent,
          appStartEvent = pure (),
          appChooseCursor = focusRingCursor _stFocus,
          appAttrMap = const $ attrMap V.defAttr attrs
        }
    initialState = reparse do
      St
        { _stEditors =
            Two
              (editorText EditorS (Just 3) "!Int . !Int . ?Int . end")
              (editorText EditorP (Just 3) ".."),
          _stParseErrors = twice emptyWidget,
          _stStripResult = StripOld (SEnd END),
          _stFocus = focusRing [EditorS, EditorP]
        }
