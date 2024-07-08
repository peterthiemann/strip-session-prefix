{-# LANGUAGE AllowAmbiguousTypes #-}
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

class HasSelector a where
  getSelector :: Selector

instance HasSelector END where
  getSelector = _S

instance HasSelector RET where
  getSelector = _P

type Event = Void

data Name
  = EditorS
  | EditorP
  | KeyTable
  deriving stock (Eq, Ord, Show)

data StripResult
  = StripGood !(Session Z END)
  | StripOld !(Session Z END)
  | StripError !Error

ageStripResult :: StripResult -> StripResult
ageStripResult = \case
  StripGood s -> StripOld s
  r -> r

newtype SessionError = SessionError {getSessionError :: String}

data St = St
  { _stEditors :: Two (Editor T.Text Name),
    _stSessionErrors :: Two (Maybe SessionError),
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

mainUI :: St -> Widget Name
mainUI st = Widget Greedy Greedy do
  render . padBottom Max . vBox . concat $
    [ [ drawEdSection _S,
        drawEdSection _P
      ],
      [drawResSection | all isNothing (st ^. stSessionErrors)],
      [drawErrorsSection]
    ]
  where
    drawResSection :: Widget Name
    drawResSection =
      borderWithLabel (txt "Result") do
        drawStripResult $ st ^. stStripResult

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

    drawEdSection :: Selector -> Widget Name
    drawEdSection sel =
      drawEditor (Two "Session" "Prefix" ^. sel) (st ^. stEditors . sel)

    drawErrorsSection :: Widget Name
    drawErrorsSection =
      withAttr errorAttr . padLeftRight 1 . vBox $
        [padBottom (Pad 1) (str err) | SessionError err <- st ^.. stSessionErrors . traversed . _Just]

    drawEditor name ed = withFocusRing (st ^. stFocus) (editorBorder (txt name)) ed do
      withFocusRing (st ^. stFocus) (renderEditor (txt . T.unlines)) ed
    editorBorder label hasFocus _
      | hasFocus =
          withBorderStyle Border.unicodeBold
            . withAttr focusedBorderAttr
            . borderWithLabel label
      | otherwise = borderWithLabel label

draw :: St -> [Widget Name]
draw st = [mainUI st <=> cached KeyTable drawKeysTable]
  where
    drawKeysTable =
      keyTable [("TAB", "change focus"), ("C-c", "quit")]

handleEvent :: BrickEvent Name Event -> EventM Name St ()
handleEvent = \case
  VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
  VtyEvent (V.EvKey (V.KChar '\t') []) -> stFocus %= focusNext
  VtyEvent (V.EvKey (V.KChar '\t') [V.MShift]) -> stFocus %= focusPrev
  VtyEvent V.EvResize {} -> invalidateCache
  ev -> do
    fr <- use stFocus
    case focusGetCurrent fr of
      Just EditorS -> zoom (stEditors . _S) $ handleEditorEvent ev
      Just EditorP -> zoom (stEditors . _P) $ handleEditorEvent ev
      _ -> pure ()
    when (isJust (focusGetCurrent fr)) do
      modify reparse

reparse :: St -> St
reparse st =
  st
    & stSessionErrors .~ Two errorS errorP
    & updateStripped
  where
    names = Two "session" "prefix"

    (errorS, sessionS) = checkRenderParse
    (errorP, sessionP) = checkRenderParse

    stripped = stripPrefix <$> sessionS <*> sessionP
    updateStripped = case stripped of
      Nothing -> stStripResult %~ ageStripResult
      Just (Left e) -> stStripResult .~ StripError e
      Just (Right r) -> stStripResult .~ StripGood r

    checkRenderParse :: forall a. (ParseEnd a, HasSelector a) => (Maybe SessionError, Maybe (Session Z a))
    checkRenderParse = case parse of
      Right s | contractive s -> (Nothing, Just s)
      Right _ -> (Just . SessionError $ names ^. getSelector @a ++ ": session type is not contractive", Nothing)
      Left pe -> (Just . SessionError $ M.errorBundlePretty pe, Nothing)

    parse :: forall a. (ParseEnd a, HasSelector a) => Either ParseError (Session Z a)
    parse =
      parseSession (names ^. getSelector @a) . T.intercalate "\n" . getEditContents $
        st ^. stEditors . getSelector @a

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
          _stSessionErrors = twice Nothing,
          _stStripResult = StripOld (SEnd END),
          _stFocus = focusRing [EditorS, EditorP]
        }
