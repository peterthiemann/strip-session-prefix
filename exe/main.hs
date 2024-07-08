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
import Data.Bifunctor
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe
import Data.Text qualified as T
import Data.These
import Data.Void
import Graphics.Vty qualified as V
import Lens.Micro.Platform
import Text.Megaparsec qualified as M

data Two a = Two a a
  deriving stock (Eq, Functor, Foldable, Traversable)

_twice :: a -> Two a
_twice a = Two a a

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
  | ErrorsVP
  deriving stock (Eq, Ord, Show)

newtype SessionError = SessionError String
  deriving newtype (Eq)

data StripResult
  = SessionErrors (These SessionError SessionError)
  | StripError !Error
  | StripSuccess !(Session Z END)

data St = St
  { _stEditors :: Two (Editor T.Text Name),
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

keyTable :: [[(T.Text, T.Text)]] -> Widget n
keyTable =
  mapMaybe nonEmpty >>> \case
    [] -> emptyWidget
    keyRows -> vBox $ hBorderPretty : fmap renderRow keyRows
  where
    renderRow ks = foldr1 (<++>) $ renderKey <$> ks
    renderKey (k, desc) = padLeft (Pad 1) . padRight Max $ do
      withAttr keyAttr (txt k) <++> txt desc

mainUI :: St -> Widget Name
mainUI st = Widget Greedy Greedy do
  render . padBottom Max . vBox $
    [ drawEdSection _S,
      drawEdSection _P,
      drawResultSection (st ^. stStripResult)
    ]
  where
    drawEdSection :: Selector -> Widget Name
    drawEdSection sel =
      drawEditor (Two "Session" "Prefix" ^. sel) (st ^. stEditors . sel)

    drawEditor name ed = withFocusRing (st ^. stFocus) (editorBorder (txt name)) ed do
      withFocusRing (st ^. stFocus) (renderEditor (txt . T.unlines)) ed
    editorBorder label hasFocus _
      | hasFocus =
          withBorderStyle Border.unicodeBold
            . withAttr focusedBorderAttr
            . borderWithLabel label
      | otherwise = borderWithLabel label

    drawResultSection :: StripResult -> Widget Name
    drawResultSection = \case
      StripSuccess r -> borderWithLabel (txt "Result") do
        padRight Max . withAttr sessionAttr . str $ show r
      SessionErrors es -> wrapErrorsSection $ mergeTheseWith renderError renderError (<=>) es
        where
          renderError (SessionError e) = padBottom (Pad 1) (str e)
      StripError ErrorNotAPrefix -> wrapErrorsSection $ str "Invalid prefix."
      StripError ErrorNoCont -> wrapErrorsSection $ str "No continuation."
      StripError (ErrorContConflict s1 s2) ->
        wrapErrorsSection . vBox $
          [ withAttr errorHeadingAttr $ str "Conflicting continuations:",
            padLeft (Pad 4) . str $ show s1,
            padLeft (Pad 4) . str $ show s2
          ]

    wrapErrorsSection =
      withVScrollBars OnRight
        . viewport ErrorsVP Vertical
        . padLeftRight 1
        . withAttr errorAttr

draw :: St -> [Widget Name]
draw st = [mainUI st <=> cached KeyTable drawKeysTable]
  where
    drawKeysTable =
      keyTable
        [ [("TAB", "change focus"), ("C-u", "scroll error messages 🠅")],
          [("C-c", "quit"), ("C-d", "scroll error messages 🠇")]
        ]

handleEvent :: BrickEvent Name Event -> EventM Name St ()
handleEvent = \case
  VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
  VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) -> vScrollPage (viewportScroll ErrorsVP) Down
  VtyEvent (V.EvKey (V.KChar 'u') [V.MCtrl]) -> vScrollPage (viewportScroll ErrorsVP) Up
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
reparse st = st & stStripResult .~ stripResult
  where
    names = Two "session" "prefix"

    stripResult :: StripResult
    stripResult =
      either id StripSuccess . join . first SessionErrors $
        mergeEitherWith
          (\s p -> first StripError $ stripPrefix s p)
          checkParse
          checkParse

    checkParse :: forall a. (ParseEnd a, HasSelector a) => Either SessionError (Session Z a)
    checkParse = case parse of
      Right s | contractive s -> Right s
      Right _ -> Left . SessionError $ names ^. getSelector @a ++ ": session type is not contractive"
      Left pe -> Left . SessionError $ M.errorBundlePretty pe

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
              (editorText EditorS (Just 2) "!Int . !Int . ?Int . end")
              (editorText EditorP (Just 2) ".."),
          _stStripResult = StripSuccess (SEnd END),
          _stFocus = focusRing [EditorS, EditorP]
        }

mergeEitherWith :: (a -> b -> c) -> Either e1 a -> Either e2 b -> Either (These e1 e2) c
mergeEitherWith _ (Left e1) (Left e2) = Left (These e1 e2)
mergeEitherWith _ (Left e1) _ = Left (This e1)
mergeEitherWith _ _ (Left e2) = Left (That e2)
mergeEitherWith f (Right a) (Right b) = Right (f a b)
