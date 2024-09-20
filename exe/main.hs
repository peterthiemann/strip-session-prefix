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
import Data.Void
import Graphics.Vty qualified as V
import Lens.Micro.Platform
import Text.Megaparsec qualified as M

type Event = Void

newtype SectionName = SectionName Word
  deriving newtype (Eq, Ord, Show)

nextSectionName :: SectionName -> SectionName
nextSectionName (SectionName i) = SectionName (i + 1)

data SectionLabel
  = MainSession
  | PrefixSection !SectionName
  deriving stock (Eq, Ord, Show)

data Name
  = Editor !SectionLabel
  | ErrorsVP !SectionLabel
  | KeyTable
  deriving stock (Eq, Ord, Show)

newtype SessionError = SessionError String
  deriving newtype (Eq)

data StripResult
  = ParseError SessionError
  | StripError !Error
  | StripSuccess !(Session Z END)

data Section = Section
  { _secName :: !SectionName,
    _secEditor :: Editor T.Text Name,
    _secResult :: !(Maybe StripResult)
  }

data St = St
  { _stSessionEditor :: !(Editor T.Text Name),
    _stSessionError :: !(Maybe SessionError),
    _stSections :: [Section],
    _stNextSectionName :: !SectionName,
    _stFocus :: !(FocusRing Name)
  }

makeLenses ''St
makeLenses ''Section

findSection :: SectionName -> Traversal' St Section
findSection name = stSections . traverse . filtered ((name ==) . view secName)

sectionEditor :: SectionLabel -> Traversal' St (Editor T.Text Name)
sectionEditor = \case
  MainSession -> stSessionEditor
  PrefixSection i -> findSection i . secEditor

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
  render . padBottom Max . foldr1 (<==>) $
    padBottom (Pad 1) drawMainSection
      : fmap drawPrefixSection (st ^. stSections)
  where
    drawEditor :: T.Text -> Editor T.Text Name -> Widget Name
    drawEditor name ed = withFocusRing (st ^. stFocus) (editorBorder (txt name)) ed do
      withFocusRing (st ^. stFocus) (renderEditor (txt . T.unlines)) ed
    editorBorder label hasFocus _
      | hasFocus =
          withBorderStyle Border.unicodeBold
            . withAttr focusedBorderAttr
            . borderWithLabel label
      | otherwise = borderWithLabel label

    drawMainSection :: Widget Name
    drawMainSection =
      vBox $
        drawEditor "Session" (st ^. stSessionEditor)
          : [wrapErrorsSection MainSession (str e) | SessionError e <- maybeToList (st ^. stSessionError)]

    drawPrefixSection :: Section -> Widget Name
    drawPrefixSection sec =
      drawEditor "Prefix" (sec ^. secEditor)
        <=> drawResultSection (PrefixSection (sec ^. secName)) (sec ^. secResult)

    drawResultSection :: SectionLabel -> Maybe StripResult -> Widget Name
    drawResultSection secLabel = \case
      Nothing -> wrapErrorsSection secLabel (str "Errors in main session type.")
      Just (StripSuccess r) -> borderWithLabel (txt "Result") do
        padRight Max . withAttr sessionAttr . str $ show r
      Just (ParseError (SessionError e)) -> wrapErrorsSection secLabel $ padBottom (Pad 1) (str e)
      Just (StripError ErrorNotAPrefix) -> wrapErrorsSection secLabel $ str "Invalid prefix."
      Just (StripError ErrorNoCont) -> wrapErrorsSection secLabel $ str "No continuation."
      Just (StripError (ErrorContConflict s1 s2)) ->
        wrapErrorsSection secLabel . vBox $
          [ withAttr errorHeadingAttr $ str "Conflicting continuations:",
            padLeft (Pad 4) . str $ show s1,
            padLeft (Pad 4) . str $ show s2
          ]
    wrapErrorsSection secLabel =
      vLimit 5
        . withVScrollBars OnRight
        . viewport (ErrorsVP secLabel) Vertical
        . padLeftRight 1
        . withAttr errorAttr

draw :: St -> [Widget Name]
draw st = [mainUI st <=> cached KeyTable drawKeysTable]
  where
    drawKeysTable =
      keyTable
        [ [("TAB", "change focus"), ("C-u", "scroll error messages 🠅")],
          [("C-c", "quit"), ("C-d", "scroll error messages 🠇")],
          [("C-n", "new prefix"), ("C-x", "close current prefix")]
        ]

handleEvent :: BrickEvent Name Event -> EventM Name St ()
handleEvent = \case
  VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
  VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) -> scrollErrors Down
  VtyEvent (V.EvKey (V.KChar 'u') [V.MCtrl]) -> scrollErrors Up
  VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl]) -> modify \st -> reparse (PrefixSection (st ^. stNextSectionName)) $ pushEditor st
  VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl]) -> modify dropCurrentEditor
  VtyEvent (V.EvKey (V.KChar '\t') []) -> stFocus %= focusNext
  VtyEvent (V.EvKey (V.KChar '\t') [V.MShift]) -> stFocus %= focusPrev
  VtyEvent (V.EvKey (V.KChar '?') []) ->
    use stFocus >>= \fr -> suspendAndResume' do
      print fr
      void getChar
  VtyEvent V.EvResize {} -> invalidateCache
  ev -> do
    fr <- use stFocus
    case focusGetCurrent fr of
      Just (Editor secLabel) -> do
        zoom (sectionEditor secLabel) $ handleEditorEvent ev
        modify $ reparse secLabel
      _ -> pure ()

scrollErrors :: Direction -> EventM Name St ()
scrollErrors dir = do
  focus <- use stFocus
  case focusGetCurrent focus of
    Just (Editor secLabel) -> vScrollPage (viewportScroll (ErrorsVP secLabel)) dir
    _ -> pure ()

pushEditor :: St -> St
pushEditor st =
  st
    & stFocus .~ newFocusRing
    & stSections %~ (++ [newSection])
    & stNextSectionName %~ nextSectionName
  where
    newName = st ^. stNextSectionName
    initialPrefix =
      ".." `fromMaybe` do
        Editor (PrefixSection i) <- focusGetCurrent (st ^. stFocus)
        section <- st ^? findSection i
        pure $ T.unlines $ getEditContents $ section ^. secEditor
    newFocusRing =
      focusRing . fmap Editor $
        PrefixSection newName
          : MainSession
          : fmap (view (secName . to PrefixSection)) (st ^. stSections)
    newSection =
      Section
        { _secName = st ^. stNextSectionName,
          _secEditor = editorText (Editor (PrefixSection newName)) (Just 2) initialPrefix,
          _secResult = Nothing -- dummy value replaced in the next `reparse` call
        }

dropCurrentEditor :: St -> St
dropCurrentEditor st
  | _ : _ : _ <- st ^. stSections,
    Just focus@(Editor (PrefixSection i)) <- focusGetCurrent (st ^. stFocus) =
      st
        & stFocus %~ avoidMainSession . focusRing . filter (/= focus) . focusRingToList
        & stSections %~ filter ((/= i) . view secName)
  | otherwise = st
  where
    avoidMainSession focus
      | focusGetCurrent focus == Just (Editor MainSession) = focusPrev focus
      | otherwise = focus

reparse :: SectionLabel -> St -> St
reparse section st =
  st
    & stSessionError .~ do Left e <- pure parsedSession; pure e
    & case section of
      MainSession -> stSections . each %~ \sec -> sec & secResult .~ reparseSection sec
      PrefixSection n -> findSection n %~ \sec -> sec & secResult .~ reparseSection sec
  where
    reparseSection :: Section -> Maybe StripResult
    reparseSection sec = do
      Right session <- Just parsedSession
      let parsed = first ParseError (checkParse $ parse "" (sec ^. secEditor))
      let stripped = parsed >>= first StripError . stripPrefix session
      pure $ either id StripSuccess stripped

    parsedSession :: Either SessionError (Session Z END)
    parsedSession = checkParse $ parse "session" (st ^. stSessionEditor)

    checkParse :: Either ParseError (Session Z a) -> Either SessionError (Session Z a)
    checkParse = \case
      Right s | contractive s -> Right s
      Right _ -> Left . SessionError $ "Session type is not contractive."
      Left pe -> Left . SessionError $ M.errorBundlePretty pe

    parse :: forall a. (ParseEnd a) => String -> Editor T.Text Name -> Either ParseError (Session Z a)
    parse name = parseSession name . T.intercalate "\n" . getEditContents

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
    initialState =
      reparse MainSession . pushEditor $
        St
          { _stSessionEditor = editorText (Editor MainSession) (Just 2) "!Int . !Int . ?Int . end",
            _stSessionError = Nothing,
            _stSections = [],
            _stFocus = focusRing [],
            _stNextSectionName = SectionName 0
          }
