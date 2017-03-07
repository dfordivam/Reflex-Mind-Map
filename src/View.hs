{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module View where

import Reflex
import Reflex.Dom

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Control.Monad.Fix

import Data

renderMindMap :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
        =>
     NodeList t -- Original list
  -> Event t (Map NodeID (Maybe (Node t)))
  -> m (Event t NodeEvent)
renderMindMap nl mapDiffEv = do

  -- evMapDyn :: Dynamic t (Map NodeID (Event t NodeEvent))
  -- Display the given map of items using the builder function provided, and
  -- update it with the given event. Nothing entries will delete the
  -- corresponding children, and Just entries will create or replace them.
  -- Since child events do not take any signal arguments,
  -- they are always rebuilt.
  -- To update a child without rebuilding, either embed signals in the map's
  -- values, or refer to them directly in the builder function.
  -- Therefore always use diffMapNoEq and put Dynamic in values
  evMapDyn <- listHoldWithKey nl mapDiffEv renderNode

  let
      ev = switchPromptlyDyn $
        ffor evMapDyn (\m -> leftmost (map snd $ Map.toList m))
  return ev

renderNode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
        =>
     NodeID
  -> Node t
  -> m (Event t NodeEvent)

-----------------------------------------------------------------
-----------------------------------------------------------------
renderNode i n = do
  let
      attr = (<>) <$> ((\c -> ("class" :: Text) =: c) <$> cl1 ) <*>
                  ((\x -> ("style" =: x)) <$> style)

      cl1 = ffor (nodeState n)
            (\case
              NodeSelected -> "mindmap-node-selected" :: Text
              NodeEditing -> "mindmap-node-editing"
              _ -> "mindmap-node-unselected")


      style = ffor (nodePosition n)
            (\case
              Nothing -> ("display: none;" :: Text)
              (Just pos) -> ("position: absolute;" <>
                              "left: " <> showT (fst pos) <>
                              "px; top: " <> showT (snd pos) <> "px;"))

  elDynAttr "div" attr $ do
    ev1 <- renderNodeText i n
    ev2 <- renderNodeEditWidget i n
    --let ev2 = never
    return $ leftmost [ev1, ev2]

renderNodeText i n = do
  let
      attr = ((\x -> ("style" =: x)) <$> style)

      style = ffor (nodeState n)
            (\case
              NodeSelected -> "" :: Text
              NodeEditing -> "display: none;"
              _ -> "")

    -- Node Text and click event
  (te,_) <- elDynAttr' "span" attr $ do
      dynText $ nodeContent n

  let
      selectEvent = domEvent Click te
      --dblClick = domEvent Dblclick t

      ev1 = fmap (const i) selectEvent
      --ev2 = fmap (const $ nodeId node) dblClick

      ev1' = traceEvent "NodeClick" $ fmap (\i -> SelectNodeEvent i) ev1
      --ev2' = fmap (\i -> NodeDoubleClicked i) ev2
  return ev1'

renderNodeEditWidget i n = do
  let
      attr = ((\x -> ("style"  =: x)) <$> style)

      style = ffor (nodeState n)
            (\case
              NodeEditing -> ""
              _ -> "display: none;")

  -- Create the textbox; it will be cleared whenever the user presses enter
  rec let newValueEntered = ffilter (keyCodeIs Enter)
            (_textInput_keypress descriptionBox)
          newValue = tag (current $ _textInput_value descriptionBox)
                    newValueEntered
      nodeText <- sample.current $ nodeContent n
      descriptionBox <- textInput $ def
        & textInputConfig_initialValue .~ nodeText
        -- & textInputConfig_setValue .~ newValue
        & textInputConfig_attributes .~
          (attr <>
        constDyn (mconcat [ "class" =: "edit-node"
            , "placeholder" =: "Node"
            , "autofocus" =: ""
        ]))


  -- Request focus on this element when the widget is done being built
    -- schedulePostBuild $ liftIO $ focus $ _textInput_element descriptionBox
  return $ fmap (\d -> EditFinishNodeEvent i d) newValue
-----------------------------------------------------------------
-----------------------------------------------------------------
renderControlPanel :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
        =>
     ()
  -> m (Event t ControlPanelEvent)
renderControlPanel _ = do
  el "div" $ el "table" $ el "tr" $ do
    (insertButton,_) <- el' "th" $
      text "Insert"

    (editButton,_) <- el' "th" $
      text "Edit"

    (openButton,_) <- el' "th" $
      text "Open"

    let
      editEv = fmap (const Data.Edit) $
                  domEvent Click editButton

      insertEv = fmap (const Data.InsertChild) $
                  domEvent Click insertButton

      openEv = fmap (const Data.OpenToggle) $
                  domEvent Click openButton

    return $ leftmost [insertEv, editEv, openEv]

showT :: (Show a) => a -> T.Text
showT = T.pack.show

keyCodeIs :: Key -> KeyCode -> Bool
keyCodeIs k c = keyCodeLookup c == k
