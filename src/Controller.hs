{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Controller where

import Reflex
import Reflex.Dom

import Control.Monad
import Control.Monad.Fix
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Data
import View
import Algo

mindMapWidget :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
        =>
  m ()
mindMapWidget = do

  rec

    n1 <- createNode selNodeDyn openEv editEv nodePos
            (NodeID 0) (NodeID 0) ("Root")

    n2 <- createNode selNodeDyn openEv editEv nodePos
            (NodeID 1) (NodeID 0) ("Child")

    let
      origNodeList = (nodeID n1 =: n1) <> (nodeID n2 =: n2)

      mindMapInit = MindMap (800,600) origNodeList 
        ((nodeID n1 =: [nodeID n2]) <> (nodeID n2 =: []))

      initState = AppState (nodeID n1, False)
                    mindMapInit ((nodeID n1 =: Just n1) <> (nodeID n2 =: Just n2))

    ctrlEv <- renderControlPanel ()


    -- Create the view
    nodeEvent <- renderMindMap origNodeList mapDiffEv

    dynState1 <- foldDynMaybeM
      (mainEventHandler
      (createNode selNodeDyn openEv editEv nodePos))
      initState allEvents

    let
      allEvents = leftmost [Left <$> ctrlEv
                    , Right <$> nodeEvent]

      openEv = OpenToggleEv <$>
        getNodeForEvent (Left OpenToggle)

      editEv = (uncurry NodeEditEv) <$>
        (fmapMaybe f allEvents)
          where
            f (Right (EditFinishNodeEvent n txt)) = Just (txt,n)
            f _ = Nothing

      -- getNodeForEvent ::
      --      AllEvents -- Filter this event
      --   -> Event t NodeID -- Get the current selected node
      getNodeForEvent ev = fmap fst $
        tagPromptlyDyn selNodeDyn
        (fmapMaybe filterEv allEvents)
          where filterEv e = if e == ev then Just () else Nothing

      nodePos = getPos (fmap mindMap dynState)

      selNodeDyn = traceDyn "SelNode" (fmap selectedNode dynState)

      dynState = traceDyn "State" dynState1

      -- Restrict the rerendering to these events
      -- May be this is not required if we have an empty nodeListDiff
      mapDiffEv = tagPromptlyDyn
        (fmap nodeListDiff dynState) reRenderEvent
        where
          reRenderEvent = fmapMaybe f allEvents
            where
             f (Left Data.InsertChild) = Just ()
             f (Left Data.Delete) = Just ()
             f (Left Data.Cut) = Just ()
             f (Left Data.Paste) = Just ()
             --f (Right _) = Just ()
             f _ = Nothing


    return ()
  return ()

-- Try not to change Pos with selection events
getPos :: (Reflex t)
  =>
     Dynamic t (MindMap t)
  -> Dynamic t NodePos
getPos mm = traceDyn "Pos" $ joinDynThroughMap $ fmap getNodePos mm

mainEventHandler ::
  (Monad m) =>
     (NodeID -> NodeID -> Text -> m (Node t))
  -> AllEvents
  -> AppState t
  -> m (Maybe (AppState t))
mainEventHandler getNewNode ev st =
  case ev of
    Right (SelectNodeEvent n) ->
      if (n == selNode)
        then return Nothing
        else return $
          Just (st {selectedNode = (n, False)
            , nodeListDiff = Map.empty})

    Right (EditFinishNodeEvent n txt) -> return $
      Just $ st { selectedNode = (n, False)
        , nodeListDiff = Map.empty}

    Left Edit -> return $
      Just $ st { selectedNode = (selNode, True)
        , nodeListDiff = Map.empty}

    -- This will recompute the positions
    Left OpenToggle -> return Nothing


    Left InsertChild -> do
      let mm = mindMap st
          newID = NodeID $ 1 +
            (unNodeID $ fst $ head $ Map.toDescList (nodeList mm))

      n <- getNewNode
             newID selNode ("")

      let newList = nodeList mm <>
                      (nodeID n =: n)
          t1 = Map.update (\l -> Just (newID:l)) selNode (nodeTree mm)
          newTree = Map.insert newID [] t1

      return $
        Just $ st {
            selectedNode = (newID, True)
          , mindMap = mm { nodeList = newList,
              nodeTree = newTree}
          , nodeListDiff = (nodeID n =: Just n) <>
                (selNode =: Map.lookup selNode newList)
          }

  where
    selNode = fst (selectedNode st)

createNode :: (
             Reflex t
           , MonadFix m
           , MonadHold t m
           )
        =>
     Dynamic t (NodeID, Bool)
  -> Event t OpenToggleEv
  -> Event t NodeEditEv
  -> Dynamic t NodePos
  -> NodeID
  -> NodeID
  -> Text
  -> m (Node t)

createNode selNodeDyn openEv editEv nodePos i p txt = do
  let
    s = ffor selNodeDyn
          (\(n, e) ->
            if (n == i)
              then if e then NodeEditing else NodeSelected
              else NodeUnselected)

    f1 (OpenToggleEv n) b = if n == i then not b else b

    f2 (NodeEditEv newText n) oldText =
      if n == i then newText else oldText

    pos = ffor nodePos (\m ->  join $ Map.lookup i m)

  o <- foldDyn f1 True openEv
  txt' <- foldDyn f2 txt editEv

  return $ Node i p txt' s o pos
