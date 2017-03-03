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
            (NodeID 0) ("Root")

    n2 <- createNode selNodeDyn openEv editEv nodePos
            (NodeID 1) ("Leaf")

    let 
      origNodeList = (nodeID n1 =: n1) <> (nodeID n2 =: n2)

      mindMapInit = MindMap (800,600) origNodeList (nodeID n1 =: [nodeID n2])
      initState = AppState (nodeID n1) (Map.empty)
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

      openEv = fmap OpenToggleEv $
        getNodeForEvent (Left OpenToggle)

      editEv = fmap (NodeEditEv "")
        (getNodeForEvent (Left Edit))

      -- getNodeForEvent ::
      --      AllEvents -- Filter this event
      --   -> Event t NodeID -- Get the current selected node
      getNodeForEvent ev =
        tagPromptlyDyn selNodeDyn
        (fmapMaybe filterEv allEvents)
          where filterEv e = if e == ev then Just () else Nothing

      nodePos = getPos
                  (uniqDyn (fmap (nodeTree.mindMap) dynState))
                  -- (uniqDyn (fmap nodeList.mindMap dynState))
                
      selNodeDyn = traceDyn "SelNode" (fmap selectedNode dynState)
      
      dynState = traceDyn "State" dynState1

      mapDiffEv = tagPromptlyDyn 
        (fmap nodeListDiff dynState) reRenderEvent

      reRenderEvent = fmapMaybe f allEvents
        where
         f (Left Data.InsertChild) = Just ()
         f (Left Data.Delete) = Just ()
         f (Left Data.Cut) = Just ()
         f (Left Data.Paste) = Just ()
         f (Right _) = Just ()
         f _ = Nothing

      
    return ()
  return ()

-- Try not to change Pos with selection events
getPos :: (Reflex t)
  =>
     Dynamic t NodeTree
  -- -> Dynamic t NodeList
  -> Dynamic t NodePos
getPos tree = constDyn Map.empty 

mainEventHandler ::
  (Monad m) =>
     (NodeID -> Text -> m (Node t))
  -> AllEvents
  -> AppState t
  -> m (Maybe (AppState t))
mainEventHandler getNewNode ev st =
  case ev of
    Right (SelectNodeEvent n) ->
      if (n == selNode)
        then return Nothing
        else return $
          Just (st {selectedNode = n, nodeListDiff = Map.empty})

    Left InsertChild -> do
      n <- getNewNode
             (NodeID 2) ("newNode")

      return $ Just (st {selectedNode = (nodeID n)
                , mindMap = (mindMap st) {
                    nodeList = nodeList (mindMap st) <>
                      (nodeID n =: n)}
                , nodeListDiff = (nodeID n =: Just n)
                      })

  where
    selNode = selectedNode st
    -- Diff the map when Add, delete, cut, paste event
    -- mapDiffEv :: Event t (Map NodeID (Maybe Node))
    mapDiffEv = undefined -- diffMapNoEq m1 m2

createNode :: (
             Reflex t
           , MonadFix m
           , MonadHold t m
           )
        => 
     Dynamic t NodeID
  -> Event t OpenToggleEv
  -> Event t NodeEditEv
  -> Dynamic t NodePos
  -> NodeID
  -> Text
  -> m (Node t)

createNode selNodeDyn openEv editEv nodePos i txt = do
  let
    s = ffor selNodeDyn ((==) i)

    f1 (OpenToggleEv n) b = if n == i then not b else b

    f2 (NodeEditEv newText n) oldText =
      if n == i then newText else oldText
    
    p = ffor nodePos (\m ->  Map.lookup i m)

  o <- foldDyn f1 True openEv
  txt' <- foldDyn f2 txt editEv
  
  return $ Node i txt' s o p
