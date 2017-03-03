{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Controller where

import Reflex
import Reflex.Dom

import Control.Monad
import Control.Monad.Fix
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

    let 
      origNodeList = (nodeID n1 =: n1)
      mindMapInit = MindMap (800,600) origNodeList (nodeID n1 =: [])
      initState = AppState (nodeID n1) (Map.empty)
                    mindMapInit (nodeID n1 =: Just n1)

    ctrlEv <- renderControlPanel ()


    -- Create the view
    nodeEvent <- renderMindMap origNodeList mapDiffEv

    dynState <- foldDynMaybe mainEventHandler initState allEvents

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
                
      selNodeDyn = fmap selectedNode dynState
      
      mapDiffEv = tagPromptlyDyn 
        (fmap nodeListDiff dynState) reRenderEvent

      reRenderEvent = fmapMaybe f allEvents
        where
         f (Left Data.InsertChild) = Just ()
         f (Left Data.Delete) = Just ()
         f (Left Data.Cut) = Just ()
         f (Left Data.Paste) = Just ()
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
     AllEvents
  -> AppState t
  -> Maybe (AppState t)
mainEventHandler ev st =
  case ev of
    Right (SelectNodeEvent n) ->
      if (n == selNode)
        then Nothing
        else Just (st {selectedNode = n})

  where
    selNode = selectedNode st
    -- Diff the map when Add, delete, cut, paste event
    -- mapDiffEv :: Event t (Map NodeID (Maybe Node))
    mapDiffEv = undefined -- diffMapNoEq m1 m2

createNode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
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
