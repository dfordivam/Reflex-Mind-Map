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
  
main :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
        => 
  m ()
main = do

  let origNodeList = undefined
      initState = undefined

  rec

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

    n1 <- createNode selNodeDyn openEv editEv nodePos
            (NodeID 0) ("Root")
    return ()
  return ()

-- Try not to change Pos with selection events
getPos ::
     Dynamic t NodeTree
  -- -> Dynamic t NodeList
  -> Dynamic t NodePos
getPos = undefined

mainEventHandler ::
     AllEvents
  -> AppState t
  -> Maybe (AppState t)
mainEventHandler = undefined
  where

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
