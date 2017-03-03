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
  -- they are always rebuilt. To
  -- update a child without rebuilding, either embed signals in the map's
  -- values, or refer to them directly in the builder function.
  --
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

renderNode i n = do
  let 
      attr = (<>) <$> ((\c -> ("class" :: Text) =: c) <$> cl1 ) <*> p
              
      cl1 = ffor (nodeSelected n)
            (\case
              True -> "mindmap-node-selected" :: Text
              _ -> "mindmap-node-unselected")

      p = ffor (nodePosition n)
            (\case
              Nothing -> (("display" :: Text) =: "none")
              (Just pos) -> ("position" =: "absolute" <>
                              "left" =: showT (fst pos) <>
                              "top" =: showT (snd pos)))

      
  (t,_) <- elDynAttr' "li" attr $ do
        dynText $ nodeContent n

  let selectEvent = domEvent Click t
      --dblClick = domEvent Dblclick t

      ev1 = fmap (const $ i) selectEvent
      --ev2 = fmap (const $ nodeId node) dblClick

      ev1' = traceEvent "NodeClick" $ fmap (\i -> SelectNodeEvent i) ev1
      --ev2' = fmap (\i -> NodeDoubleClicked i) ev2

  return $ ev1' -- leftmost [ev2', ev1']

renderControlPanel :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
        => 
     ()
  -> m (Event t ControlPanelEvent)
renderControlPanel _ = return never
showT :: (Show a) => a -> T.Text
showT = T.pack.show
