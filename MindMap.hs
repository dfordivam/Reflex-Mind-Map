{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module MindMap where

import Prelude hiding (mapM, mapM_, all, sequence)

import Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Monad.Fix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
import Data.Monoid ((<>))
import Data.FileEmbed
import Data.Text (Text)
import qualified Data.Text as T

import Reflex
import Reflex.Dom

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

data Node = Node {
    nodeContent     :: Text
  , nodeSelected    :: Bool
}

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") mindMapWidget

initialNodes = [
    Node "n1" False
  , Node "N2" False
  ]

mindMapWidget :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
        => m ()
mindMapWidget = do
  el "div" $ do

    rec
      dyn1 <- holdDyn (Node "second" False) ev
      ev2 <- viewNode dyn1
      dyn2 <- holdDyn (Node "third" False) ev2
      ev3 <- viewNode dyn2

      dyn3 <- holdDyn (Node "first" False) ev3
      ev <- viewNode dyn3

    return ()

viewNode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
  => Dynamic t Node -> m (Event t Node)

viewNode node = do
  let dynAttr = fmap f node
      f n = let cl = if nodeSelected n then "textboxselected" else "textbox"
            in ("class" =: cl)
      dynT = fmap nodeContent node
 

  el "div" $ do
    text "in viewNode"

  (t,_) <- elDynAttr' "span" dynAttr  $ do
    dynText dynT

  let selectEvent = domEvent Click t
      nodeInv = current $ fmap (\n -> n {nodeSelected = not $ nodeSelected n}) node

  return $ tag nodeInv selectEvent

viewNodeCon :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
  => Node -> m (Event t Node)

viewNodeCon node = do
  let cl = if nodeSelected node then "textboxselected" else "textbox"
      attr = ("class" =: cl)
 

  el "div" $ do
    text "in viewNode"

  (t,_) <- elAttr' "span" attr  $ do
    text (nodeContent node)

  let selectEvent = domEvent Click t

  return $ fmap (\_ -> node {nodeSelected = not (nodeSelected node)}) selectEvent
