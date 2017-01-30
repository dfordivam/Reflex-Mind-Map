{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

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

import GHCJS.DOM.EventM (mouseOffsetXY)

import Reflex
import Reflex.Dom

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

-- Application State
-- Reading - No node selected
--
-- Node selected - Do action based on user input
--  - 'i' - insert new child node
--  - 'space' - close tree
--  - 'del' - delete tree
--  - arrow keys - select different node
--  - 'e' - enter edit node mode
--
-- editing node - after double click, exit this on 'Enter'
-- drag node - If clicked on node and dragged
-- drag whole tree - If clicked outside node and dragged
-- multiple nodes selected - using ctrl

-- Application
-- Take mouse events from canvas and keyboard events to maintain state
--

-- Canvas rendering
-- Print root in middle
-- Input : Node Map
-- Output : Mouse events on all nodes, and empty space
--          Which node is clicked/empty space clicked

-- Tree rendering
-- Easy - print list
--

-- Node rendering
-- Input : Node
-- Output : Click Event, Double Click Event, TextBoxValue Event
--    

data Canvas = Canvas {
    canvasWidth   :: Int
  , canvasHieght  :: Int
}
  deriving (Show)

type NodeId = Int

data Node = Node {
    nodeId          :: NodeId
  , nodeParent      :: NodeId
  , nodeContent     :: Text
  , nodeSelected    :: Bool
  , nodeOpen        :: Bool
}
  deriving (Show)

type NodeTree = Map NodeId [NodeId]
type NodeMap = Map NodeId Node

type Coords = (Int, Int)

type EditNode = Maybe NodeId

data MindMap = MindMap {
    canvas          :: Canvas
  , nodeTree        :: NodeTree
  , nodeMap         :: NodeMap
}
  deriving (Show)

data CanvasMouseEvents = 
    NodeClicked NodeId
  | NodeDoubleClicked NodeId
  | CanvasClicked
  deriving (Show)

data DebugInfo = CanvasDebugInfo {
    debugInfoMouseCoord :: (Int, Int)
}
  deriving (Show)
--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") mindMapWidget

type RefMonad m t = (DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           , TriggerEvent t m
           , MonadSample t m
           )

-- Application
-- Handle Events from Canvas, Keyboard and propagate them back to Canvas
mindMapWidget :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           , TriggerEvent t m
           )
        => m ()
mindMapWidget = do
  let mindMapOrig = MindMap (Canvas 600 400) nt nm
      nt = Map.fromList [
                  (0, [1])
                , (1, [2])]
      nm = Map.fromList [
                  (0, Node 0 0 "0" False True)
                , (1, Node 1 0 "1" False True)
                , (2, Node 2 1 "2" False True)
                ]
  el "div" $ do
    text "Some text"

  el "div" $ do
    rec 
      editNodeDyn <- holdDyn Nothing editNodeEv    
      nodeMapDyn <- holdDyn (nodeMap mindMapOrig) nodeMapEv

      dynT <- holdDyn "Debug Info" $ fmap (showT) debugInfo
      dynT2 <- holdDyn "Node Clicked: " $ fmap (showT) clickEvent

      --clickDyn <- holdDyn CanvasClicked clickEvent

      (clickEvent, debugInfo) <- drawCanvas (Canvas 400 200)
            nodeTreeDyn nodeMapDyn editNodeDyn

      let   (editNodeEv, nodeMapEv) = handleClickEvent clickEvent nodeMapDyn
            --(editNodeEv, nodeMapEv) = (never,never)
            --(editNodeDyn, nodeMapDyn) = 
            --  (constDyn Nothing, constDyn $ Map.empty)
            nodeTreeDyn = constDyn (nodeTree mindMapOrig)
            --debugInfo = tag (constant CanvasClicked) never
            --clickEvent = tag (constant CanvasClicked) never
      el "div" $ text "hello"
      dynText dynT
      dynText dynT2
      return ()
    return ()

-- handleClickEvent :: (
--      Event t CanvasMouseEvents
--   -> Dynamic t NodeMap
--   -> (Event t EditNode, Event t NodeMap)

handleClickEvent ev nodes = 
  (\(a,b) -> (a, coincidence b)) $
  splitE $ fmap f ev
  where 
    f ev' =
       case ev' of
         NodeClicked n -> (Nothing, newNodesEv)
           where
             newNodes = fmap (Map.mapWithKey f) nodes
             newNodesEv = tag (current newNodes) ev
             f k a = a {nodeSelected = s}
               where s = if k == n then True else False
           
         NodeDoubleClicked n -> (Just n, newNodesEv)
           where
             newNodes = fmap (Map.mapWithKey f) nodes
             newNodesEv = tag (current newNodes) ev
             f k a = a {nodeSelected = False}

         CanvasClicked -> (Nothing, newNodesEv)
           where
             newNodes = fmap (Map.mapWithKey f) nodes
             newNodesEv = tag (current newNodes) ev
             f k a = a {nodeSelected = False}

-- 
drawCanvas :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           , TriggerEvent t m
           )
  => Canvas 
  -> Dynamic t NodeTree -- Structure of MindMap
  -> Dynamic t NodeMap  -- Contents of MindMap
  -> Dynamic t EditNode -- Current node to edit
  -> m (Event t CanvasMouseEvents, Event t DebugInfo)

drawCanvas c nt nm en = do

  -- Construct tree
  -- Render nodes at proper position
  --
  -- Simple render a list
  let svgAttr = constDyn $ 
                  Map.fromList [
                    ("xmlns", "http://www.w3.org/2000/svg")
                  , ("width", "600" )
                  , ("height", "400")
                  , ("style", "border:solid; margin:4em")
                  ]
  (elm, ev) <- elDynAttrNS' Nothing "svg" svgAttr $ do
    canvasMouseEv <- renderTree nm

    let mouseEvent = tag (constant (100,100)) canvasMouseEv
    -- mouseEvent <- wrapDomEvent 
    --   (_element_raw elm) 
    --   (onEventName Mousemove) 
    --   mouseOffsetXY

    let dbgInfo = fmap CanvasDebugInfo mouseEvent

    return (canvasMouseEv, dbgInfo)

  return ev

initCoord :: (Int, Int)
initCoord = (10, 10)

renderTree :: RefMonad m t
  => Dynamic t NodeMap -> m (Event t CanvasMouseEvents)
renderTree nodeMap =
  let 
    --f :: (RefMonad m t) =>
    --  Int -> [(NodeId, Node)] -> m (Event t CanvasMouseEvents)
    --f _ [] = return (never)
    --f y (n:ns) = do
    --  --ev <- viewNode (constDyn (snd n)) (constDyn (10,y))
    --  --ev2 <- f (y + 10) ns
    --  let ev = never
    --  return ev -- (mergeWith const [ev2,ev])
    f k n = viewNode n (constDyn (10,k*10))
  in do
     -- listViewWithKey
     --  (k -> Dynamic t v -> m (Event t a)) -> m (Event t (Map k a))
     evMap <- listViewWithKey nodeMap f 
     let ev = fmap (snd.head.Map.toList) evMap
         
     return ev

     --f 10 ns

-- editNode :: Dynamic t (Text, Coords) -> m ()

-- Output : Node select event
viewNode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
  => Dynamic t Node
  -> Dynamic t Coords
  -> m (Event t CanvasMouseEvents)

viewNode node coord = do
  let dynAttr = (<>) <$> (f1 <$> node) <*> (f2 <$> coord)

      f1 n = let cl = if nodeSelected n then "red" else "black"
             in ("fill" =: cl)

      f2 (x,y) = ("x" =: showT x <> "y" =: showT y)
      
      dynT = fmap nodeContent node
 
  (t,_) <- elDynAttr' "text" dynAttr  $ do
    dynText dynT


  let selectEvent = domEvent Click t
      dblClick = domEvent Dblclick t

      ev1 = tag (current $ fmap nodeId node) selectEvent
      ev2 = tag (current $ fmap nodeId node) dblClick

      ev1' = fmap (\i -> NodeClicked i) ev1
      ev2' = fmap (\i -> NodeDoubleClicked i) ev1

  --elDynAttr "foreignObject" attr $ do
  --  _ <- elDynAttrNS' xmlns "body" (constDyn Map.empty) $
  --    el "form" $
  --      elAttr "input" ("type" =: "text") $ return ()
  return $ leftmost [ev1', ev2']

-- 
-- canvasContents :: ( DomBuilder t m
--            , DomBuilderSpace m ~ GhcjsDomSpace
--            , MonadFix m
--            , MonadHold t m
--            , PostBuild t m
--            )
--   => Dynamic t Coords -> m ()
-- canvasContents cord = do
--   let attr = fmap (\(x,y) ->
--           (Map.fromList [
--             ("x", showT x)
--           , ("y", showT y)
--           , ("width", "50")
--           , ("height", "30")
--           --, ("fill", "red")
--           ])) cord
--   --elDynAttr "text" attr
--   --  $ text "Node Text"
--     return ()
-- 
-- viewNodeCon :: ( DomBuilder t m
--            , DomBuilderSpace m ~ GhcjsDomSpace
--            , MonadFix m
--            , MonadHold t m
--            , PostBuild t m
--            )
--   => Node -> m (Event t Node)
-- 
-- viewNodeCon node = do
--   let cl = if nodeSelected node then "textboxselected" else "textbox"
--       attr = ("class" =: cl)
 

--  el "div" $ do
--    text "in viewNode"
--
--  (t,_) <- elAttr' "span" attr  $ do
--    text (nodeContent node)
--
--  let selectEvent = domEvent Click t
--
--  return $ fmap (\_ -> node {nodeSelected = not (nodeSelected node)}) selectEvent

showT :: (Show a) => a -> T.Text
showT = T.pack.show

xmlns :: Maybe Text
xmlns = (Just "http://www.w3.org/1999/xhtml")

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

