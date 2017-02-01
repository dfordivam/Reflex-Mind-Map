{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

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
import GHCJS.DOM.Document (Document)


import Reflex
import Reflex.Dom

import qualified Event as E
import MyWidget (myMainWidgetWithCss)
import DataModel
import Algo

import qualified Data.List.NonEmpty as NE

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


type EditNode = Maybe NodeId

data AppState =
    SelectedNode NodeId
  | EditingNode NodeId
  deriving (Show, Eq)

data CanvasEvent = 
    CanvasMouseEvents CanvasMouseEvents
  | KeyPressEvent KeyPressEvent
  deriving (Show)

data CanvasMouseEvents = 
    NodeClicked NodeId
  | NodeDoubleClicked NodeId
  | CanvasClicked
  deriving (Show)

data KeyPressEvent =
    NodeCreate
  | NodeDelete
  | NodeEdit
  | NodeCollapseToggle
  deriving (Show)
  
data DebugInfo = CanvasDebugInfo {
    debugInfoMouseCoord :: (Int, Int)
}
  deriving (Show)
--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

main :: IO ()
main = 
  myMainWidgetWithCss $(embedFile "style.css") mindMapWidget


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
        => Document -> m ()
mindMapWidget doc = do
  kev <- E.globalKeyEvents doc

  let mindMapOrig = MindMap (Canvas 600 400) nt nm
      nt = Map.fromList [
                  (0, [1])
                , (1, [2])
                , (2, [])
                ]
      nm = Map.fromList [
                  (0, Node 0 0 "0" False True)
                , (1, Node 1 0 "1" False True)
                , (2, Node 2 1 "2" False True)
                ]
  el "div" $ do
    text "Some text"
    kdyn <- holdDyn (NE.fromList [E.KeyPress 'a']) kev
    dynText $ fmap showT kdyn

    kpdyn <- holdDyn (NodeCreate) (fforMaybe kev keypressEvent)
    dynText $ fmap showT kpdyn

  el "div" $ do
    rec 
      dynT <- holdDyn "Debug Info" $ fmap (showT) debugInfo
      dynT2 <- holdDyn "Node Clicked: " $ fmap (showT) clickEvent

      (clickEvent, debugInfo) <- drawCanvas 
        mindMapDyn editNodeDyn

      -- (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)
      dynData <- foldDyn handleEvent
        (SelectedNode 0, mindMapOrig) canvasEv
      
      let 
        
        (appStateDyn, mindMapDyn) = splitDynPure dynData

        nodeMapDyn = traceDynWith show $ fmap nodeMap mindMapDyn

        nodeTreeDyn = fmap nodeTree mindMapDyn
        editNodeDyn = ffor appStateDyn
          (\case
            SelectedNode _ -> Nothing
            EditingNode n -> Just n)

        keypressEv = fforMaybe kev keypressEvent

        clickEvent' = fmap CanvasMouseEvents clickEvent
        keybEv' = fmap KeyPressEvent keypressEv

        canvasEv' = leftmost [clickEvent',keybEv']
        canvasEv = traceEventWith show canvasEv'

      el "div" $ text "Text after canvas"
      dynText dynT
      dynText dynT2
      return ()
    return ()

keypressEvent :: NE.NonEmpty E.KeyEvent -> Maybe KeyPressEvent
keypressEvent x = case (NE.head x) of
  E.KeyPress 'i' -> Just NodeCreate
  E.KeyStroke E.Down E.Delete 
    (E.Modifiers False False False) -> Just NodeDelete
  E.KeyPress 'e' -> Just NodeEdit
  _ -> Nothing

handleEvent ::
     CanvasEvent
  -> (AppState, MindMap)
  -> (AppState, MindMap)
handleEvent ev (appState, mindMap) =
  case ev of
    CanvasMouseEvents (NodeClicked n) ->
      (SelectedNode n, mindMap {nodeMap = newNodes})
      where
        newNodes = Map.mapWithKey f nodes
        f k a = a {nodeSelected = (k == n)}
      
    CanvasMouseEvents (NodeDoubleClicked n) ->
      (EditingNode n, mindMap {nodeMap = newNodes})
      where
        newNodes = Map.mapWithKey f nodes
        f k a = a {nodeSelected = False}

    CanvasMouseEvents CanvasClicked ->
      (SelectedNode n, mindMap {nodeMap = newNodes})
      where
        newNodes = Map.mapWithKey f nodes
        f k a = a {nodeSelected = False}

    KeyPressEvent NodeCreate ->
      (EditingNode $ nodeId newNode, mindMap {nodeMap = newNodes, 
        nodeTree = newTree})
      where
        (k, _) = Map.findMax nodes
        parent = n
        newNode = Node (k+1) parent (showT $ (k+1)) True True

        newNodes' = Map.mapWithKey f nodes
        f k a = a {nodeSelected = False}

        newNodes = Map.insert (nodeId newNode) newNode newNodes'

        newTree' = Map.insert (nodeId newNode) [] (nodeTree mindMap)
        newTree  = Map.update (\x -> Just $ x ++ [nodeId newNode])
                      parent (newTree')


    KeyPressEvent NodeDelete ->
      (SelectedNode $ nodeParent node, mindMap {nodeMap = newNodes})
      where
        newNodes' = Map.mapWithKey f nodes
        f k a = a {nodeSelected = False}

        newNodes = Map.delete n newNodes'

    KeyPressEvent NodeEdit   ->
      (EditingNode n, mindMap)
          

  where 
    nodes = nodeMap mindMap
    (Just node) = Map.lookup n nodes 
    n = f appState
    f (SelectedNode n) =  n
    f (EditingNode n) =  n


-- handleClickEvent :: (
--      Event t CanvasMouseEvents
--   -> Dynamic t NodeMap
--   -> (Event t EditNode, Event t NodeMap)

        
-- 
drawCanvas :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           , TriggerEvent t m
           )
  => Dynamic t MindMap 
  -> Dynamic t EditNode -- Current node to edit
  -> m (Event t CanvasMouseEvents, Event t DebugInfo)

drawCanvas mindMapDyn en = do

  
  -- Construct tree
  -- Render nodes at proper position
  --
  -- Simple render a list
  let 
      svgAttr = ffor csize $ (\(x,y) -> 
                  Map.fromList [
                    ("xmlns", "http://www.w3.org/2000/svg")
                  , ("width", showT x)
                  , ("height", showT y)
                  , ("style", "border:solid; margin:4em")
                  ])

      csize = fmap (\c -> (canvasWidth c, canvasHieght c)) $
              fmap canvas mindMapDyn
      
  (elm, ev) <- elDynAttrNS' svgns "svg" svgAttr $ do
    canvasMouseEv <- renderTree mindMapDyn en

    let mouseEvent = tag (constant (100,100)) canvasMouseEv
    -- mouseEvent <- wrapDomEvent 
    --   (_element_raw elm) 
    --   (onEventName Mousemove) 
    --   mouseOffsetXY
    let dbgInfo = fmap CanvasDebugInfo mouseEvent

    return (canvasMouseEv, dbgInfo)

  return ev

renderTree :: RefMonad m t
  => Dynamic t MindMap
  -> Dynamic t EditNode
  -> m (Event t CanvasMouseEvents)
renderTree mindMapDyn editNodeDyn = do
  let 
      -- nodeCoords :: Dynamic t (Map NodeId (Node, Coords))
      nodeCoords = fmap getNodeCoords mindMapDyn

      -- Take edit node value and decide render function
      -- f (m,e) = Map.mapWithKey 
      --   (\k a -> (a
      --    , if (Just k) == e then editNode else viewNode)) m

      -- r :: Dynamic t Map NodeId ((Node, Coords), 
      --   (Dynamic t (Node, Coords) -> m (Event t CanvasMouseEvents)))
      -- r = fmap f d

      -- g :: Dynamic t ((Node, Coords), 
      --   (Dynamic t (Node, Coords) -> m (Event t CanvasMouseEvents)))
      --   -> m (Event t CanvasMouseEvents)
      -- g d = fmap coincidence $ dyn h
      --   where
      --     -- v :: Dynamic t (Node, Coords)
      --     -- f :: Dynamic t (Dynamic t (Node, Coords) -> m (Event t CanvasMouseEvents))
      --     (v, f) = splitDyn d
      --     
      --     -- h :: Dynamic t (m (Event t CanvasMouseEvents))
      --     h = fmap (\f' -> f' v) f

  evMap <- list nodeCoords (renderNode editNodeDyn)
  return $ switchPromptlyDyn $ fmap leftmost
    $ fmap (map snd) $ fmap (Map.toList) evMap

renderNode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
  => Dynamic t EditNode
  -> Dynamic t (Node, Coords)
  -> m (Event t CanvasMouseEvents)

renderNode e v = do
  val <- dyn $ fmap f d
  return $ coincidence val
  where
    -- d :: Dynamic t (Map NodeId (Node, Coords) , EditNode)
    d = zipDynWith (,) e v

    f (e',v') = if (e' == (Just (nodeId $ fst v')))
                  then editNode v
                  else viewNode v


editNode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
  => Dynamic t (Node, Coords)
  -> m (Event t CanvasMouseEvents)

editNode nodeCoord = do
  let node = fmap fst nodeCoord
      coord = fmap snd nodeCoord
      dynAttr = (<>) <$> (f1 <$> node) <*> (f2 <$> coord)

      f1 n = let cl = if nodeSelected n then "red" else "black"
             in ("fill" =: cl)

      f2 (x,y) = ("x" =: showT x <> "y" =: showT y)
      
      dynT = fmap nodeContent node
 
  (t,_) <- elDynAttrNS' svgns "text" dynAttr  $ do
    dynText dynT


  let selectEvent = domEvent Click t
      dblClick = domEvent Dblclick t

      ev1 = tag (current $ fmap nodeId node) selectEvent
      ev2 = tag (current $ fmap nodeId node) dblClick

      ev1' = fmap (\i -> NodeClicked i) ev1
      ev2' = fmap (\i -> NodeDoubleClicked i) ev2

  --elDynAttr "foreignObject" attr $ do
  --  _ <- elDynAttrNS' xmlns "body" (constDyn Map.empty) $
  --    el "form" $
  --      elAttr "input" ("type" =: "text") $ return ()
  return $ leftmost [ev2', ev1']


-- Output : Node select event
viewNode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
  => Dynamic t (Node, Coords)
  -> m (Event t CanvasMouseEvents)

viewNode nodeCoord = do
  let node = fmap fst nodeCoord
      coord = fmap snd nodeCoord
      dynAttr = (<>) <$> (f1 <$> node) <*> (f2 <$> coord)

      f1 n = let cl = if nodeSelected n then "red" else "black"
             in ("fill" =: cl)

      f2 (x,y) = ("x" =: showT x <> "y" =: showT y)
      
      dynT = fmap nodeContent node
 
  (t,_) <- elDynAttrNS' svgns "text" dynAttr  $ do
    dynText dynT


  let selectEvent = domEvent Click t
      dblClick = domEvent Dblclick t

      ev1 = tag (current $ fmap nodeId node) selectEvent
      ev2 = tag (current $ fmap nodeId node) dblClick

      ev1' = fmap (\i -> NodeClicked i) ev1
      ev2' = fmap (\i -> NodeDoubleClicked i) ev2

  --elDynAttr "foreignObject" attr $ do
  --  _ <- elDynAttrNS' xmlns "body" (constDyn Map.empty) $
  --    el "form" $
  --      elAttr "input" ("type" =: "text") $ return ()
  return $ leftmost [ev2', ev1']

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

