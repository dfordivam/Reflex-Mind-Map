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
    CanvasNodeEvents CanvasNodeEvents
  | KeyPressEvent KeyPressEvent
  deriving (Show)

data CanvasNodeEvents = 
    NodeClicked NodeId
  | NodeDoubleClicked NodeId
  | CanvasClicked
  | NodeEditText NodeId Text
  deriving (Show)

data KeyPressEvent =
    NodeCreate
  | NodeDelete
  | NodeEdit
  | NodeCollapseToggle
  deriving (Show)
  
data CssAttrs = CssAttrs {
    selectedNode :: Int
}
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
  --myMainWidgetWithCss $(embedFile "style.css") mindMapWidget
  mainWidgetWithHead' (headerCode, bodyCode)


type RefMonad m t = (DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           , TriggerEvent t m
           , MonadSample t m
           )

headerCode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           , TriggerEvent t m
           )
        => (Dynamic t CssAttrs) -> m ()
headerCode dynA = do
  let t = ffor dynA (\(CssAttrs n) -> T.pack $
        "." ++ (T.unpack (getNodeIdName n)) ++ " {color: red;}")
  el "style" $ dynText t

-- Application
-- Handle Events from Canvas, Keyboard and propagate them back to Canvas
bodyCode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           , TriggerEvent t m
           )
        => () -> m (Dynamic t CssAttrs)
bodyCode _ = do

  let mindMapOrig = MindMap (Canvas 600 400) nt nm
      nt = Map.fromList [
                  (0, [1,2])
                , (1, [3,4])
                , (2, [5])
                , (3, [6])
                , (4, [])
                , (5, [])
                , (6, [])
                ]
      nm = Map.fromList [
                  (0, Node 0 0 "0" False True)
                , (1, Node 1 0 "1" False True)
                , (2, Node 2 0 "2" False True)
                , (3, Node 3 1 "3" False True)
                , (4, Node 4 1 "4" False True)
                , (5, Node 5 2 "5" False True)
                , (6, Node 6 3 "6" False True)
                ]
      kev = never

  el "div" $ do
    text "Some text"
    kdyn <- holdDyn (NE.fromList [E.KeyPress 'a']) kev
    dynText $ fmap showT kdyn

    kpdyn <- holdDyn (NodeCreate) (fforMaybe kev keypressEvent)
    dynText $ fmap showT kpdyn

  appStateDyn <- el "div" $ do
    rec 
      dynT <- holdDyn "Debug Info" $ fmap (showT) debugInfo
      dynT2 <- holdDyn "Node Clicked: " $ fmap (showT) clickEvent

      (clickEvent, debugInfo) <- drawCanvas 
        mindMapDyn editNodeDyn

      -- (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)
      dynData <- foldDynMaybe handleEvent
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

        clickEvent' = fmap CanvasNodeEvents clickEvent
        keybEv' = fmap KeyPressEvent keypressEv

        canvasEv' = leftmost [clickEvent',keybEv']
        canvasEv = traceEventWith show canvasEv'

      el "div" $ text "Text after canvas"
      dynText dynT
      dynText dynT2
      return ()
    return appStateDyn

  let cssAttrDyn = ffor appStateDyn
                    (\case
                      SelectedNode n -> CssAttrs n
                      EditingNode n -> CssAttrs n)
  return cssAttrDyn

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
  -> Maybe (AppState, MindMap)
handleEvent (KeyPressEvent _) a@(EditingNode _,_) = Nothing  -- Ignore keypress events when editing
  
handleEvent ev (appState, mindMap) = Just $
  case ev of
    CanvasNodeEvents (NodeClicked n) ->
      (SelectedNode n, mindMap {nodeMap = newNodes})
      where
        newNodes = Map.mapWithKey f nodes
        f k a = a {nodeSelected = (k == n)}
      
    CanvasNodeEvents (NodeDoubleClicked n) ->
      (EditingNode n, mindMap {nodeMap = newNodes})
      where
        newNodes = Map.mapWithKey f nodes
        f k a = a {nodeSelected = False}

    CanvasNodeEvents CanvasClicked ->
      (SelectedNode n, mindMap {nodeMap = newNodes})
      where
        newNodes = Map.mapWithKey f nodes
        f k a = a {nodeSelected = False}

    CanvasNodeEvents (NodeEditText n txt) ->
      (SelectedNode n, mindMap {nodeMap = newNodes})
      where
        newNodes = Map.update
          (\a -> Just $ a {nodeSelected = True, nodeContent = txt}) n nodes

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
  -> m (Event t CanvasNodeEvents, Event t DebugInfo)

drawCanvas mindMapDyn en = do

  let 

      attr = ("class" =: "mindmap-canvas")
      csize = fmap (\c -> (canvasWidth c, canvasHieght c)) $
              fmap canvas mindMapDyn
      
  ev <- elAttr "ul" attr $ do
    canvasMouseEv <- renderTree mindMapDyn en

    let mouseEvent = tag (constant (100,100)) canvasMouseEv
    -- mouseEvent <- wrapDomEvent 
    --   (_element_raw elm) 
    --   (onEventName Mousemove) 
    --   mouseOffsetXY
    let dbgInfo = fmap CanvasDebugInfo mouseEvent

    return (canvasMouseEv, dbgInfo)

  return ev

getNode :: Functor f => NodeId -> f MindMap -> f Node
getNode i mm = fmap f mm
  where 
    f mm' = n
      where
        Just n = Map.lookup i (nodeMap mm')

getChildren
  :: Reflex t =>
       Dynamic t Node -> Dynamic t MindMap -> Dynamic t [Node]
getChildren n mm = zipDynWith f n mm
  where 
    f n' mm' = map g ns
      where
        Just ns = Map.lookup i (nodeTree mm')
        i = nodeId n'

        g i = n
          where
            Just n = Map.lookup i (nodeMap mm')


renderTree :: RefMonad m t
  => Dynamic t MindMap
  -> Dynamic t EditNode
  -> m (Event t CanvasNodeEvents)
renderTree mindMapDyn editNodeDyn = do
  let 
      --nodeCoords = fmap getNodeCoords mindMapDyn

      rootNode = getNode 0 mindMapDyn

  ev1 <- elAttr "li" ("class" =: "mindmap-rootnode") $
    renderOnlyNode editNodeDyn rootNode

  ev2 <- el "li" $ 
    elAttr "ul" ("class" =: "mindmap-children-right") $
    renderChildren editNodeDyn mindMapDyn
      (getChildren rootNode mindMapDyn)
  
  return (leftmost [ev1, ev2])

renderChildren :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
  => Dynamic t EditNode
  -> Dynamic t MindMap
  -> Dynamic t [Node]
  -> m (Event t CanvasNodeEvents)

renderChildren e mm ns = do
  evs <- simpleList ns (renderNode e mm)
  let d = fmap leftmost evs
  return $ switchPromptlyDyn d

renderOnlyNode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
  => Dynamic t EditNode
  -> Dynamic t Node
  -> m (Event t CanvasNodeEvents)

renderOnlyNode e n = do
  val <- dyn $ d
  ev <- switchPromptly never val
  return ev
  where
    -- d :: Dynamic t (Map NodeId (Node, Coords) , EditNode)
    d = zipDynWith f e n
    
    f e' n' = if (e' == (Just (nodeId $ n')))
                  then editNode n'
                  else viewNode n'
    

renderNode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
  => Dynamic t EditNode
  -> Dynamic t MindMap
  -> Dynamic t Node
  -> m (Event t CanvasNodeEvents)

renderNode e mm n =
  el "li" $
  elAttr "ul" ("class" =: "mindmap-childtree") $ do
    ev <- renderOnlyNode e n
    ev2 <- el "li" $ 
      elAttr "ul" ("class" =: "mindmap-children") $ do
      val2 <- dyn $ fmap g n
      switchPromptly never val2
      
    return $ leftmost [ev, ev2]
  where
    g n' = if nodeOpen n'
            then renderChildren e mm (getChildren n mm)
            else return never

editNode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
  => Node
  -> m (Event t CanvasNodeEvents)

editNode node= do
  let 
      attr = ("class" =: "mindmap-editnode")

  (_, ev) <- elAttr' "li" attr $ do
      -- Create the textbox; it will be cleared whenever the user presses enter
      rec let newValueEntered = ffilter (keyCodeIs Enter) (_textInput_keypress descriptionBox)
          descriptionBox <- textInput $ def
            & textInputConfig_initialValue .~ (nodeContent node)
            & textInputConfig_setValue .~ fmap (const "") newValueEntered
            & textInputConfig_attributes .~ 
            constDyn (mconcat [ "class" =: "edit-node"
                , "placeholder" =: "Node"
                , "autofocus" =: ""
            ])

      -- Request focus on this element when the widget is done being built
--      schedulePostBuild $ liftIO $ focus $ _textInput_element descriptionBox
      let -- | Get the current value of the textbox whenever the user hits enter
          newValue = tag (current $ _textInput_value descriptionBox) newValueEntered
      return $ fmap (\d -> NodeEditText (nodeId node) d) newValue

  return $ ev

getNodeIdName :: Int -> Text
getNodeIdName n = T.pack ("node" ++ show n)

-- Output : Node select event
viewNode :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
  => Node
  -> m (Event t CanvasNodeEvents)

viewNode node = do
  let 
      attr = let cl = if nodeSelected node
                        then "mindmap-node-selected"
                        else "mindmap-node-unselected"
             in ("classs" =: cl) <> 
                ("class" =: getNodeIdName (nodeId node))

  (t,_) <- elAttr' "li" attr $ do
    text $ nodeContent node


  let selectEvent = domEvent Click t
      dblClick = domEvent Dblclick t

      ev1 = fmap (const $ nodeId node) selectEvent
      ev2 = fmap (const $ nodeId node) dblClick

      ev1' = fmap (\i -> NodeClicked i) ev1
      ev2' = fmap (\i -> NodeDoubleClicked i) ev2

  return $ leftmost [ev2', ev1']

keyCodeIs :: Key -> KeyCode -> Bool
keyCodeIs k c = keyCodeLookup c == k

showT :: (Show a) => a -> T.Text
showT = T.pack.show

