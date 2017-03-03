module Data where

import Data.Text (Text)
import Data.Map (Map)

import Reflex.Dynamic

-- Model Data
--
type Position = (Int, Int)
type NodePos = Map NodeID Position

newtype NodeID = NodeID { unNodeID :: Int }
  deriving (Show, Ord, Eq)

data Node t = Node {
    nodeID        :: NodeID
  , nodeContent   :: Dynamic t Text
  , nodeSelected  :: Dynamic t Bool
  , nodeOpen      :: Dynamic t Bool
  , nodePosition  :: Dynamic t (Maybe Position)
}

type NodeList t = Map NodeID (Node t)
type NodeTree = Map NodeID [NodeID]

data MindMap t = MindMap {
    canvas        :: (Int, Int)
  , nodeList      :: NodeList t
  , nodeTree      :: NodeTree
}

-- Control Data
data AppState t = AppState {
    selectedNode  :: NodeID
  , nodePos       :: NodePos
  , mindMap       :: MindMap t
  , nodeListDiff  :: Map NodeID (Maybe (Node t))
}

-- Events
--
-- The source of these events can be Control Panel or rendered Mind Map itself

type AllEvents = 
  Either ControlPanelEvent NodeEvent

data ControlPanelEvent =
  -- These only modify view
    Edit
  | EditFinish -- required?
  | OpenToggle

  -- These modify MindMap structure
  | InsertChild
  | Delete
  | Cut
  | Paste

data OpenToggleEv = OpenToggleEv NodeID
data NodeEditEv = NodeEditEv NodeID Text

data NodeEvent =
    SelectNodeEvent NodeID

