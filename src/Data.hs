module Data where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dynamic

-- Model Data
--
newtype NodeID = NodeID { unNodeID :: Int }
  deriving (Show, Ord, Eq)

data Node t = Node {
    nodeID        :: NodeID
  , nodeParent    :: NodeID
  , nodeContent   :: Dynamic t Text
  , nodeState     :: Dynamic t NodeState
  , nodeOpen      :: Dynamic t Bool
  , nodePosition  :: Dynamic t (Maybe Position)
}

data NodeState =
    NodeUnselected
  | NodeSelected
  | NodeEditing

type Position = (Int, Int)
type NodePos = Map NodeID (Maybe Position)

type NodeList t = Map NodeID (Node t)
type NodeTree = Map NodeID [NodeID]

data MindMap t = MindMap {
    canvas        :: (Int, Int)
  , nodeList      :: NodeList t
  , nodeTree      :: NodeTree
}

-- Control Data
data AppState t = AppState {
    selectedNode  :: (NodeID, Bool) -- (Editing?)
  , mindMap       :: MindMap t
  -- This diff will trigger rerendering
  , nodeListDiff  :: Map NodeID (Maybe (Node t))
}

-- Events
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
  deriving (Show, Ord, Eq)

-- Events generated from Node
data NodeEvent =
    SelectNodeEvent NodeID
  | EditFinishNodeEvent NodeID Text
  deriving (Show, Ord, Eq)

-- Special events for node
-- These are just wrappers/alias for respective events
data OpenToggleEv = OpenToggleEv NodeID
  deriving (Show, Ord, Eq)
data NodeEditEv = NodeEditEv Text NodeID
  deriving (Show, Ord, Eq)

instance Show (AppState t) where
  show st = show (selectedNode st) ++ " "
            ++ " DiffKeysNew: " ++ show (Map.keys $ nodeListDiff st)

