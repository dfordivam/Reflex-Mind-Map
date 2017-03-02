module Data where

-- The source of these events can be Control Panel or rendered Mind Map itself
data InputEvent =
  -- These only modify view
    Click
  | Edit
  | EditFinish -- required?
  | OpenToggle

  -- These modify MindMap structure
  | InsertChild
  | Delete
  | Cut
  | Paste

data OpenToggleEv = OpenToggleEv NodeID

data NodeEditEv = NodeEditEv NodeID Text

type Position = (Int, Int)
type NodePos = Map NodeID Pos

newtype NodeID = NodeID { unNodeID :: Int }

data Node = Node {
    nodeID        :: NodeID
  , nodeContent   :: Dynamic t Text
  , nodeSelected  :: Dynamic t Bool
  , nodeOpen      :: Dynamic t Bool
  , nodePosition  :: Dynamic t (Maybe Position)
}

type NodeList = Map NodeID Node
type NodeTree = Map NodeID [NodeID]

data MindMap = MindMap {
    canvas        :: (Int, Int)
  , nodeList      :: NodeList
  , nodeTree      :: NodeTree
}


