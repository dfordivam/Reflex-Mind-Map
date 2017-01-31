-- Model definitions
--
module DataModel where

import Data.Text (Text)
import Data.Map (Map)

data MindMap = MindMap {
    canvas          :: Canvas
  , nodeTree        :: NodeTree
  , nodeMap         :: NodeMap
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
  deriving (Show, Eq)

type NodeTree = Map NodeId [NodeId]
type NodeMap = Map NodeId Node

data Canvas = Canvas {
    canvasWidth   :: Int
  , canvasHieght  :: Int
}
  deriving (Show)

type Coords = (Int, Int)

