-- Pure algo stuff
--

module Algo where

import DataModel

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

data Direction = LeftD | RightD
  deriving (Eq, Show)

-- Render tree
-- Determine the coordinates of each node based on the structure of tree
--
-- Starting with Root Node
-- Half its children are rendered on right and rest on left
-- Each children' sub forest is rendered in the cooresponding direction
--
-- A group of leaf nodes have a fixed height based on their number
-- This group's vertical position is such that half of it is above its parent
-- and the rest below.
-- The group of leaf nodes with its parent, can be thought as another
-- set of leaf nodes wrt to its own parent.
-- In this way vertical position of each node can be determined
--
-- For simplicity the horizontal position is kept constant

getNodeCoords :: MindMap -> [(Node, Coords)]
getNodeCoords mm = coords
  where
    -- Input Stuff
    nm = nodeMap mm
    nt = nodeTree mm
    csize = (\x -> (canvasWidth x, canvasHieght x)) $ canvas mm

    rootCoord :: (Double, Double)
    rootCoord = (\(x,y) ->
      (((fromIntegral x)/2), ((fromIntegral y)/2))) csize

    nodes = Map.keys nm
    
    dir :: NodeId -> Direction
    dir n = d
      where
        dirMap = constructDirMap nt
        (Just d) = Map.lookup n dirMap

    -- Output Stuff
    coords = map f nodes
      where f n = (node n, (ceiling (fst p), ceiling (snd p)))
              where p = absPos n
 
    absPos 0 = rootCoord
    absPos n = absPos (parent n) +++ pos n

    (+++) :: (Num a) => (a,a) -> (a,a) -> (a,a)
    (+++) (a,b) (c,d) = (a+c, b+d)
   
    
    parent :: NodeId -> NodeId
    parent 0 = error "parent"
    parent n = nodeParent (node n)

    node :: NodeId -> Node
    node n =
      let (Just nd) = Map.lookup n nm
      in nd

    pos n = (hPos n, vPos n)

    hPos n = if (dir n) == RightD
        then (width $ parent n)
        else (width n)
      where width n = fromIntegral
              (40 + (T.length $ nodeContent $ node n))

    -- Relative to parent pos
    vPos n = p * (h/p) + o - p/2
      where p = height (parent n)
            h = height n
            o = foldl (+) 0 (map height $ beforeSiblings n)

    height n = if c == 0 then 40 else c
      where c = foldl (+) 0 (map height $ children n)
    
    children :: NodeId -> [NodeId]
    children n = 
      let (Just ls) = Map.lookup n nt
      in ls
    
    beforeSiblings :: NodeId -> [NodeId]
    beforeSiblings n = takeWhile (/= n) c
      where c = children $ parent n
           


constructDirMap :: NodeTree -> Map NodeId Direction
constructDirMap nt = Map.map (\_ -> RightD) nt
