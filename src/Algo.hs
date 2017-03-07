{-# LANGUAGE LambdaCase #-}
-- Pure algo stuff
-- No Dom related things here

module Algo where

import Reflex

import Data

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad (join)

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

getNodePos :: (Reflex t) => MindMap t -> Map NodeID (Dynamic t (Maybe Position))
getNodePos (MindMap csize nm nt) = coords
  where
    rootCoord :: (Int, Int)
    rootCoord = (\(x,y) ->
      (ceiling ((fromIntegral x)/2),
       ceiling ((fromIntegral y)/2))) csize

    dir :: NodeID -> Direction
    dir n = d
      where
        dirMap = constructDirMap nt
        (Just d) = Map.lookup n dirMap

    -- Output Stuff
    coords = Map.mapWithKey absPos nm

    -- Absolute position is dynamic based on
    -- all of these dynamic conditions
    -- 1. Whether parent node is open/collapsed
    -- 2. Absolute position of parent
    -- 3. Relative position of current node
    -- absPos :: (Reflex t) => NodeID -> (Node t) -> Dynamic t (Maybe Position)
    absPos (NodeID 0) _  = constDyn (Just rootCoord)
    absPos nId n = pPos +++ pos nId
      where
        p = nodeParent n
        pNode = node p
        pPos = join $ ffor (nodeOpen pNode)
          (\case
            True -> absPos p pNode
            False -> constDyn Nothing)

    (+++) :: (Num a, Reflex t) =>
         Dynamic t (Maybe Position)
      -> Dynamic t (Maybe Position)
      -> Dynamic t (Maybe Position)
    (+++) p1 p2 = join $ combineDyn f p1 p2
      where
        f (Just (a,b)) (Just (c,d)) = Just (a+c, b+d)
        f _ _ = Nothing


    parent :: NodeID -> NodeID
    parent (NodeID 0) = error "parent"
    parent n = nodeParent (node n)

    node n =
      let (Just nd) = Map.lookup n nm
      in nd

    pos n = ffor (vPos n) (\v -> Just (hPos n, v))

    nodeDefaultWidth = 100

    hPos n = if (dir n) == RightD
        then (width $ parent n)
        else (width n)
      where width n = nodeDefaultWidth -- fromIntegral
              --(40 + (T.length $ nodeContent $ node n))

    -- Relative to parent pos
    -- h/2 + o - p/2
    vPos n = (+) <$>
                (half h) <*>
                ((-) <$> o <*> (half p))
      where p = height (parent n)
            h = height n
            o = foldAdd (map height $ beforeSiblings n)
            half = fmap (\v -> ceiling ((fromIntegral v)/2))

    foldAdd = foldl (\a b -> (+) <$> a <*> b) (constDyn 0)
    nodeDefaultHeight = 40

    -- Height is dependent on whether the node is open/close
    --height :: NodeID -> Dynamic t Float
    height n = if null (children n)
                 then constDyn nodeDefaultHeight
                 else h
      where
        h = join $ ffor (nodeOpen (node n))
          (\case
            True -> foldAdd (map height $ children n)
            False -> constDyn nodeDefaultHeight)

    children :: NodeID -> [NodeID]
    children n =
      let (Just ls) = Map.lookup n nt
      in ls

    beforeSiblings :: NodeID -> [NodeID]
    beforeSiblings n = takeWhile (/= n) c
      where c = children $ parent n



constructDirMap :: NodeTree -> Map NodeID Direction
constructDirMap nt = Map.map (\_ -> RightD) nt
