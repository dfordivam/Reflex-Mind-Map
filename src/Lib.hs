{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where


import Data.Text (Text)
import qualified Data.Text as T

import Reflex
import Reflex.Dom

import qualified Controller

main = mainWidget Controller.mindMapWidget
