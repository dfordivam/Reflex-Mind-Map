{-# LANGUAGE TemplateHaskell #-}

module Lib where


import Data.Text (Text)
import qualified Data.Text as T
import Data.FileEmbed

import Reflex
import Reflex.Dom

import qualified Controller

main = mainWidgetWithCss  $(embedFile "src/style.css") Controller.mindMapWidget
