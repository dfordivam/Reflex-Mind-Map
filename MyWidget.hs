{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}

module MyWidget where

import GHCJS.DOM.Document
import qualified GHCJS.DOM.Types as DOM
import Data.ByteString (ByteString)

import qualified Data.Text as T

import Reflex
import Reflex.Dom

import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Class
import Reflex.Dom.Internal.Foreign
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PostBuild.Base
import Reflex.Spider (Global, Spider, SpiderHost, runSpiderHost)
import Reflex.TriggerEvent.Base

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Reader hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Control.Monad.Ref
import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum (..))
import Data.IORef
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import GHCJS.DOM hiding (runWebGUI)
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import qualified GHCJS.DOM.Types as DOM


myMainWidgetWithCss :: ByteString -> (forall x. Document -> Widget x ()) -> IO ()
myMainWidgetWithCss css w =
  runWebGUI $ \webView -> withWebViewSingleton webView $ \webViewSing -> do
  Just doc <- fmap DOM.castToHTMLDocument <$> webViewGetDomDocument webView
  Just doc' <- webViewGetDomDocument webView
  
  Just headElement <- fmap DOM.castToHTMLElement <$> getHead doc
  setInnerHTML headElement . Just $ "<style>" <> T.unpack (decodeUtf8 css) <> "</style>" --TODO: Fix this
  Just body <- getBody doc
  attachWidget body webViewSing (w doc')

