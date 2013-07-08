-- | A renderer that produces a GHCJS list of DOM nodes.

module Text.Blaze.Renderer.GHCJS
    ( renderMarkup
    ) where

import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.Types
import Control.Applicative
import Text.Blaze.Internal
import Text.Blaze.Renderer.String (fromChoiceString)

renderMarkup :: IsDocument doc => doc   -- ^ Document to create nodes with
             -> Markup    -- ^ Markup to render
             -> IO [Node]  -- ^ Resulting list of GHCJS DOM nodes
renderMarkup doc = go
  where
    withNewElement :: String -> (Node -> IO [Node]) -> IO [Node]
    withNewElement name f = do
      mn <- documentCreateElement doc name
      case mn of
        Just n -> f (castToNode n)
        Nothing -> return []
    go :: MarkupM b -> IO [Node]
    go (Parent tag _ _ content) =
      withNewElement (getString tag "") $ \n -> do
        nl <- go content
        mapM_ (nodeAppendChild n . Just) nl
        return [n]
    go (CustomParent tag content) = do
      withNewElement (fromChoiceString tag "") $ \n -> do
        nl <- go content
        mapM_ (nodeAppendChild n . Just) nl
        return [n]
    go (Leaf tag _ _) = withNewElement (getString tag "") (return . pure)
    go (CustomLeaf tag _) = withNewElement (fromChoiceString tag "") (return . pure)
    go (AddAttribute key _ value h) = do
      nl <- go h
      mapM_ (\x -> elementSetAttribute (castToElement x) (getString key "") (fromChoiceString value "")) nl
      return nl
    go (AddCustomAttribute key value h) = do
      nl <- go h
      mapM_ (\x -> elementSetAttribute (castToElement x) (fromChoiceString key "") (fromChoiceString value "")) nl
      return nl
    go (Content content) = do
      mn <- documentCreateTextNode doc $ fromChoiceString content ""
      case mn of 
        Just n -> return [castToNode n]
        Nothing -> return []
    go (Append h1 h2) = (++) <$> go h1 <*> go h2
    go Empty = return []
