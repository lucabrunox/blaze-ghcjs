module Text.Blaze.Html.Renderer.GHCJS
    ( renderHtml
    ) where

import Text.Blaze.Html (Html)
import Text.Blaze.Renderer.GHCJS (renderMarkup)
import GHCJS.DOM.Types

renderHtml :: IsDocument doc => doc -> Html -> IO [Node]
renderHtml = renderMarkup
