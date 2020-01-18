{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.HTML where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept(..), MimeRender(..))

data HTML

newtype RawHtml = RawHtml { unRaw :: ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw
