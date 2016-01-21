{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( openSslTemplate
      , openSslSanTemplate
    ) where

import Data.ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.FileEmbed

openSslTemplate :: Text
openSslTemplate = Text.decodeUtf8 $(embedFile "openssl.cnf")

openSslSanTemplate :: Text
openSslSanTemplate = Text.decodeUtf8 $(embedFile "openssl-san.cnf")
