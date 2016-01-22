{-# LANGUAGE TemplateHaskell #-}

module Lib.Template
    ( openSslTemplate
      , openSslSanTemplate
      , getOpenSslConfig
      , genOpenSslConfig
    ) where

import           Data.FileEmbed
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import           Data.Text.Template
import           System.FilePath (takeDirectory)
import           Data.Maybe (fromMaybe)

openSslTemplate :: Text
openSslTemplate = Text.decodeUtf8 $(embedFile "openssl.cnf")

openSslSanTemplate :: Text
openSslSanTemplate = Text.decodeUtf8 $(embedFile "openssl-san.cnf")

context :: [(Text, Text)] -> Context
context assocs x = fromMaybe err . lookup x $ assocs
  where err = error $ "Could not find key: " ++ show x

getOpenSslConfig :: Text -> FilePath -> Text
getOpenSslConfig tpl path =
    let tt =
            (LazyText.toStrict . substitute tpl $ context [("dir", Text.pack path)])
    in tt

genOpenSslConfig :: Text -> FilePath -> IO ()
genOpenSslConfig tpl path = do
    _ <- Text.writeFile path (getOpenSslConfig tpl (takeDirectory path))
    return ()

