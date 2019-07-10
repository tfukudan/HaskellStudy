{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq ( hjq ) where

import Control.Error.Util (note)
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy as B
import Data.Hjq.Parser (parseJqQuery)
import Data.Hjq.Query (executeQuery)
import Data.Text as T

hjq :: ByteString -> T.Text -> Either T.Text ByteString
hjq jsonString queryString = do
    value <- note "invalid json format." $ decode jsonString
    query <- parseJqQuery queryString
    executeQuery query value >>= return . encodePretty
