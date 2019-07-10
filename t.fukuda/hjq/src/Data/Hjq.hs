{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq
  ( hjq
  ) where

import           Control.Error.Util       (note)
import           Data.Aeson               (decode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.Hjq.Parser          (parseJqQuery)
import           Data.Hjq.Query           (executeQuery)
import qualified Data.Text                as T

hjq :: B.ByteString -> T.Text -> Either T.Text B.ByteString
hjq jsonString queryString = do
  value <- note "invalid json format." $ decode jsonString
  query <- parseJqQuery queryString
  result <- executeQuery query value
  return $ encodePretty result
