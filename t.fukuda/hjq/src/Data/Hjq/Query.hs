{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq.Query
  ( applyFilter
  ) where

import           Control.Lens        ((^?))
import           Control.Monad       (join)
import           Data.Aeson          (Value (..))
import           Data.Aeson.Lens     (key, nth)
import qualified Data.HashMap.Strict as H
import           Data.Hjq.Parser     (JqFilter (..))
import qualified Data.Text           as T (Text, pack)
import qualified Data.Vector         as V

applyFilter :: JqFilter -> Value -> Either T.Text Value
applyFilter (JqField fieldName n) obj@(Object _) =
  join $ noteNotFoundError fieldName (applyFilter n <$> obj ^? key fieldName)
applyFilter (JqIndex index n) array@(Array _) =
  join $ noteOutOfRangeError index (applyFilter n <$> array ^? nth index)
applyFilter JqNil v = Right v
applyFilter f o = Left $ "unexpected pattern : " <> tshow f <> " : " <> tshow o

noteNotFoundError :: T.Text -> Maybe a -> Either T.Text a
noteNotFoundError _ (Just x) = Right x
noteNotFoundError s Nothing  = Left $ "field name not found" <> s

noteOutOfRangeError :: Int -> Maybe a -> Either T.Text a
noteOutOfRangeError _ (Just x) = Right x
noteOutOfRangeError s Nothing  = Left $ "out of range : " <> tshow s

tshow :: Show a => a -> T.Text
tshow = T.pack . show
