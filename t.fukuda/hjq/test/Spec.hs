{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import           Control.Lens        ((^?))
import           Data.Aeson          (Value (..))
import           Data.Aeson.QQ.Simple(aesonQQ)
import           Data.Aeson.Lens     (key, nth)
import qualified Data.HashMap.Strict as H
import           Data.Hjq.Parser     (JqFilter (..), JqQuery (..),
                                      parseJqFilter, parseJqQuery)
import           Data.Hjq.Query      (applyFilter, executeQuery)
import           Data.Text           (Text, unpack)
import qualified Data.Vector         as V
import           Test.HUnit

main :: IO ()
main = do
  runTestTT $ TestList [jqFilterParserTest, jqQueryParserTest, jqQueryParserSpacesTest, applyFilterTest, executeQueryTest]
  return ()

jqFilterParserTest :: Test
jqFilterParserTest =
  TestList
    [ "jqFilterParser test 1" ~: parseJqFilter "." ~?= Right JqNil
    , "jqFilterParser test 2" ~: parseJqFilter ".[0]" ~?=
      Right (JqIndex 0 JqNil)
    , "jqFilterParser test 3" ~: parseJqFilter ".fieldName" ~?=
      Right (JqField "fieldName" JqNil)
    , "jqFilterParser test 4" ~: parseJqFilter ".[0].fieldName" ~?=
      Right (JqIndex 0 (JqField "fieldName" JqNil))
    , "jqFilterParser test 5" ~: parseJqFilter ".fieldName[0]" ~?=
      Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]

jqQueryParserTest :: Test
jqQueryParserTest = TestList
  [ "jqQueryParser test 1" ~:
    parseJqQuery "[]" ~?= Right (JqQueryArray [])
  , "jqQueryParser test 2" ~:
    parseJqQuery "[.hoge,.piyo]" ~?= Right (JqQueryArray [JqQueryFilter (JqField "hoge" JqNil), JqQueryFilter (JqField "piyo" JqNil)])
  , "jqQueryParser test 3" ~:
    parseJqQuery "{\"hoge\":[],\"piyo\":[]}" ~?= Right (JqQueryObject [("hoge", JqQueryArray []), ("piyo", JqQueryArray [])])
  ]

jqQueryParserSpacesTest :: Test
jqQueryParserSpacesTest = TestList
  [ "jqQueryParser spaces test 1" ~:
    parseJqQuery " [ ] " ~?= Right (JqQueryArray [])
  , "jqQueryParser spaces test 2" ~:
    parseJqQuery " [ . hoge , . piyo ] " ~?= Right (JqQueryArray [JqQueryFilter (JqField "hoge" JqNil), JqQueryFilter (JqField "piyo" JqNil)])
  , "jqQueryParser spaces test 3" ~:
    parseJqQuery "{ \"hoge\" : [ ] , \"piyo\" :  [ ] } " ~?= Right (JqQueryObject [("hoge", JqQueryArray []), ("piyo", JqQueryArray [])])
  ]

unsafeParseFilter :: Text -> JqFilter
unsafeParseFilter t = case parseJqFilter t of
  Right f -> f
  Left s -> error $ "PARSE FAILURE IN A TEST : " ++ unpack s

applyFilterTest :: Test
applyFilterTest = TestList
  [ "applyFilter test 1" ~:
      applyFilter (unsafeParseFilter ".") testData
      ~?= Right testData
  , "applyFilter test 2" ~:
      (Just $ applyFilter (unsafeParseFilter ".string-field") testData)
      ~?= fmap Right (testData ^? key "string-field")
  , "applyFilter test 3" ~:
      (Just $ applyFilter (unsafeParseFilter ".nested-field.inner-string") testData)
      ~?= fmap Right (testData ^? key "nested-field" . key "inner-string")
  , "applyFilter test 4" ~:
      (Just $ applyFilter (unsafeParseFilter ".nested-field.inner-number") testData)
      ~?= fmap Right (testData ^? key "nested-field" . key "inner-number")
  , "applyFilter test 5" ~:
      (Just $ applyFilter (unsafeParseFilter ".array-field[0]") testData)
      ~?= fmap Right (testData ^? key "array-field" . nth 0)
  , "applyFilter test 6" ~:
      (Just $ applyFilter (unsafeParseFilter ".array-field[1]") testData)
      ~?= fmap Right (testData ^? key "array-field" . nth 1)
  , "applyFilter test 7" ~:
      (Just $ applyFilter (unsafeParseFilter ".array-field[2].object-in-array") testData)
      ~?= fmap Right (testData ^? key "array-field" . nth 2 . key "object-in-array")
  ]
  
testData :: Value
testData = [aesonQQ|
  { "string-field": "string value"
  , "nested-field":
    { "inner-string": "inner value"
    , "inner-number": 100
    }
  , "array-field":
      [ "first field"
      , "next field"
      , { "object-in-array": "string value in object-in-array" }
      ]
  }
  |]

executeQueryTest :: Test
executeQueryTest = TestList
  [ "executeQuery test 1" ~: executeQuery (unsafeParseQuery "{}") testData ~?= Right (Object $ H.fromList [])
  , "executeQuery test 2" ~:
      executeQuery (unsafeParseQuery "{ \"field1\": . , \"field2\": .string-field }") testData ~?=
      Right (Object $ H.fromList [("field1", testData), ("field2", String "string value")])
  , "executeQuery test 3" ~:
      executeQuery (unsafeParseQuery "[ .string-field, .nested-field.inner-string ]") testData ~?=
      Right (Array $ V.fromList [String "string value", String "inner value"])
  ]

unsafeParseQuery :: Text -> JqQuery
unsafeParseQuery t = case parseJqQuery t of
  Right q -> q
  Left s -> error $ "PARSE FAILURE IN A TEST : " ++ unpack s