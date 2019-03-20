# hjq

## 作業履歴 20190320

プロジェクト作成

```sh
$ stack new hjq
$ cd hjq
```

テスト準備

```sh
$ rm src/Lib.hs
$ mkdir -p src/Data/Hjq
$ echo "module Data.Hjq where" > src/Data/Hjq.hs
$ echo "module Data.Hjq.Parser where" > src/Data/Hjq/Parser.hs
$ echo "module Data.Hjq.Query where" > src/Data/Hjq/Query.hs
$ nano package.yaml # tests.dependencies へ `- HUnit` を追加
$ cat test/Spec.hs
import Test.HUnit

main :: IO ()
main = do
    runTestTT $ "Test1" ~: 1 + 1 ~?=2
    return ()
$ cat app/Main.hs
module Main where

main :: IO ()
main = return ()
$ stack test
```
