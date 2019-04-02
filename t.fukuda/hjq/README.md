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
$ cat > test/Spec.hs <<EOF
import Test.HUnit

main :: IO ()
main = do
    runTestTT $ "Test1" ~: 1 + 1 ~?=2
    return ()
EOF
$ cat > app/Main.hs <<EOF
module Main where

main :: IO ()
main = return ()
EOF
$ stack test
```

## MEMO

### powershellとかの場合

- `chcp 65001` でUTF-8設定 or `-Encoding UTF8` などで指定
- ヒアドキュメントの代替手段
  - Here-Stringsを利用して、`Out-File -Path file.name -InputObject @'`, ..., `'@` とすること
  - TODO: 公式のドキュメントを探す。参考: https://en.wikipedia.org/wiki/Here_document#Windows_PowerShell
  - 注意: `@"`, `"@` で囲んだ場合は変数展開される
