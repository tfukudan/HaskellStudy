import Test.HUnit

main :: IO ()
main = do
    runTestTT $ "Test1" ~: 1 + 1 ~?=2
    return ()
