{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Server qualified as Test
import Common
import qualified Data.Map as M 

main :: IO ()
main = runTests 
  [prob
  ]

prob :: Score -> TestTree
prob sc = testGroup "TestTree"
  [
  ]
