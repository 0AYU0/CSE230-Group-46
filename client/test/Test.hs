{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Client qualified as Test
import Common
import qualified Data.Map as M 

main :: IO ()
main = runTests 
  [prob
  ]

prob :: Score -> TestTree
prob sc = testGroup "TestTree"
  [
    scoreProp sc ("prop_checkWordLetters", Test.prop_checkWordLetters, 3),
    scoreProp sc ("prop_checkMoreWords", Test.prop_checkMoreWords, 3)
  ]
