{-# LANGUAGE StandaloneDeriving #-}
module Main where
import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Ex4
main = defaultMain tests
tests :: [TF.Test]
tests = 
  [ testGroup "TEST Ex4" [
      testCase "Ex1 2+2=5" (2+2 @?= 5)
  ] ]

