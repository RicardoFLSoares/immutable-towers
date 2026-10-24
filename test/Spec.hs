module Main (main) where

import Test.HUnit

import Tarefa1Spec (testesTarefa1)
import Tarefa2Spec (testesTarefa2)
import Tarefa3Spec (testesTarefa3)

testSuite :: Test
testSuite = TestList
  [ TestLabel "Tarefa 1" testesTarefa1,
    TestLabel "Tarefa 2" testesTarefa2,
    TestLabel "Tarefa 3" testesTarefa3
  ]


main :: IO ()
main = runTestTTAndExit testSuite
