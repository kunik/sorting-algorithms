module Sorting.Tests
( 
    runAll,
    sorting_functions
) where  

import qualified Sorting.QuickSort as QS

data SortFn = SortFn String ([Integer] -> [Integer])
data TestCase = TestCase String [Integer] [Integer] deriving Show
data TestResult = TestResult String String Bool deriving Show

sorting_functions :: [SortFn]
sorting_functions = [
        SortFn "Quick Sort" QS.sort
    ]


tests :: [TestCase]
tests = [
        TestCase "Empty list" [] []
        --TestCase "Empty list" [1] [1]
    ]

runTest :: SortFn -> TestCase -> TestResult
runTest (SortFn fnName fn) (TestCase testName input output) = TestResult fnName testName (output == fn input)

runTests :: SortFn -> [TestCase] -> [TestResult]
runTests fn = foldr (\test results -> (runTest fn test):results) []

runAll' :: [TestCase] -> [SortFn] -> [[TestResult]]
runAll' tests = foldr (\fn results -> (runTests fn tests):results) []

runAll = runAll' tests
