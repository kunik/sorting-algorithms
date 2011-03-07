module Sorting.Tests
( 
    runAll,
    sorting_functions
) where  

import qualified Sorting.QuickSort as QS
import qualified Sorting.Bubble as B
import qualified Sorting.Insertion as I
import qualified Sorting.Selection as S

data SortFn = SortFn String ([Integer] -> [Integer])
data TestCase = TestCase String [Integer] [Integer] deriving Show
data TestResult = Success String String | Fail String String deriving Show

sorting_functions :: [SortFn]
sorting_functions = [
        SortFn "Quick Sort" QS.sort,
        SortFn "Bubble Sort" B.sort,
        SortFn "Insertion Sort" I.sort,
        SortFn "Selection Sort" S.sort
    ]


tests :: [TestCase]
tests = [
        TestCase "Empty list" [] [],
        TestCase "List with one item" [1] [1],
        TestCase "Sorted list" [1, 2, 3, 4, 10] [1, 2, 3, 4, 10],
        TestCase "Not sorted list" [1, 9, 5, 27, 26, 21, 15, 4] [1, 4, 5, 9, 15, 21, 26, 27]
    ]

runTest :: SortFn -> TestCase -> TestResult
runTest (SortFn fnName fn) (TestCase testName input output)
    | output == fn input = Success fnName testName
    | otherwise          = Fail fnName testName

runTests :: SortFn -> [TestCase] -> [TestResult]
runTests fn = foldr (\test results -> (runTest fn test):results) []

runAll' :: [TestCase] -> [SortFn] -> [[TestResult]]
runAll' tests = foldr (\fn results -> (runTests fn tests):results) []

runAll = runAll' tests