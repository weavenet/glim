-- File: test.hs

import Glim
import Test.HUnit

import Data.List (intercalate)

input1 = [0, 1, 2, 3, 9, 15, 17, 250]
output1 = [(1, 2), (2, 2), (4, 0), (8, 2), (16, 1), (32, 0), (64, 0), (128, 1)]

genCountItTCs (x,y,z) =
  let msg = (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " "
    in TestCase $ assertEqual msg x $ countIt y z

main :: IO ()
main = do
  let countTCs = map genCountItTCs [(True,  0, 0), (False, 0, 1), (False, 0, 2),
                                    (True,  1, 0), (False, 1, 1), (False, 1, 2),
                                    (False, 2, 0), (True,  2, 1), (False, 2, 2),
                                    (False, 3, 0), (True,  3, 1), (False, 3, 2),
                                    (False, 4, 0), (False, 4, 1), (True, 4, 2),
                                    (True, 20, 4), (True, 37, 5), (True, 101, 6)]
      basicTCs = [TestCase (assertEqual "Test process" ([(1,0),(2,1),(4,2)],0) (process "3\n4\n5\n"))
                 ,TestCase (assertEqual "Test process" ([(1,1)],0) (process "0"))
                 ,TestCase (assertEqual "Test format" "\nNo valid input processed. Total processed' 1', ignored '1'." (format ([], 1)))
                 ,TestCase (assertEqual "Test format" "\n   value  ------------- Distribution ------------- count\n       1 |                                         0\n       2 |@@@@@@@@@@@@@                            1\n       4 |@@@@@@@@@@@@@@@@@@@@@@@@@@@              2\n          ----------------------------------------\n ignored                                           0\n   total                                           3" (format ([(1,0),(2,1),(4,2)],0)))]
    in runTestTT $ TestList $ concat [countTCs, basicTCs]
  return ()
