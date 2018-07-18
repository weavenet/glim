module Glim (countIt, format, process) where

import Data.List.Split (splitOn)
import Data.List (sort, intercalate)
import Data.Text (justifyLeft, justifyRight, pack, strip, unpack)
import Data.Char (isSpace)

version = "1.0.0"

type Input = [Int]
type Output = ([(Int, Int)], Int)

countIt :: Int -> Int -> Bool
countIt 0 0 = True
countIt 1 0 = True
countIt x v = x >= (2 ^ v) && x < 2 ^ (v + 1)

addValue :: Input -> Int -> (Int,Int)
addValue input v = 
  let c = length $ filter (\x -> countIt x v) input
    in (2 ^ v, c)

addValues :: Input -> [Int] -> [(Int,Int)]
addValues input v = map (\x -> addValue input x) v

generateValues :: Input -> Int -> Int -> [Int]
generateValues input n i = take n $ iterate (+i) 0

getLargestInputValue :: Input -> Int
getLargestInputValue input = 
  let y = sort input
    in if length y == 0 then 0 else last y

getFirstPowerGreaterThanLargestInputValuePower :: Input -> Int
getFirstPowerGreaterThanLargestInputValuePower input =
  let v = getLargestInputValue input
    in head $ filter (\y -> 2 ^ y > v) [1..100]

trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

getFirst :: [String] -> Maybe Int
getFirst [] = Nothing
getFirst (x:_) = Just (read x :: Int)

removeNothing [] = []
removeNothing (x:xs) | x == Nothing = removeNothing xs
                     | otherwise    = x : removeNothing xs

eliminate :: Maybe a -> a
eliminate (Just a) = a

parseInput i = map getFirst $ map (\x -> splitOn " " x) $ map trim $ lines i

process :: String -> Output
process input =
  let parsedInput = parseInput input
      removedIgnore = map eliminate $ removeNothing parsedInput
      ignored = length parsedInput - length removedIgnore
      y = getFirstPowerGreaterThanLargestInputValuePower removedIgnore
      v = generateValues removedIgnore y 1
    in (addValues removedIgnore v, ignored)

format :: Output -> String
format output =
  let
      totalProcessed = sum $ map (\x -> snd x) (fst output)
      totalIgnored = snd output
      total = totalProcessed + totalIgnored
    in if totalProcessed > 0 then
      let
           distribution = formatDistribution output total
           totalFormated = formatTotal totalIgnored total
         in distribution ++ totalFormated 
       else
         nothingProcessed totalIgnored total

nothingProcessed totalIgnored total =
  "\nNo valid input processed. Total processed' " ++
  show totalIgnored ++ "', ignored '" ++
  show totalIgnored ++ "'."

formatDistribution output total =
  let
      header = "\n   value  ------------- Distribution ------------- count\n"
      values = intercalate "\n" $ map (\x -> formatOutputValue x total) (fst output)
      end = "\n          ----------------------------------------"
    in header ++ values ++ end

formatTotal totalIgnored total = 
  let
      ignored = "\n ignored                                           " ++ show totalIgnored
      t = "\n   total                                           " ++ show total
    in ignored ++ t

formatOutputValue :: (Int, Int) -> Int -> String
formatOutputValue (value, count) total = 
  let percent = fromIntegral count / fromIntegral total
      num = round $ percent * 40
      arobies = intercalate "" $ take num $ repeat "@"
      ws = intercalate "" $ take (41 - num) $ repeat " "
      output = (justifyR 8 (show value)) ++ " |" ++ arobies ++ ws ++ show count
    in output

justifyR n txt = unpack $ justifyRight n ' ' $ pack txt
justifyL n txt = unpack $ justifyLeft n ' ' $ pack txt
