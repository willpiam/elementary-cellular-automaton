module Main where

import Numeric -- (showIntAtBase)
import Data.Char -- (intToDigit)
import Data.Time.Clock.POSIX (getPOSIXTime)

getCurrentTimeInMs :: IO Integer
getCurrentTimeInMs = do
  now <- getPOSIXTime
  return $ round (now * 1000)

calculateCell :: String -> String -> String
calculateCell pState rule =
  case pState of
    "111" -> [head rule]
    "110" -> [rule !! 1]
    "101" -> [rule !! 2]
    "100" -> [rule !! 3]
    "011" -> [rule !! 4]
    "010" -> [rule !! 5]
    "001" -> [rule !! 6]
    "000" -> [rule !! 7]

padZeros :: Int -> String 
padZeros n = concat ["0" | _ <- [1..n]]

binaryString :: Int -> String
binaryString x = do
  let bs = showIntAtBase 2 intToDigit x ""
  if length bs >= 8 then bs
    else do
      -- pad the string with zeros
      let missingZeros = 8 - length bs
      padZeros missingZeros ++ bs

ensureLengthThree :: String -> String -> String
ensureLengthThree s making
 | length s == 3 = s
 | null making = "0" ++ s
 | otherwise = s ++ padZeros (3 - length s)

padGen :: String -> Int -> String
padGen gen padTo = do
  let missingZeros = padTo - (length gen `quot` 2)
  let zeros = padZeros missingZeros
  zeros ++ gen ++ zeros

generateLine :: String -> String -> String -> Int -> Int -> String
generateLine prev rule making limit initialConditionLength 
  | length making == length prev = padGen making (limit - 1 + initialConditionLength)
  | otherwise = do
      let substr = drop (length making - 1) (take ((length making - 1)+3) prev) -- get substring of previous state
      let psubstr = ensureLengthThree substr making -- pad if needed to ensure length of three
      let newmaking = making ++ calculateCell psubstr rule
      generateLine prev rule newmaking limit initialConditionLength

generate :: String -> String -> Int -> Int -> String -> Int -> String
generate prev rule count limit state initialConditionLength 
  | count >= limit = state
  | otherwise = do
    let thisLine = generateLine prev rule "" limit initialConditionLength
    generate thisLine rule (count + 1) limit (state ++ "\n" ++ thisLine) initialConditionLength

main :: IO ()
main = do
  time1 <- getCurrentTimeInMs
  contents <- readFile "input.txt"
  let [sRule, incon, slines] = lines contents -- how can we make this more robust?

  let rule = read sRule :: Int
  let initialLength = length incon
  -- let nlines = fromIntegral (read slines :: Int)
  let nlines = read slines :: Int

  let initialConditions = padGen incon (nlines + initialLength -1)

  putStrLn ("Rule " ++ show rule ++ " is \"" ++ binaryString rule ++ "\"")

  let lines = generate initialConditions (binaryString rule) 0 (nlines -1)  initialConditions initialLength

  let fprefix = "results/r" ++ show rule ++ "_g" ++ slines ++ "_i" ++ incon ++ "_haskell"

  -- WRITE TO FILE SYSTEM AS IMAGE
  let pbmText = "P1\n" ++ show (length initialConditions) ++ " " ++ show nlines ++ "\n" ++ lines ++ "\n"

  writeFile (fprefix ++ ".pbm") pbmText
  time2 <- getCurrentTimeInMs

  let timeDelta = time2 - time1
  putStrLn ("Time taken: " ++ show timeDelta ++ "ms")
