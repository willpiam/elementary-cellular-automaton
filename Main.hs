module Main where

import Control.Monad -- allows use of `when`
import Numeric -- (showIntAtBase)
import Data.Char -- (intToDigit)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)

-- | Get the current system time in milliseconds.
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

padZeros :: String -> Int -> String
padZeros rval targetDepth
  |  targetDepth < 1 = ""
  |  length rval == targetDepth = rval
  |  otherwise = padZeros (rval ++ "0") targetDepth

binaryString :: Integer -> String
binaryString x = do
  let bs = showIntAtBase 2 intToDigit x ""
  if length bs >= 8 then bs
    else do
      -- pad the string with zeros
      let missingZeros = 8 - length bs
      padZeros "" missingZeros ++ bs

ensureLengthThree :: String -> String -> String
ensureLengthThree s making
 | length s == 3 = s
 | null making = "0" ++ s
 | otherwise = s ++ padZeros "" (3 - length s)

padGen :: String -> Int -> String
padGen gen padTo = do
  let missingZeros = padTo - (length gen `quot` 2)
  let zeros = padZeros "" missingZeros
  zeros ++ gen ++ zeros

generateLine :: String -> String -> String -> Int -> Int -> String
generateLine prev rule making limit initialConditionLength =
  if length making == length prev
    then
      padGen making (limit - 1 + initialConditionLength)
    else do
      -- get substring of previous state
      let substr = drop (length making - 1) (take ((length making - 1)+3) prev)
      -- make sure its 3 chars long -- pad if needed 
      let psubstr = ensureLengthThree substr making
      -- grow 'making' by adding the result of 'calculate cell'
      let newmaking = making ++ calculateCell psubstr rule
      -- recursivly call generateLine
      generateLine prev rule newmaking limit initialConditionLength

generate :: String -> String -> Int -> Int -> String -> Int -> String
generate prev rule count limit state initialConditionLength 
  | count >= limit = state
  | otherwise = do
    let thisLine = generateLine prev rule "" limit initialConditionLength
    generate thisLine rule (count + 1) limit (state ++ "\n" ++ thisLine) initialConditionLength

stringReplace :: String -> String -> String -> String
stringReplace body old new = Text.unpack (Text.replace (Text.pack old) (Text.pack new) (Text.pack body))

parseNumberWithDefault :: Integer -> String -> Integer
parseNumberWithDefault def s
  | null s = def
  | otherwise = read s :: Integer

initialConditionsOrDefault :: String -> String
initialConditionsOrDefault condition 
  | null condition = "010"
  | otherwise = condition

inputYes :: String -> Bool
inputYes input
  | null input = False
  | head input == 'y' = True
  | head input == 'Y' = True
  | otherwise = False

-- Modified main function to read from a file
main :: IO ()
main = do
  time1 <- getCurrentTimeInMs
  -- Read contents from file
  contents <- readFile "input.txt"
  let [sRule, incon, slines] = lines contents
  let rule = parseNumberWithDefault 30 sRule
  let tempInitialConditions = initialConditionsOrDefault incon
  let initialLength = length tempInitialConditions
  let nlines = fromIntegral (parseNumberWithDefault 12 slines)

  let initialConditions = padGen tempInitialConditions (nlines + initialLength -1)

  putStrLn ("Rule " ++ show rule ++ " is \"" ++ binaryString rule ++ "\"")

  let lines = generate initialConditions (binaryString rule) 0 (nlines -1)  initialConditions initialLength

  let fprefix = "results/r" ++ show rule ++ "_g" ++ slines ++ "_i" ++ tempInitialConditions ++ "_haskell"

  -- WRITE TO FILE SYSTEM AS IMAGE
  let pbmText = "P1\n" ++ show (length initialConditions) ++ " " ++ show nlines ++ "\n" ++ lines ++ "\n"

  writeFile (fprefix ++ ".pbm") pbmText
  time2 <- getCurrentTimeInMs

  let timeDelta = time2 - time1
  putStrLn ("Time taken: " ++ show timeDelta ++ "ms")
