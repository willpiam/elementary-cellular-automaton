module Main where

import Numeric -- (showIntAtBase)
import Data.Char -- (intToDigit)
import qualified Data.ByteString.Char8 as BC
import GHCi.Run (run)

calculateCell :: BC.ByteString -> BC.ByteString -> Char
calculateCell pState rule =
  case BC.unpack pState of
    "111" -> BC.head rule
    "110" -> rule `BC.index` 1
    "101" -> rule `BC.index` 2
    "100" -> rule `BC.index` 3
    "011" -> rule `BC.index` 4
    "010" -> rule `BC.index` 5
    "001" -> rule `BC.index` 6
    "000" -> rule `BC.index` 7
    
padZeros :: Int -> BC.ByteString 
padZeros n = BC.concat [BC.pack "0" | _ <- [1..n]]

binaryString :: Int -> BC.ByteString
binaryString x = do
  let bs = BC.pack $ showIntAtBase 2 intToDigit x ""
  if BC.length bs >= 8 then bs
    else do
      let missingZeros = 8 - BC.length bs
      BC.append (padZeros missingZeros) bs

ensureLengthThree :: BC.ByteString -> BC.ByteString
ensureLengthThree s 
 | BC.length s == 3 = s
 | otherwise = BC.append s (padZeros (3 - BC.length s))

padGen :: BC.ByteString -> Int -> BC.ByteString
padGen gen padTo = do
  let missingZeros = padTo - (BC.length gen `quot` 2)
  let zeros = padZeros missingZeros
  BC.concat [zeros, gen, zeros]

generateLine :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Int -> Int -> BC.ByteString
generateLine previousLine rule currentLine numberOfGenerations initialConditionLength 
--   | BC.length currentLine == BC.length previousLine = padGen currentLine (numberOfGenerations - 1 + initialConditionLength)
  | BC.length currentLine == BC.length previousLine = padGen currentLine (numberOfGenerations  + initialConditionLength)
--   | BC.length currentLine == BC.length previousLine = currentLine
  | otherwise = do
      let substr = BC.drop (BC.length currentLine - 1) (BC.take ((BC.length currentLine - 1)+3) previousLine) -- get substring of previous state
      let psubstr = ensureLengthThree substr-- pad if needed to ensure length of three
      let calulatedCell = calculateCell psubstr rule
      let currentLineExtended = BC.append currentLine (BC.singleton calulatedCell)
      generateLine previousLine rule currentLineExtended numberOfGenerations initialConditionLength

generate :: BC.ByteString -> BC.ByteString -> Int -> Int -> BC.ByteString -> Int -> BC.ByteString
generate previousLine rule generationCounter numberOfGenerations cellularAutomaton initialConditionLength 
  | generationCounter >= numberOfGenerations = cellularAutomaton
  | otherwise = do
    let thisLine = generateLine previousLine rule BC.empty numberOfGenerations initialConditionLength
    generate thisLine rule (generationCounter + 1) numberOfGenerations (BC.append cellularAutomaton (BC.append (BC.pack "\n") thisLine)) initialConditionLength

runCellularAutomation :: Int -> Int -> BC.ByteString -> IO [BC.ByteString]
runCellularAutomation rule generations initialConditions = do
    return [BC.pack "a"]

main :: IO ()
main = do
  contents <- BC.readFile "input.txt"
  let [sRule, initialConditionsRaw, slines] = BC.lines contents 

  let rule = read (BC.unpack sRule) :: Int
  let initialLength = BC.length initialConditionsRaw
  let nlines = read (BC.unpack slines) :: Int
  let imageWidth = (nlines * 2) + initialLength

  cellularAutomaton <- runCellularAutomation rule nlines initialConditionsRaw

--   let initialConditions = padGen initialConditionsRaw (nlines + (initialLength `div` 2))
--   let initialConditions = initialConditionsRaw
  -- let initialConditions = padGen initialConditionsRaw (nlines + initialLength -1)

--   let lines = generate initialConditions (binaryString rule) 0 (nlines -1) initialConditions initialLength
--   let lines = generate initialConditionsRaw (binaryString rule) 0 (nlines -1) initialConditions initialLength
--   let lines = generate initialConditions (binaryString rule) 0 (nlines -1) initialConditionsRaw initialLength
--   let lines = generate initialConditions (binaryString rule) 0 (nlines -1) (BC.pack "") initialLength

--   putStrLn $ BC.unpack lines

  let fileNamePrefix = BC.concat [BC.pack "results/r", BC.pack $ show rule, BC.pack "_g", slines, BC.pack "_i", initialConditionsRaw, BC.pack "_haskell_C"]

  -- WRITE TO FILE SYSTEM AS IMAGE
--   let pbmText = BC.concat [BC.pack "P1\n", BC.pack $ show imageWidth, BC.pack " ", BC.pack $ show nlines, BC.pack "\n", lines, BC.pack "\n"]
  let pbmText = BC.concat [BC.pack "P1\n", BC.pack $ show imageWidth, BC.pack " ", BC.pack $ show nlines, BC.pack "\n", BC.pack "\n"]

  BC.writeFile (BC.unpack (BC.append fileNamePrefix (BC.pack ".pbm"))) pbmText


  