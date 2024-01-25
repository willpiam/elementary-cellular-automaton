module Main where

import Numeric -- (showIntAtBase)
import Data.Char -- (intToDigit)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString.Char8 as BC

getCurrentTimeInMs :: IO Integer
getCurrentTimeInMs = do
  now <- getPOSIXTime
  return $ round (now * 1000)

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

ensureLengthThree :: BC.ByteString -> BC.ByteString -> BC.ByteString
ensureLengthThree s making
 | BC.length s == 3 = s
 | BC.null making = BC.append (BC.pack "0") s
 | otherwise = BC.append s (padZeros (3 - BC.length s))

padGen :: BC.ByteString -> Int -> BC.ByteString
padGen gen padTo = do
  let missingZeros = padTo - (BC.length gen `quot` 2)
  let zeros = padZeros missingZeros
  BC.concat [zeros, gen, zeros]

generateLine :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Int -> Int -> BC.ByteString
generateLine prev rule making limit initialConditionLength 
  | BC.length making == BC.length prev = padGen making (limit - 1 + initialConditionLength)
  | otherwise = do
      let substr = BC.drop (BC.length making - 1) (BC.take ((BC.length making - 1)+3) prev) -- get substring of previous state
      let psubstr = ensureLengthThree substr making -- pad if needed to ensure length of three
      let newmaking = BC.append making (BC.singleton $ calculateCell psubstr rule)
      generateLine prev rule newmaking limit initialConditionLength

generate :: BC.ByteString -> BC.ByteString -> Int -> Int -> BC.ByteString -> Int -> BC.ByteString
generate prev rule count limit state initialConditionLength 
  | count >= limit = state
  | otherwise = do
    let thisLine = generateLine prev rule BC.empty limit initialConditionLength
    generate thisLine rule (count + 1) limit (BC.append state (BC.append (BC.pack "\n") thisLine)) initialConditionLength

main :: IO ()
main = do
  time1 <- getCurrentTimeInMs
  contents <- BC.readFile "input.txt"
  let [sRule, incon, slines] = BC.lines contents -- how can we make this more robust?

  let rule = read (BC.unpack sRule) :: Int
  let initialLength = BC.length incon
  let nlines = read (BC.unpack slines) :: Int

  let initialConditions = padGen incon (nlines + initialLength -1)

  BC.putStrLn (BC.append (BC.pack "Rule ") (BC.append (BC.pack $ show rule) (BC.append (BC.pack " is \"") (binaryString rule))))

  let lines = generate initialConditions (binaryString rule) 0 (nlines -1) initialConditions initialLength

  let fprefix = BC.concat [BC.pack "results/r", BC.pack $ show rule, BC.pack "_g", slines, BC.pack "_i", incon, BC.pack "_haskell_B"]

  -- WRITE TO FILE SYSTEM AS IMAGE
  let pbmText = BC.concat [BC.pack "P1\n", BC.pack $ show (BC.length initialConditions), BC.pack " ", BC.pack $ show nlines, BC.pack "\n", lines, BC.pack "\n"]

  BC.writeFile (BC.unpack (BC.append fprefix (BC.pack ".pbm"))) pbmText
  time2 <- getCurrentTimeInMs

  let timeDelta = time2 - time1
  BC.putStrLn (BC.concat [BC.pack "Time taken: ", BC.pack $ show timeDelta, BC.pack "ms"])
