module Main where

import Control.Exception (catch)
import Control.Monad (forM_, when)
import Data.Array.MArray (newArray, writeArray)
import Data.Array.ST (runSTUArray)
import Data.Array.Unboxed (UArray, (!))
import Data.Bits ((.&.), shiftR)
import Data.Char (isDigit, isSpace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.IO (IOMode (WriteMode), hPutStrLn, stderr, withBinaryFile)

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

ioFailed :: IOError -> IO BC.ByteString
ioFailed _ = die "cannot read input.txt"

trim :: BC.ByteString -> BC.ByteString
trim bs = BC.dropWhile isSpace (BC.reverse (BC.dropWhile isSpace (BC.reverse bs)))

stripCR :: BC.ByteString -> BC.ByteString
stripCR bs
  | not (BC.null bs) && BC.last bs == '\r' = BC.init bs
  | otherwise = bs

parseInt :: BC.ByteString -> Maybe Int
parseInt raw =
  let t = trim raw
      (neg, digits) = case BC.uncons t of
        Just ('-', rest) -> (True, rest)
        _ -> (False, t)
  in if BC.null digits || not (BC.all isDigit digits)
       then Nothing
       else case reads (BC.unpack digits) of
         [(value, "")] -> Just (if neg then -value else value)
         _ -> Nothing

validSeed :: BC.ByteString -> Bool
validSeed bs = not (BC.null bs) && BC.all (\c -> c == '0' || c == '1') bs

ruleTable :: Int -> UArray Int Int
ruleTable rule = runSTUArray $ do
  arr <- newArray (0, 7) 0
  forM_ [0 .. 7] $ \i -> writeArray arr i ((rule `shiftR` i) .&. 1)
  return arr

initialRow :: Int -> Int -> BC.ByteString -> UArray Int Int
initialRow width g seed = runSTUArray $ do
  arr <- newArray (0, width - 1) 0
  forM_ [0 .. BC.length seed - 1] $ \i ->
    when (BC.index seed i == '1') $ writeArray arr (g + i) 1
  return arr

step :: UArray Int Int -> UArray Int Int -> Int -> Int -> Int -> Int -> Bool -> UArray Int Int
step rule prev width n g gen even = runSTUArray $ do
  next <- newArray (0, width - 1) 0
  let (lo, hi) =
        if even
          then (g - (gen + 1), g + n + (gen + 1))
          else (0, width)
  forM_ [lo .. hi - 1] $ \j -> do
    let left = if j > 0 then prev ! (j - 1) else 0
        center = prev ! j
        right = if j + 1 < width then prev ! (j + 1) else 0
        idx = left * 4 + center * 2 + right
    writeArray next j (rule ! idx)
  return next

rowBytes :: UArray Int Int -> Int -> BS.ByteString
rowBytes arr width = BS.pack [if arr ! j /= 0 then 49 else 48 | j <- [0 .. width - 1]]

main :: IO ()
main = do
  contents <- BC.readFile "input.txt" `catch` ioFailed
  let ls = BC.split '\n' contents
  when (length ls < 3) $ die "invalid input"
  let ruleLine = head ls
      seed = stripCR (ls !! 1)
      genLine = ls !! 2
  rule <- case parseInt ruleLine of
    Just v | v >= 0 && v <= 255 -> return v
    _ -> die "invalid rule"
  when (not (validSeed seed)) $ die "invalid initial conditions"
  generations <- case parseInt genLine of
    Just v | v >= 1 -> return v
    _ -> die "invalid generations"
  let n = BC.length seed
      g = generations
  when (toInteger g > (toInteger (maxBound :: Int) - toInteger n) `div` 2) $ die "invalid generations"
  let width = n + 2 * g
      even = (rule .&. 1) == 0
      ruleBits = ruleTable rule
      start = initialRow width g seed
      path = "results/r" ++ show rule ++ "_g" ++ show generations ++ "_i" ++ BC.unpack seed ++ "_haskell.pbm"
      header = BC.pack ("P1\n" ++ show width ++ " " ++ show generations ++ "\n")
      go handle prev gen
        | gen >= g = return ()
        | otherwise = do
            BS.hPut handle (BS.snoc (rowBytes prev width) 10)
            if gen + 1 == g
              then return ()
              else go handle (step ruleBits prev width n g gen even) (gen + 1)
  createDirectoryIfMissing True "results"
  withBinaryFile path WriteMode $ \handle -> do
    BS.hPut handle header
    go handle start 0
