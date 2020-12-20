import qualified Data.Map as Map
import Data.Char (isDigit)
import Data.Bits (setBit, clearBit)
import Data.Foldable (foldl')
import Data.List (isPrefixOf)
import Debug.Trace (trace)

type Map = Map.Map
-- TODO is there a better mask representation?
-- yes, just 2 nums, 1 for "or", another for "and complement"
-- we need 2 64 bit unsigned nums, but it doesn't really seem like we have one.
-- But, Integer (unlimited) should work just as well here.
type Mask = [(Int, Bool)]

-- for ea nonX char, i will get an index and a 0|1
parseMask :: String -> Mask
parseMask = foldl' f [] . zip [35,34..0] . unwords . drop 2 . words
    where f res (i, c) | c == 'X' = res
                       | c == '1' = (i, True):res
                       | c == '0' = (i, False):res

nonNumToSpace :: String -> String
nonNumToSpace = map toSpace
    where toSpace c | isDigit c = c
                    | otherwise = ' '

-- at location store value up to 2^36-1
parseMem :: String -> (Int, Integer)
parseMem line = (read num1, read num2)
    where [num1, num2] = words (nonNumToSpace line)

data Memory = Memory { mMask :: Mask
                     , mMem :: Map Int Integer
                     } deriving (Show)

applyMask :: Integer -> Mask -> Integer
applyMask = foldl' setVal
    where setVal num (ind, set) | set = setBit num ind
                                | otherwise = clearBit num ind

addLine :: Memory -> String -> Memory
addLine mem@(Memory mask m) l | "mask" `isPrefixOf` l = mem { mMask = parseMask l }
                              | otherwise = mem { mMem = Map.insert ind v m }
    where (ind, val) = parseMem l
          v = applyMask val mask

solveP1 :: String -> Integer
solveP1 contents = sum . Map.elems . mMem $ mem
    where mem = foldl' addLine (Memory [] Map.empty) (lines contents)

main :: IO()
main = do
    print "hi"
    example <- readFile "../example.input"
    batch <- readFile "../batch.input"

    print $ solveP1 example
    print $ solveP1 batch
    print "bye"
