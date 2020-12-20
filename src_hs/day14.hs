import qualified Data.Map as Map
import Data.Char (isDigit)
import Data.Bits (setBit, (.|.), (.&.), complement)
import Data.Foldable (foldl')
import Data.List (isPrefixOf)
import Debug.Trace (trace)

type Map = Map.Map

data Mask = Mask { mSet :: Integer
                 , mClear :: Integer
                 } deriving (Show)

-- for ea nonX char, i will get an index and a 0|1
parseMask :: String -> Mask
parseMask = foldl' f (Mask 0 0) . zip [35,34..0] . unwords . drop 2 . words
    where f mask (i, c) | c == 'X' = mask
                        | c == '1' = mask { mSet = setBit (mSet mask) i }
                        | c == '0' = mask { mClear = setBit (mClear mask) i}

applyMask :: Mask -> Integer -> Integer
applyMask (Mask set clear) = (.&. complement clear) . (.|. set)

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

addLine :: Memory -> String -> Memory
addLine mem@(Memory mask m) l | "mask" `isPrefixOf` l = mem { mMask = parseMask l }
                              | otherwise = mem { mMem = Map.insert ind v m }
    where (ind, val) = parseMem l
          v = applyMask mask val

solveP1 :: String -> Integer
solveP1 contents = sum . Map.elems . mMem $ mem
    where mem = foldl' addLine (Memory (Mask 0 0) Map.empty) (lines contents)

main :: IO()
main = do
    print "hi"
    example <- readFile "../example.input"
    batch <- readFile "../batch.input"

    print $ solveP1 example
    print $ solveP1 batch
    print "bye"
