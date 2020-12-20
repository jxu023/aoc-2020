import qualified Data.Map as Map
import Data.Char (isDigit)
import Data.Bits (clearBit, setBit, (.|.), (.&.), complement)
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
parseMem :: String -> (Integer, Integer)
parseMem line = (read num1, read num2)
    where [num1, num2] = words (nonNumToSpace line)

data Memory = Memory { mMask :: Mask
                     , mMem :: Map Integer Integer
                     } deriving (Show)

addLine :: Memory -> String -> Memory
addLine mem@(Memory mask m) l | "mask" `isPrefixOf` l = mem { mMask = parseMask l }
                              | otherwise = mem { mMem = Map.insert ind v m }
    where (ind, val) = parseMem l
          v = applyMask mask val

solveP1 :: String -> Integer
solveP1 contents = sum . Map.elems . mMem $ mem
    where mem = foldl' addLine (Memory (Mask 0 0) Map.empty) (lines contents)

type FloatingMask = String

parseFloatingMask :: String -> FloatingMask
parseFloatingMask = unwords . drop 2 . words

applyFloatingMask :: FloatingMask -> Integer -> [Integer]
applyFloatingMask initMask initNum =
    let vals = zip [35,34..0] initMask
        addVal :: [Integer] -> (Integer, Char) -> [Integer]
        addVal nums (i, c) | c == '1' = map (`setBit` fromIntegral i) nums
                           | c == '0' = nums
                           | c == 'X' = concat [[setBit n (fromIntegral i), clearBit n (fromIntegral i)] | n <- nums]
                           | otherwise = error ("invalid mask char " ++ show c)
        in foldl' addVal [initNum] vals

data FloatingMem = FloatingMem { fmFloat :: FloatingMask
                               , fmMem :: Map Integer Integer
                               } deriving (Show)

addFloatingLine :: FloatingMem -> String -> FloatingMem
addFloatingLine (FloatingMem float mem) l | "mask" `isPrefixOf` l = FloatingMem (parseFloatingMask l) mem
                                          | otherwise = FloatingMem float mem'
    where mem' = foldr (`Map.insert` v) mem (applyFloatingMask float k)
          (k, v) = parseMem l

solveP2 :: String -> Integer
solveP2 contents = sum . Map.elems . fmMem $ mem
    where mem = foldl' addFloatingLine (FloatingMem "" Map.empty) (lines contents)

main :: IO()
main = do
    print "hi"
    example <- readFile "../example.input"
    batch <- readFile "../batch.input"

    print $ solveP1 example
    print $ solveP1 batch

    print $ solveP2 example
    print $ solveP2 batch
    print "bye"
