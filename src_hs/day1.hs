import System.IO (openFile, hGetContents, IOMode(ReadMode))
import qualified Data.Set as Set
import qualified Data.MultiSet as MS
-- import Debug.Trace (trace)

type Set = Set.Set

find2Sum :: [Int] -> Int -> Set Int -> Maybe Int
find2Sum [] _ _
    = Nothing
find2Sum (x:xs) sum past
    = let y = sum - x
          in if Set.member y past then Just x
                                  else find2Sum xs sum (Set.insert x past)

-- Find the two numbers that sum to 2020 and multiply them
solveDay1 :: String -> Int
solveDay1 contents =
    let nums = map (read :: String -> Int) $ lines contents
        result = find2Sum nums 2020 Set.empty
        in case result of Nothing -> error "No pair sums to 2020"
                          Just x -> x * (2020 - x)

uniquePairs :: [Int] -> [(Int, Int)]
uniquePairs nums = go nums (tail nums)
    where go [] _ = []
          go xs [] = go (tail xs) (tail (tail xs))
          go (x:xs) (y:ys) = (x, y): go (x:xs) ys

find3Sum :: [Int] -> Int -> (Int, Int, Int)
find3Sum nums sum =
    let multiset = MS.fromList nums
        pairs = uniquePairs nums
        is3Sum (x, y) =
            let multiset' = MS.delete x (MS.delete y multiset)
                in MS.member (2020 - x - y) multiset'
        threeSums = filter is3Sum pairs
        in case threeSums of [] -> error "No three sums to 2020"
                             ((x, y):ps) -> (x, y, 2020 - x - y)

-- Find 3 numbers that sum to 2020 and muiltiply them
solveDay1Part2 :: String -> Int
solveDay1Part2 contents =
    let nums = map (read :: String -> Int) $ lines contents
        (x, y, z) = find3Sum nums 2020
        in x * y * z

main :: IO ()
main = do
    print "hi"

    contents  <- openFile "day1.input" ReadMode >>= hGetContents
    -- contents  <- openFile "example.input" ReadMode >>= hGetContents

    -- print $ solveDay1 contents
    print $ solveDay1Part2 contents

    print "bye"
