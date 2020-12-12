import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

type Set = Set.Set
type Map = Map.Map

-- TODO memoize
findChainWithCond :: (Int -> Set Int -> Bool) -> Int -> Set Int -> [[Int]]
findChainWithCond cond = findChain
    where findChain cur bag = if cond cur bag then [[cur]] else do
              let useChild val = if Set.member val bag then Just (val, Set.delete val bag)
                                                       else Nothing
              child <- concatMap (uncurry findChain) $ mapMaybe useChild [cur+1, cur+2, cur+3]
              return (cur:child)

type Freq = Map Int Int
addNum :: Int -> Freq -> Freq
addNum x freq = Map.insert x (count + 1) freq
    where count = Map.findWithDefault 0 x freq

solveP1 :: [Int] -> [Int]
solveP1 nums = do
    let numSet = Set.fromList nums
    let findChain = findChainWithCond (\_ s -> null s)
    chain <- findChain 0 (Set.insert (Set.findMax numSet + 3) numSet)
    let diffs = zipWith (-) (tail chain) chain
    let freq = foldr addNum Map.empty diffs
    let getFreq key = Map.findWithDefault 0 key freq
    return $ getFreq 1 * getFreq 3

solveP2 :: [Int] -> Int
solveP2 nums =
    let numSet = Set.fromList nums
        maxNum = Set.findMax numSet
        in length $ findChainWithCond (\val _ -> val == maxNum) 0 (Set.insert (maxNum + 3) numSet)

main :: IO ()
main = do
    print "hi"

    let getNums filename = readFile filename >>= (return . map (read :: String -> Int) . lines)

    example <- getNums "../example.input"
    batch <- getNums "../batch.input"

    print $ solveP1 example
    print . head $ solveP1 batch

    print $ solveP2 example
    print $ solveP2 batch

    print "bye"
