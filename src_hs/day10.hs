import Debug.Trace (trace)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

type Set = Set.Set
type Map = Map.Map

-- TODO how to simplify?
findChain :: Int -> Set Int -> Maybe [Int]
findChain cur bag = if null bag then Just [cur] else do
    let useChild val = if Set.member val bag then Just (val, Set.delete val bag)
                                             else Nothing
    let children = mapMaybe (uncurry findChain) $ mapMaybe useChild [cur+1, cur+2, cur+3]
    if null children then Nothing
                     else Just (cur: head children)

type Freq = Map Int Int
addNum :: Int -> Freq -> Freq
addNum x freq = Map.insert x (count + 1) freq
    where count = Map.findWithDefault 0 x freq

solveP1 :: [Int] -> Maybe Int
solveP1 nums = do
    let numSet = Set.fromList nums
    chain <- findChain 0 (Set.insert (Set.findMax numSet + 3) numSet)
    let diffs = zipWith (-) (tail chain) chain
    let freq = foldr addNum Map.empty diffs
    return $ Map.findWithDefault 0 1 freq * Map.findWithDefault 0 3 freq

main :: IO ()
main = do
    print "hi"

    let getNums filename = readFile filename >>= (return . map (read :: String -> Int) . lines)

    example <- getNums "../example.input"
    batch <- getNums "../batch.input"

    print $ solveP1 example
    print $ solveP1 batch

    print "bye"
