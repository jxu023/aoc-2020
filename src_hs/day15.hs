import qualified Data.Map as Map
import Control.Monad.State.Strict
import Data.Text (pack, unpack, splitOn)

type Map = Map.Map

-- track current turn
-- maintain map of value to turn it was said on with default 0

type Turn = Int

addNum :: Int -> State (Int, Map Int Turn) Int
addNum key = do
    (curTurn, mem) <- get
    put (curTurn + 1, Map.insert key curTurn mem)
    return $ case Map.lookup key mem of Just prevTurn -> curTurn - prevTurn
                                        Nothing       -> 0

solveP1 nums endTurn =
    let initMap = foldr (uncurry Map.insert) Map.empty (zip (init nums) [1..])
        startTurn = length nums
        endState = (!! (endTurn - startTurn)) . iterate (>>= addNum) $ return (last nums)
        in evalState endState (startTurn, initMap)

parseStartingNums :: String -> [Int]
parseStartingNums = fmap (read . unpack) . splitOn (pack ",") . pack

main :: IO()
main = do
    print "hi"
    let getData filename = readFile filename >>= return . parseStartingNums
    example <- getData "../example.input"
    batch <- getData "../batch.input"

    print $ solveP1 example 2020
    print $ solveP1 batch 2020
    print $ solveP1 batch 30000000
    print "bye"
