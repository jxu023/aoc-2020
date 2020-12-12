import qualified Data.Map.Strict as Map
import Data.List (scanl')
import qualified Data.Sequence as Seq
import Debug.Trace (trace)
import Data.Foldable (toList)

type Map = Map.Map
type Seq = Seq.Seq

has2Sum :: Int -> Preamble -> Bool
has2Sum x (Preamble p _) = any is2Sum . Map.keys $ p
    where is2Sum y = Map.member (x-y) p && (x-y) /= y

incrementFreq :: Int -> Map Int Int -> Map Int Int
incrementFreq key freq = Map.insert key val' freq
    where val' = case Map.lookup key freq of Just count -> count + 1
                                             Nothing -> 1

decrementFreq :: Int -> Map Int Int -> Map Int Int
decrementFreq = Map.update minusOne
    where minusOne val | val == 1 = Nothing
                       | otherwise = Just (val-1)

-- pCount is a poor man's multiset
-- pQueue tracks what value to remove next
data Preamble = Preamble { pCount :: Map Int Int
                         , pQueue :: Seq Int
                         } deriving (Show)
initPreamble :: [Int] -> Preamble
initPreamble xs = Preamble (foldr incrementFreq Map.empty xs)
                           (Seq.fromList xs)

updatePreamble :: Preamble -> Int -> Preamble
updatePreamble (Preamble count queue) x =
    let count' = incrementFreq x count
        queue' = queue Seq.|> x
        y = Seq.index queue 0
        in Preamble (decrementFreq y count') (Seq.drop 1 queue')

takeFirst :: (a -> Bool) -> [a] -> a
takeFirst pred = head . filter pred
    
firstNonPreamble2Sum :: Int -> [Int] -> Int
firstNonPreamble2Sum preambleLen numStream =
    let (initNums, nums) = splitAt preambleLen numStream
        preamble = initPreamble initNums
        states = scanl' updatePreamble preamble nums
        in fst . takeFirst (not . uncurry has2Sum) $ zip nums states

parseNums :: String -> [Int]
parseNums = map (read :: String -> Int) . lines

-- P2 Start --
data Range = Range { rNums :: Seq Int
                   , rTotal :: Int
                   } deriving (Show)

pushNum :: Int -> Range -> Range
pushNum x (Range nums total) = Range (nums Seq.|> x) (total + x)

popNum :: Range -> Range
popNum (Range nums total) = let x = Seq.index nums 0
                                in Range (Seq.drop 1 nums) (total - x)

goalRange :: Int -> Range -> Bool
goalRange target range = rTotal range == target && Seq.length (rNums range) > 1

findRangeSumTarget :: Int -> Range -> [Int] -> Maybe Range
findRangeSumTarget target range [] = if goalRange target range then Just range
                                                               else Nothing

findRangeSumTarget target range (x:xs)
    | goalRange target range = trace (show range ) $ Just range
    | rTotal range > target = trace (show range ) $ findRangeSumTarget target (popNum range) (x:xs)
    | otherwise = trace (show range ) $ findRangeSumTarget target (pushNum x range) xs

initRange :: Range
initRange = Range Seq.empty 0

solveP2 :: Int -> [Int] -> Int
solveP2 pl nums =
    let target = firstNonPreamble2Sum pl nums
        range = case findRangeSumTarget target initRange nums of Just r -> toList $ rNums r
                                                                 Nothing -> error "no range found"
        x = head range
        minX = foldr min x (tail range)
        maxX = foldr max x (tail range)
        in minX + maxX
-- P2 End --

main :: IO ()
main = do
    print "hi"

    example <- readFile "../example.input"
    batch <- readFile "../batch.input"

    let exampleNums = parseNums example
    let batchNums = parseNums batch

    print $ firstNonPreamble2Sum 5 exampleNums
    print $ firstNonPreamble2Sum 25 batchNums

    print $ solveP2 5 exampleNums
    print $ solveP2 25 batchNums


    print "bye"
