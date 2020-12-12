import qualified Data.Map.Strict as Map
import Data.List (scanl')
import qualified Data.Sequence as Seq

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

main :: IO ()
main = do
    print "hi"

    example <- readFile "../example.input"
    batch <- readFile "../batch.input"

    print $ firstNonPreamble2Sum 5 (parseNums example)
    print $ firstNonPreamble2Sum 25 (parseNums batch)

    print "bye"
