import qualified Data.Map as Map
import Debug.Trace (trace)
import Data.Maybe (fromMaybe, mapMaybe)

type Map = Map.Map

-- let's try writing out some unit tests while coding

data RangeVal = Begin | End
              deriving (Show, Eq)
type Ranges = Map Int RangeVal

-- deletes entries between indices in map (non-inclusive)
deleteBetween :: Show a => Int -> Int -> Map Int a -> Map Int a
deleteBetween l r ranges = if l == r then ranges else case Map.lookupGT l ranges of
    Just (x, _) -> if x >= r then ranges
                             else deleteBetween x r (Map.delete x ranges)
    Nothing -> ranges

endpt lookPast ind val ranges = fromMaybe ind $ do
    (k, v) <- lookPast ind ranges
    if v /= val && abs (k - ind) <= 1 then fst <$> lookPast k ranges
                                      else Nothing

addRange :: Int -> Int -> Ranges -> Ranges
addRange begin end ranges =
    let begin' = endpt Map.lookupLE begin Begin ranges
        end' = endpt Map.lookupGE end End ranges
        ranges' = deleteBetween begin' end' ranges
        in Map.insert end' End $ Map.insert begin' Begin ranges'

unionRanges :: Ranges -> Ranges -> Ranges
unionRanges = Map.union

inRanges :: Int -> Ranges -> Bool
inRanges x ranges = undefined

makeRanges :: [(Int, Int)] -> Ranges
makeRanges = foldr (uncurry addRange) Map.empty

-- parsing (rules, your ticket, nearby tickets)
type Rules = Map String Ranges
type Ticket = [Int]

parseContents :: String -> (Rules, Ticket, [Ticket])
parseContents = undefined

splitOn :: Char -> String -> [String]
splitOn c = filter (not . null) . uncurry (:) . foldr f ([], [])
    where f d (cur, res) = if c == d then ([], cur:res)
                                     else (d:cur, res)

parseRules :: String -> [(String, [(Int, Int)])]
parseRules = map parseLine . lines
    where parseLine line = (name, ranges)
              where ws = words line
                    name = init $ head ws
                    ranges = map ((\[a,b] -> (read a, read b)) . splitOn '-')
                                 . (\x -> trace (show x) x)
                                 . filter (/= "or")
                                 $ tail ws

-- ticket is invalid for any possible range
invalidTicket :: Ticket -> Ranges -> Maybe Ticket
invalidTicket = undefined

-- compose the solution
solveP1 :: String -> Int
solveP1 contents =
    let (rules, _myTicket, nearbyTickets) = parseContents contents
        allRanges = foldr unionRanges Map.empty $ Map.elems rules
        in sum . concat $ mapMaybe (`invalidTicket` allRanges) nearbyTickets

expectEq :: (Eq a, Show a) => a -> a -> String -> IO ()
expectEq a b test = do
    if a /= b then putStrLn $ "FAILED " ++ test++ "\nresult: " ++ show a ++ "\nexpected: " ++ show b
              else putStrLn $ "PASS " ++ test
    putStrLn ""

runUnitTests :: IO ()
runUnitTests = do
    expectEq (deleteBetween 1 10 (makeRanges [(2, 3), (5, 7)]))
             (makeRanges [])
             "deleteBetween"
    expectEq (endpt Map.lookupLE 5 Begin (makeRanges [(2, 3)]))
             3
             "endpt"
    expectEq (addRange 31 60 (makeRanges [(1, 30), (50, 50), (52, 59), (60, 100)]))
             (makeRanges [(1, 100)])
             "merging ranges"
    expectEq (inRanges 5 (makeRanges [(1, 3), (3, 10)]))
             True
             "inRanges"
    expectEq (parseRules "foo: 1-3 or 5-7\nbar: 6-11\n")
             [ ("foo", [(1, 3), (5, 7)]) , ("bar", []) ]
             "parseRules"

main :: IO ()
main = do
    putStrLn "hi"
    runUnitTests

    example <- readFile "../example.input"
    expectEq (solveP1 example) 71 "example"

    batch <- readFile "../batch.input"
    putStrLn "bye"
