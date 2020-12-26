import Control.Monad.State.Strict
import Debug.Trace
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Map as Map
import Data.Bifunctor (second)
import Data.List (find, isInfixOf)
import qualified Data.Array as Array
import qualified Data.Set as Set

type Map = Map.Map

data RangeVal = Begin | End | Single
              deriving (Show, Eq)
otherVal Begin = End
otherVal End = Begin
otherVal Single = Single
type Ranges = Map Int RangeVal

-- TODO try expressing this as a takeFirst and iterate () also
-- deletes entries between indices in map (non-inclusive)
deleteBetween :: Show a => Int -> Int -> Map Int a -> Map Int a
deleteBetween l r ranges = if l == r then ranges else case Map.lookupGT l ranges of
    Just (x, _) -> if x >= r then ranges
                             else deleteBetween x r (Map.delete x ranges)
    Nothing -> ranges

-- how about initializing ranges with (-inf, End), (+inf, Begin)? would simplify the logic a bit
endpt lookTowards lookPast ind val ranges = maybe ind f (lookTowards ind ranges)
    where f (k, v) | v == otherVal val = if abs (k - ind) <= 1 then fst . fromJust $ lookPast k ranges
                                                               else ind
                   | otherwise = k 

-- if single inRange .. then do nothing, otherwise add single .. easy ..
-- if begin or end already has a single .. then just overwite it .
addRange :: Int -> Int -> Ranges -> Ranges
addRange begin end ranges =
    let begin' = endpt Map.lookupLE Map.lookupLT begin Begin ranges
        end' = endpt Map.lookupGE Map.lookupGT end End ranges
        ranges' = deleteBetween begin' end' ranges
        in Map.insert end' End $ Map.insert begin' Begin ranges'

unionRanges :: Ranges -> Ranges -> Ranges
unionRanges a b = foldr (uncurry addRange) a . halve $ Map.toList b
    where halve [] = []
          halve (a:b:lst) = (fst a, fst b) : halve lst
          halve lst = error (show lst)

-- TODO add unit tests for this ... even this was more complicated than I thought
inRanges :: Int -> Ranges -> Bool
inRanges x ranges = onRangeBorder || inRange
    where onRangeBorder = Map.member x ranges
          inRange = (snd <$> left) == Just Begin
          left = Map.lookupLT x ranges

makeRanges :: [(Int, Int)] -> Ranges
makeRanges = foldr (uncurry addRange) Map.empty

type Rules = Map String Ranges
type Ticket = [Int]

parseContents :: String -> (Rules, Ticket, [Ticket])
parseContents contents =
    let [rs, t, ts] = splitOn [] (lines contents)
        in (parseRules rs, parseTicket (head $ tail t), map parseTicket (tail ts))

parseTicket :: String -> Ticket
parseTicket = map read . splitOn ','

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c = filter (not . null) . uncurry (:) . foldr f ([], [])
    where f d (cur, res) = if c == d then ([], cur:res)
                                     else (d:cur, res)

parseRules :: [String] -> Rules
parseRules = Map.fromList . map parseLine
    where parseLine line = (name, ranges)
              where [name, rs] = splitOn ':' line
                    ranges = makeRanges
                        . map ((\[a,b] -> (read a, read b)) . splitOn '-')
                        . filter (/= "or")
                        $ words rs

-- TODO why is p1 broken now? ... add more unit tests? .. ranges should be good..
solveP1 :: String -> Int
solveP1 contents =
    let (rules, _myTicket, nearbyTickets) = parseContents contents
        allRanges = foldr unionRanges Map.empty $ Map.elems rules
        in sum . concatMap (filter (not . (`inRanges` allRanges))) $ nearbyTickets

eliminate :: String -> [[String]] -> [[String]]
eliminate str = map (addIfEmpty str . filter (/= str))
    where addIfEmpty x xs | null xs = [x]
                          | otherwise = xs

findSingle :: Set.Set String -> [[String]] -> Maybe String
findSingle elimd = fmap head . find isSingle
    where isSingle xs = case xs of [x] -> not (Set.member x elimd)
                                   _ -> False

-- TODO why is everything being eliminated?
eliminatePossibs :: [[String]] -> State (Set.Set String) [[String]]
eliminatePossibs strs = do
    elimd <- get
    case findSingle elimd strs of
        Nothing -> return strs
        Just single -> do
            put (Set.insert single elimd)
            eliminatePossibs (eliminate single strs)

deduce :: [[String]] -> [[String]]
deduce field = evalState (eliminatePossibs field) Set.empty

identifyFields :: Rules -> [Ticket] -> [String]
identifyFields rules tickets =
    let grid = Array.listArray ((0, 0), (lastTicket, lastField)) (concat tickets)
        lastTicket = length tickets - 1
        lastField = length (head tickets) - 1

        findField :: Int -> [String]
        findField col = filter allValid (Map.keys rules)
            where allValid k = all (`inRanges` (rules Map.! k)) vals
                  vals = [grid Array.! (r, col) | r <- [0..lastTicket]]
        in map concat . deduce $ map findField [0..lastField]

solveP2 :: String -> Int
solveP2 contents =
    let (rules, myTicket, nearbyTickets) = parseContents contents
        allRanges = foldr unionRanges Map.empty $ Map.elems rules
        validTickets = filter (all (`inRanges` allRanges)) nearbyTickets
        departures = map fst
            . filter (("departure" `isInfixOf`) . snd)
            . (\x -> traceShow x x)
            . zip [0..]
            $ identifyFields rules validTickets
        in product . (\x -> traceShow x x) $ map (myTicket !!) departures

expectEq :: (Eq a, Show a) => a -> a -> String -> IO ()
expectEq a b test = do
    if a /= b then putStrLn $ "FAILED " ++ test++ "\nresult: " ++ show a ++ "\nexpected: " ++ show b
              else putStrLn $ "PASS " ++ test
    putStrLn ""

runUnitTests :: IO ()
runUnitTests = do
    expectEq (endpt Map.lookupLE Map.lookupLT 5 Begin (makeRanges [(2, 4)]))
             2
             "endpt diff by 1"
    expectEq (endpt Map.lookupGE Map.lookupGT 2 End (makeRanges [(2, 5)]))
             5
             "endpt match on same"
    expectEq (endpt Map.lookupGE Map.lookupGT 4 End (makeRanges [(2, 8)]))
             8
             "endpt match end on end"
    expectEq (endpt Map.lookupGE Map.lookupGT 5 End (makeRanges [(7, 8)]))
             5
             "endpt end finds begin further away than 1"

    expectEq (makeRanges [(1, 10)])
             (Map.insert 1 Begin $ Map.insert 10 End Map.empty)
             "makeRanges w/ single range"
    expectEq (makeRanges [(1, 3), (5, 7)])
             (Map.fromList [(1, Begin), (3, End), (5, Begin), (7, End)])
             "makeRanges w/ two ranges"
    expectEq (deleteBetween 1 10 (makeRanges [(2, 3), (5, 7)]))
             (makeRanges [])
             "deleteBetween"

    -- TODO add test for singleNum ranges
    expectEq (makeRanges [(31, 60), (1, 30), (52, 59), (60, 100)])
             (makeRanges [(1, 100)])
             "merging ranges"
    expectEq (inRanges 5 (makeRanges [(1, 3), (3, 10)]))
             True
             "inRanges"
    expectEq (parseRules ["foo: 1-3 or 5-7", "bar: 6-11"])
             (Map.fromList [("foo", makeRanges [(1, 3), (5, 7)]), ("bar", makeRanges [(6, 11)])])
             "parseRules"

    expectEq (eliminate "a" [words "a b c", words "a d e f", words "a x y z", words "z"])
             [words "b c", words "d e f", words "x y z", words "z"]
             "eliminate a"
    expectEq (findSingle (Set.fromList ["a"]) [words "a b c", ["b"], ["a"], words "c d"])
             (Just "b")
             "findSingle"
    expectEq (deduce [words "a", words "a b", words "a b c"])
             [words "a", words "b", words "c"]
             "deduce"
    expectEq (deduce [["row"],["class","row"],["class","row","seat"]])
             [words "row", words "class", words "seat"]
             "deduce row class seat"

main :: IO ()
main = do
    putStrLn "hi"
    runUnitTests

    example <- readFile "../example.input"
    -- expectEq (solveP1 example) 71 "example p1"
    expectEq (solveP2 example) 1 "example p2"

    batch <- readFile "../batch.input"
    print $ solveP1 batch
    print $ solveP2 batch
    putStrLn "bye"
