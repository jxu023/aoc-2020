import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict

type Map = Map.Map
type Set = Set.Set

splitOnCond :: (a -> Bool) -> [a] -> [[a]]
splitOnCond pred = filter (not . null) . uncurry (:) . foldr f ([], [])
    where f c (w, ws) = if not (pred c) then (c:w, ws)
                                        else ([], w:ws)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = splitOnCond (== x)

splitOnWord :: String -> String -> [String]
splitOnWord x = map unwords . splitOn x . words

type Bag = String
type Rules = Map Bag (Map Bag Int)

addBag :: Bag -> (Int, Bag) -> Rules -> Rules
addBag from (num, to) rules = let val = Map.findWithDefault Map.empty from rules
                                  in Map.insert from (Map.insert to num val) rules

parseToBag :: String -> (Int, Bag)
parseToBag str = let [numStr, adj, color, _] = words str
                     in (read numStr :: Int, unwords [adj, color])

parseRule :: String -> Rules -> Rules
parseRule line rules =
    let [fromStr, toStr] = splitOnWord "contain" line
        from = unwords . init . words $ fromStr
        tos = map parseToBag . splitOnCond (\c -> c == '.' || c == ',') $ toStr
        in foldr (addBag from) rules tos

parseRules :: String -> Rules
parseRules = foldr parseRule Map.empty . lines

-- state consists of set of visited bags
-- result is set of bags containing target bag
type VisitedState = State (Set Bag) (Set Bag)

visitChildren :: Bag -> Rules -> [Bag] -> Bag -> VisitedState
visitChildren target rules path root = do
    undefined

-- if visited then return Set.Empty
-- if Set.member root vbt then return Path including set
-- do i even need stat here? why can't i just fold over visited
visit :: Bag -> Rules -> [Bag] -> Set Bag -> Bag -> VisitedState
visit target rules path vbt root = do
    visited <- get
    if 
    if root == target || Set.member root vbt
        then do
            return . Set.fromList . takeWhile (not . flip Set.member vbt) $ path
        else do
            vbs <- get
            put $ Set.insert root vbs
            if not (Map.member root rules) then return Set.empty
                                           else foldM (visit target rules (root:path) vbt 
                                                      foldM
                                           -- else visitChildren target rules path root

-- visit each key of rules if not visited already
visitAll :: Bag -> Rules -> VisitedState
visitAll bag rules = foldM addChildren Set.empty (Map.keys rules)
    where addChildren res from = do
          vbs <- get
          if Set.member from vbs then return Set.empty
                                 else visit bag rules [] res from

dfs :: Bag -> Rules -> Set Bag
dfs target rules = evalState (visitAll target rules) Set.empty

numBagsContaining :: Bag -> String -> Int
numBagsContaining bag contents = Set.size $ dfs bag (parseRules contents)

main :: IO()
main = do
    print "hi"

    example <- readFile "../example.input"
    print $ numBagsContaining "shiny gold" example

    batch <- readFile "../batch.input"
    print $ numBagsContaining "shiny gold" batch

    print "bye"
