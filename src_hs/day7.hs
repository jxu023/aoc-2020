import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict
import Debug.Trace (trace)

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
parseToBag "no other bags" = (0, "")
parseToBag str = let [numStr, adj, color, _] = words str
                     in (read numStr :: Int, unwords [adj, color])

parseRule :: String -> Rules -> Rules
parseRule line rules =
    let [fromStr, toStr] = splitOnWord "contain" line
        from = unwords . init . words $ fromStr
        tos = filter (\(x, _) -> x > 0) . map parseToBag . splitOnCond (\c -> c == '.' || c == ',') $ toStr
        in foldr (addBag from) rules tos

parseRules :: String -> Rules
parseRules = foldr parseRule Map.empty . lines

data VisitedBags = VisitedBags { vsSeen :: Set Bag
                               , vsTarget :: Set Bag -- bags that contain the target
                               }

getChildren :: Bag -> Rules -> [(Bag, Int)]
getChildren root = Map.toList . Map.findWithDefault Map.empty root

-- TODO can I use State monad to simplify?
findTarget :: Bag -> Rules -> Set Bag
findTarget target rules = vsTarget visitAll
    where visitAll = foldr tryVisit (VisitedBags Set.empty Set.empty) (Map.keys rules)
          tryVisit from vb@(VisitedBags vss _) = if Set.member from vss then vb
                                                                        else visit [] from vb

          visit :: [Bag] -> Bag -> VisitedBags -> VisitedBags
          visit path root vb@(VisitedBags vss vst)
              | root == target || Set.member root vst = addPath path vb
              | Set.member root vss = vb
              | otherwise = let vss' = Set.insert root vss
                                children = map fst $ getChildren root rules
                                in foldr (visit (root:path)) (vb { vsSeen = vss' }) children

          addPath path vb@(VisitedBags _ vst) = vb { vsTarget = foldr Set.insert vst newOnPath }
              where newOnPath = Set.fromList . takeWhile (not . flip Set.member vst) $ path

bagsContaining :: Bag -> String -> Int
bagsContaining bag contents = Set.size $ findTarget bag (parseRules contents)

bagsContainedIn :: Bag -> String -> Int
bagsContainedIn bag contents =
    let rules = parseRules contents
        dfs :: Bag -> State (Map Bag Int) Int
        dfs root = do
            cache <- get
            case Map.lookup root cache of
                 Just val -> return val
                 Nothing -> do
                     let addChild total (child, coeff) = do { x <- dfs child; return (x*coeff + total); }
                     total <- foldM addChild 1 (getChildren root rules)
                     cache <- get
                     put (Map.insert root total cache)
                     return total
        in (+ (-1)) . evalState (dfs bag) $ Map.empty
        -- in (+ (-1)) . fst . (\x -> trace ("state: " ++ show x) x) . runState (dfs bag) $ Map.empty

main :: IO()
main = do
    print "hi"

    example <- readFile "../example.input"
    print $ bagsContaining "shiny gold" example

    batch <- readFile "../batch.input"
    print $ bagsContaining "shiny gold" batch

    print $ bagsContainedIn "shiny gold" example
    print $ bagsContainedIn "shiny gold" batch

    print "bye"
