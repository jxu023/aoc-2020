import Data.Foldable (foldl')
import Debug.Trace
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Control.Monad (foldM)

-- a rule is either a terminal String or a list of matches where each match is an ordering of rules
data Rule = Terminal String | Node [[Int]]
          deriving (Show)

type Rules = Map.Map Int Rule

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c = filter (not . null) . uncurry (:) . foldr f ([], [])
    where f d (cur, res) = if c == d then ([], cur:res)
                                     else (d:cur, res)

afterPrefix :: String -> String -> Maybe String
afterPrefix a abc = if a == take n abc then Just (drop n abc)
                                       else Nothing
    where n = length a

-- top down parsing
-- given rules and a string to match, it'll return a list of remainders
-- where the remainders are the suffixes left after it matched on the prefixes
--
-- this function assumes that matches are un-ambiguous
matchString :: Rules -> String -> Bool
matchString rules = any null . match 0
    where match :: Int -> String -> Maybe String
          match _root [] = Nothing
          match root str = case rules Map.! root of
              Terminal s -> afterPrefix s str
              Node children -> if null matches then Nothing
                                               else Just $ head matches
                  where matches = mapMaybe (matchOrd str) children 
                        matchOrd :: String -> [Int] -> Maybe String
                        matchOrd s [] = Just s
                        matchOrd s (x:xs) = match x s >>= flip matchOrd xs
                        
parseRules :: [String] -> Rules
parseRules = Map.fromList . map parseLine
    where parseLine :: String -> (Int, Rule)
          parseLine line = (ind, rule)
              where [indStr, ruleStr] = splitOn ':' line
                    ind = read indStr
                    rule = if '"' `elem` ruleStr then Terminal (filter (\c -> c /= '"' && c /= ' ') ruleStr)
                                                 else Node (parseChildren ruleStr)
                    parseChildren = map (map read) . splitOn "|" . words

-- getMatches :: String -> [String]
getMatches f contents = filter (matchString rules) wordLines
    where [ruleLines, wordLines] = splitOn [] . lines $ contents
          rules = f $ parseRules ruleLines

solveP1 :: String -> Int
solveP1 = length . getMatches id

fixRules :: Rules -> Rules
fixRules = Map.insert 8 (Node [[42], [42, 8]]) . Map.insert 11 (Node [[42, 31], [42, 11, 31]])

solveP2 = length . getMatches fixRules

expectEq :: (Eq a, Show a) => a -> a -> String -> IO ()
expectEq a b test = do
    if a /= b then putStrLn $ "FAILED " ++ test++ "\nresult: " ++ show a ++ "\nexpected: " ++ show b
              else putStrLn $ "PASS " ++ test
    putStrLn ""

runUnitTests :: IO ()
runUnitTests = do
    expectEq (matchString (Map.fromList [(0, Terminal "abc")]) "abc") True "one terminal"
    expectEq (matchString (Map.fromList [(0, Node [[1, 2]]), (1, Terminal "a"), (2, Terminal "bc")]) "abc") True "one node"

main :: IO ()
main = do
    putStrLn "hi"
    runUnitTests

    example <- readFile "../example.input"
    expectEq (Set.fromList $ getMatches id example) (Set.fromList ["ababbb", "abbbab"]) "example getMatches"
    expectEq (solveP1 example) 2 "solve p1 example"

    batch <- readFile "../batch.input"
    putStrLn $ "p1 has answer " ++ show (solveP1 batch)

    example2 <- readFile "../example2.input"
    expectEq (solveP2 example2) 12 "solve p2 example"
    putStrLn "bye"
