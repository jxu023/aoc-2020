import Data.Foldable (foldl')
import Debug.Trace (trace)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = uncurry (:) . foldr addChar ([], [])
    where addChar c (word, res) = if c == x then ([], word:res)
                                            else (c:word, res)

parseInput :: String -> (Int, [Int])
parseInput contents =
    let [time, buses] = lines contents
        in (read time, map read . filter (/= "x") $ splitOn ',' buses)

solveP1 :: String -> Int
solveP1 contents =
    let (time, buses) = parseInput contents
        busTime bus = time + (bus - (time `mod` bus))
        busTimes = zip buses (map busTime buses)
        (earlyBus, earlyBusTime) = foldr (\a b -> if snd a < snd b then a else b) (head busTimes) (tail busTimes)
        in trace (show earlyBus ++ "|" ++ show earlyBusTime) $ earlyBus * (earlyBusTime - time)

parseP2 :: String -> [(Int, Int)]
parseP2 contents =
    let [_, buses] = lines contents
        in trace (show (splitOn ',' buses)) $ map (\(ind, str) -> (ind, read str))
            . filter ((/= "x") . snd)
            . zip [0..]
            $ splitOn ',' buses

minT :: (Int, Int) -> (Int, Int) -> (Int, Int)
minT (ox, x) (oy, y) =
    let xs = iterate (+x) ox
        pick a b = let (a', b') = (max a b, min a b)
                       in if a' `mod` b' == 0 then a else a * b
        in (head . filter (\x' -> (x' + oy) `mod` y == 0) $ xs, pick x y)

solveP2 :: String -> (Int, Int)
solveP2 contents =
    let buses = parseP2 contents
        in foldl' minT (head buses) (tail buses)

main :: IO ()
main = do
    print "hi"
    example <- readFile "../example.input"
    batch <- readFile "../batch.input"
    print $ solveP1 example
    print $ solveP1 batch

    print $ solveP2 example
    print $ solveP2 "\n17,x,13,19"
    print $ solveP2 "\n67,7,59,61"
    print $ solveP2 "\n67,x,7,59,61"
    print $ solveP2 "\n1789,37,47,1889"
    print $ solveP2 batch
    print "bye"
