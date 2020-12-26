import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable (foldl')
import Debug.Trace
import Data.Maybe (catMaybes)

-- Coordinate to locate a cube in a pocket dimension
type Coord = (Int, Int, Int)
-- Set of Coords of Activated Cubes
type PocketDimension = Set.Set Coord

-- given a coord
-- return its 26 neighbor coords
neighborCoords :: Coord -> [Coord]
neighborCoords c = filter (/= c) [addCoord c (x, y, z) | x <- diffs, y <- diffs, z <- diffs]
    where diffs = [(-1)..1]

addCoord :: Coord -> Coord -> Coord
addCoord (a,b,c) (d,e,f) = (a+d,b+e,c+f)

-- counts the number of active neighbors of a coord in the pocket dimenion
numActiveNeighbors :: Coord -> PocketDimension -> Int
numActiveNeighbors c pd = length . filter (`Set.member` pd) $ neighborCoords c

-- Given an *inactive* coord, attempt to activate it
-- return Nothing if coord stays inactive, otherwise coord
activate :: PocketDimension -> Coord -> Maybe Coord
activate pd c = if n == 3 then Just c else Nothing
    where n = numActiveNeighbors c pd

-- Given an *active* coord, attempt to deactivate it
-- return Nothing if coord becomes inactive, otherwise coord
deactivate :: PocketDimension -> Coord -> Maybe Coord
deactivate pd c = if n == 2 || n == 3 then Just c else Nothing
    where n = numActiveNeighbors c pd

stepOneCycle :: PocketDimension -> PocketDimension
stepOneCycle pd = Set.fromList . catMaybes $ map (deactivate pd) activeCoords ++ map (activate pd) inactiveCoords
    where activeCoords = Set.elems pd :: [Coord]
          inactiveCoords = Set.toList . Set.fromList $ concatMap neighborCoords activeCoords :: [Coord]

-- read a 2d grid, set (x, y, 0) to init a Set of Coords for locs w/ '#'
-- track current coordinate while iter'ing over all vals
parsePocketDimension :: String -> PocketDimension
parsePocketDimension = snd . foldl' addCoord ((0, 0, 0), Set.empty)
    where addCoord ((x, y, _), activeCoords) c | traceShow (c, (x, y)) False = undefined
                                               | c == '\n' = ((x+1, 0, 0), activeCoords)
                                               | c == '#' || c == '.' = ((x, y+1, 0), if c == '#' then Set.insert (x, y, 0) activeCoords
                                                                                                  else activeCoords)
                                               | otherwise = ((x, y, 0), activeCoords)

-- Read initial state, simulate 6 cycles, return # of active cubes
solveP1 :: String -> Int
solveP1 = Set.size . (!! 6) . iterate stepOneCycle . parsePocketDimension

expectEq :: (Eq a, Show a) => a -> a -> String -> IO ()
expectEq a b test = do
    if a /= b then putStrLn $ "FAILED " ++ test++ "\nresult: " ++ show a ++ "\nexpected: " ++ show b
              else putStrLn $ "PASS " ++ test
    putStrLn ""

runUnitTests :: IO ()
runUnitTests = do
    example <- readFile "../example.input"
    let examplePocketDimension = parsePocketDimension example
    expectEq examplePocketDimension 
             (Set.fromList [(0,1,0), (1,2,0), (2,0,0), (2,1,0), (2,2,0)])
             "parsePocketDimension example"

    expectEq (length $ neighborCoords (0, 0, 0)) 26 "neighborCoords has 26"
    expectEq (neighborCoords (0, 0, 0))
             [(-1,-1,-1),(-1,-1,0),(-1,-1,1),(-1,0,-1),(-1,0,0),(-1,0,1),(-1,1,-1),(-1,1,0),(-1,1,1),(0,-1,-1),(0,-1,0),(0,-1,1),(0,0,-1),(0,0,1),(0,1,-1),(0,1,0),(0,1,1),(1,-1,-1),(1,-1,0),(1,-1,1),(1,0,-1),(1,0,0),(1,0,1),(1,1,-1),(1,1,0),(1,1,1)]
             "neighbordCoord values"

    -- This is a wrong unit test, the lesson here is to copy paste the input and then parse it instead of transcribing coords.
    -- expectEq (stepOneCycle examplePocketDimension)
    --          (Set.fromList [(0,0,-1), (1,2,-1), (2,1,-1), (0,0,0), (0,2,0), (1,1,0), (1,2,0), (2,1,0), (0,0,1), (1,2,1), (2,1,1)])
    --          "stepOneCycle examplePocketDimension"

main :: IO()
main = do
    putStrLn "hi"
    example <- readFile "../example.input"
    batch <- readFile "../batch.input"

    runUnitTests
    expectEq (solveP1 example) 112 "p1 example"
    print $ solveP1 batch
    putStrLn "bye"
