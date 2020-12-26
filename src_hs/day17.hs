import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable (foldl')
import Debug.Trace (trace)

-- Coordinate to locate a cube in a pocket dimension
type Coord = (Int, Int, Int)
-- Set of Coords of Activated Cubes
type PocketDimension = Set.Set Coord

-- given a coord
-- return its 26 neighbor coords
neighborCoords :: Coord -> [Coord]
neighborCoords = undefined

-- counts the number of active neighbors of a coord in the pocket dimenion
numActiveNeighbors :: Coord -> PocketDimension -> Int
numActiveNeighbors = undefined

-- Given an *inactive* coord, attempt to activate it
activate :: PocketDimension -> Coord -> Coord
activate = undefined

-- Given an *active* coord, attempt to deactivate it
deactivate :: PocketDimension -> Coord -> Coord
deactivate = undefined

stepOneCycle :: PocketDimension -> PocketDimension
stepOneCycle pd = Set.fromList $ map activate activeCoords ++ map deactivate inactiveCoords
    where activeCoords = Set.elems pd :: [Coord]
          inactiveCoords = Set.toList . Set.fromList $ map neighborCoords activeCoords :: [Coord]

-- Read initial state, simulate 6 cycles, return # of active cubes
solveP1 :: String -> Int
solveP1 = undefined

main :: IO()
main = do
    putStrLn "hi"
    example <- readFile "../example.input"
    batch <- readFile "../batch.input"
    putStrLn "bye"
