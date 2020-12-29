import qualified Data.Map as Map
import Data.Foldable (foldl')
import Debug.Trace

type Map = Map.Map


-- start with a single tile t at coord 0 0
-- for each remaining tile t'
--    attempt to attach it to t, on each of the 4 sides, and with each orientation (flip)
--          8 tries specifying orientation and flip
--          note: each flip needs only to match on the border, perhaps laziness will omit the non-border rotations for you?

-- when we find a match, we need to merge the tiles
-- maintain a list of borders identified by (Coord, Side) where Coord is a length 2 vector or tuple, side is Up/Left/Down/Right

-- maintain set of borders (matched tiles), iterate over each ... remaining tile (unmatched tiles) attempting to merge it in to borders
--
-- hypothetically we don't need to model the coordinate system either
-- what if we could just view this as a graph problem with edges connecting via matched borders?
--
-- this is like an n^2 * log(n) solution where n is # of tiles


-- or how about we initialize a Map from Border to [Tile] matching that border?
--      each border has only 2 orderings (forwards, backwards)
--
--      we still need to pick one tile as "root" to initialize this Map.
--      every other tile will follow this tile's orientation
--
--      **the end result would then be flipped or rotated according to the initial state of our root tile**
--
--      ****let's initialize the map s.t. the root tile has only 4 borders
--         **** each other tile element will have 8 borders mapping to it
--
-- we could even validate that the borders are all unique
--
-- **all tiles must match on at least two borders given its arrangement into a square**
--
-- Then we could construct the arrangement from the Map
--      to do so we'd simply pick any tile as the "root", and then branch out to borders which match it
--
-- initializing the map will take O(n * log(n)) time (using balanced BST to impl map)
--
-- traversing the map while constructing the grid should take another O(n) * log(n) time
--      O(n) since one iteration for each tiles
--          log(n) for accesses to DS and for construction of set of borders
--
--  we also need to maintain an intermediate DS of list of borders to match on (root will grow as a set of borders)
--  could use state to track it.
--  
--
--  we could/should turn the map into an array at the end perhaps
--  we should also print out the final map flipped a few times
--
--  constructGrid :: Map Border [TileId] -> State [(Border, TileId)] -> Map (Int, Int) (TileId)

-- Note: Wow, writing stuff out really helps a lot with the problem solving process

expectEq :: (Eq a, Show a) => a -> a -> String -> IO ()
expectEq a b test = do
    if a /= b then putStrLn $ "FAILED " ++ test++ "\nresult: " ++ show a ++ "\nexpected: " ++ show b
              else putStrLn $ "PASS " ++ test
    putStrLn ""

main :: IO()
main = do
    putStrLn "hi"
    example <- readFile "../example.input"
    batch <- readFile "../batch.input"
    putStrLn "bye"
