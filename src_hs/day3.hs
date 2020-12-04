import qualified Data.Array as Array
import Debug.Trace (trace)

type Array = Array.Array
type Coord = (Int, Int)

addCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

hasTree :: Array Coord Bool -> Coord -> Bool
hasTree grid (row, col) =
    let numCols = (+1) . snd . snd $ Array.bounds grid
        c = mod col numCols
        in grid Array.! (row, c)
        -- in trace (show col ++ " " ++ show (row, c) ++ show (grid Array.! (row, c))) $ grid Array.! (row, c)

numTreesInPath grid start slope
    = let rows = (+1) . fst . snd $ Array.bounds grid
          line = takeWhile (\coord -> fst coord < rows) $ iterate (addCoord slope) start
          in length . filter (hasTree grid) $ line

parseGrid contents =
    let rows = lines contents
        m = length rows
        n = length (head rows)
        vals = zip [0..] rows >>= \(r, row) ->
                   zip [0..] row >>= \(c, elem) ->
                       [((r, c), elem == '#')]
        in Array.array ((0, 0), (m-1, n-1)) vals
        -- in trace (show (m, n)) $ Array.array ((0, 0), (m-1, n-1)) vals

solvePart1 contents =
    numTreesInPath (parseGrid contents) (0, 0) (1, 3)

solvePart2 contents =
    let grid = parseGrid contents
        treesFound = map (numTreesInPath grid (0, 0)) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
        in foldr (*) 1 treesFound
        -- in trace (show treesFound) $ foldr (*) 1 treesFound

main :: IO()
main = do
    exampleContents <- readFile "../example.input"
    print $ solvePart1 exampleContents
    print $ solvePart2 exampleContents

    problemContents <- readFile "../problem.input"
    print $ solvePart1 problemContents
    print $ solvePart2 problemContents
