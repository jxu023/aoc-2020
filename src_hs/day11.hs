import qualified Data.Array as Array
import Data.Maybe (mapMaybe)
-- import Debug.Trace (trace)

type Array = Array.Array

data Cell = Floor | EmptySeat | FilledSeat | Wall
          deriving (Eq)

instance Show Cell where
    show Floor = "."
    show EmptySeat = "L"
    show FilledSeat = "#"

type Coord = (Int, Int)
type Seats = Array (Int, Int) Cell

showSeats :: Seats -> String
showSeats seats =
    let (_, (maxr, maxc)) = Array.bounds seats
        in [0..maxr] >>= \r -> ("\n" ++)
                $ [0..maxc] >>= \c -> show $ seats Array.! (r, c)

parseSeats :: [String] -> Seats
parseSeats rows =
    let m = length rows
        n = length (head rows)
        toSeat '.' = Floor
        toSeat 'L' = EmptySeat
        toSeat '#' = FilledSeat
        toSeat v = error ("invalid cell " ++ show v)
        in Array.listArray ((0, 0), (m-1, n-1)) (map toSeat $ concat rows)

addCoord :: Coord -> Coord -> Coord
addCoord (a, b) (c, d) = (a+c, b+d)

dirs :: [Coord]
dirs = zip [0, 1, 1, 1, 0, -1, -1, -1] [1, 1, 0, -1, -1, -1, 0, 1]

inBounds seats = Array.inRange (Array.bounds seats)

neighbors :: Seats -> (Int, Int) -> [Cell]
neighbors seats coord = map (seats Array.!)
    . filter (inBounds seats)
    $ map (addCoord coord) dirs

takeFirst :: (a -> Bool) -> [a] -> a
takeFirst pred = head . filter pred

neighborsSeen :: Seats -> (Int, Int) -> [Cell]
neighborsSeen seats coord =
    let see dir = (\c -> if inBounds seats c then seats Array.! c else Wall)
            . takeFirst (\c -> inBounds seats c && seats Array.! c /= Floor
                                   || not (inBounds seats c))
            . tail $ iterate (addCoord dir) coord
        in map see dirs

step :: (Seats -> Coord -> [Cell]) -> Seats -> Seats
step getNeighs seats =
    let neighs = getNeighs seats
        flipSeat (coord, v) | v == EmptySeat && all (/= FilledSeat) (neighs coord)
                                = Just (coord, FilledSeat)
                            | v == FilledSeat && length (filter (== FilledSeat) (neighs coord)) >= 5
                                = Just (coord, EmptySeat)
                            | otherwise = Nothing
        in seats Array.// mapMaybe flipSeat (Array.assocs seats)

stepUntilStatic :: Seats -> (Seats -> Coord -> [Cell]) -> Seats
stepUntilStatic seats getNeighs = let seats' = step getNeighs seats
                                      in if seats' == seats then seats
                                                            else stepUntilStatic seats' getNeighs

numOccupiedStaticSeats :: Seats -> (Seats -> Coord -> [Cell]) -> Int
numOccupiedStaticSeats seats getNeighs =
    let static = stepUntilStatic seats getNeighs
        in length . filter (== FilledSeat) $ Array.elems static

main :: IO ()
main = do
    print "hi"
    let parse fileName = readFile fileName >>= (return . parseSeats . lines)
    example <- parse "../example.input"
    batch <- parse "../batch.input"

    print $ numOccupiedStaticSeats example neighbors
    print $ numOccupiedStaticSeats batch neighbors

    putStrLn $ showSeats example
    putStrLn . showSeats $ step neighborsSeen example
    putStrLn . showSeats . step neighborsSeen $ step neighborsSeen example
    print $ numOccupiedStaticSeats example neighborsSeen
    print $ numOccupiedStaticSeats batch neighborsSeen
    print "bye"
