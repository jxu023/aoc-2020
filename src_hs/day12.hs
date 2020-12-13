import Data.Foldable (foldl')
import Debug.Trace (trace)

type Coord = (Int, Int)
opCoord f (a, b) (c, d) = (f a c, f b d)
addCoord = opCoord (+)
multCoord d (a, b) = (d*a, d*b)

--      North  East   South   West
data Position = Position { pShip :: Coord
                         , pWayPt :: Coord -- index into dirs
                         } deriving (Show)
initPos = Position (0, 0) (10, 1)

dir cmd = case cmd of 'N' -> (0, 1)
                      'E' -> (1, 0)
                      'S' -> (0, -1)
                      'W' -> (-1, 0)

turnRight (a, b) = (b, -a)
turnLeft (b, a) = (-a, b)

act :: Position -> (Char, Int) -> Position
act pos@(Position ship waypt) (cmd, dist)
    | cmd `elem` "NESW" = pos { pWayPt = addCoord waypt (multCoord dist (dir cmd)) }
    | cmd == 'F' = pos { pShip = addCoord ship (multCoord dist waypt) }
    | otherwise = pos { pWayPt = last . take (1 + (dist `div` 90)) $ iterate turn waypt }
          where turn | cmd == 'L' = turnLeft
                     | cmd == 'R' = turnRight
                     | otherwise = error ("unknown cmd" ++ show cmd)

steerFerry :: String -> Position
steerFerry contents =
    let acts = map parseAct $ lines contents
        parseAct (c:str) = (c, read str :: Int)
        in foldl' (\a b -> trace (show $ act a b) act a b) initPos acts

main :: IO ()
main = do
    print "hi"

    example <- readFile "../example.input"
    batch <- readFile "../batch.input"

    let val (Position (a, b) _) = abs a + abs b
    print .val $ steerFerry example
    print . val $ steerFerry batch

    print "bye"
