import Data.Foldable (foldl')
import Debug.Trace (trace)

type Coord = (Int, Int)
opCoord f (a, b) (c, d) = (f a c, f b d)
addCoord = opCoord (+)
multCoord d (a, b) = (d*a, d*b)

--      North  East   South   West
dirs = [(0,1), (1,0), (0,-1), (-1,0)]
data Position = Position { pCoord :: Coord
                         , pDir :: Int -- index into dirs
                         } deriving (Show)
initPos = Position (0, 0) 1

dir cmd = case cmd of 'N' -> (0, 1)
                      'E' -> (1, 0)
                      'S' -> (0, -1)
                      'W' -> (-1, 0)

act :: Position -> (Char, Int) -> Position
act pos (cmd, dist)
    | cmd `elem` "NESW" = pos { pCoord = addCoord (pCoord pos) (multCoord dist (dir cmd)) }
    | cmd == 'F' = pos { pCoord = addCoord (pCoord pos) (multCoord dist (dirs !! pDir pos)) }
    | otherwise = pos { pDir = (pDir pos + (dist `div` 90) * sign) `mod` 4 }
        where sign = case cmd of 'L' -> -1
                                 'R' -> 1
                                 c   -> error ("err:" ++ show c)

steerFerry :: String -> Position
steerFerry contents =
    let acts = map parseAct $ lines contents
        parseAct (c:str) = (c, read str :: Int)
        in foldl' (\pos cmd -> trace (show pos) act pos cmd) initPos acts

main :: IO ()
main = do
    print "hi"

    example <- readFile "../example.input"
    batch <- readFile "../batch.input"

    print $ steerFerry example
    print $ steerFerry batch

    print "bye"
