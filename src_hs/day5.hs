import Data.List (foldl')
import qualified Data.Set as Set
-- import Debug.Trace (trace)

type Set = Set.Set

data SeatRange = SeatRange { minRow :: Int
                           , maxRow :: Int
                           , minCol :: Int
                           , maxCol :: Int
                           } deriving (Show)

mid l r = l + ((r - l) `div` 2)

decodeSeat :: String -> (Int, Int)
decodeSeat str = fromSr . foldl' f (SeatRange 0 127 0 7) $ str
    where f sr 'F' = sr { maxRow = mid (minRow sr) (maxRow sr) }
          f sr 'B' = sr { minRow = 1 + mid (minRow sr) (maxRow sr) }
          f sr 'L' = sr { maxCol = mid (minCol sr) (maxCol sr) }
          f sr 'R' = sr { minCol = 1 + mid (minCol sr) (maxCol sr) }
          fromSr (SeatRange a b c d) = (a, c)

seatId :: (Int, Int) -> Int
seatId (row, col) = 8 * row + col

highest :: [Int] -> Int
highest xs = foldr max (head xs) xs

main :: IO()
main = do
    example <- readFile "../example.input"
    print . map decodeSeat $ lines example
    batch <- readFile "../batch.input"
    let filledSeats = map decodeSeat $ lines batch
    print . highest $ map seatId filledSeats

    let allSeats = [(r, c) | r <- [0..127], c <- [0..7]]
    let remainingSeats = Set.difference (Set.fromList allSeats) (Set.fromList filledSeats)

    let filledSeatIds = Set.fromList $ map seatId filledSeats
    print . seatId . head . Set.toList
        $ Set.filter (\seat -> let id = seatId seat
                                   in Set.member (id+1) filledSeatIds && Set.member (id-1) filledSeatIds)
                     remainingSeats
