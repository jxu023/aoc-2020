import System.IO (openFile, hGetContents, IOMode(ReadMode))

splitOn :: Char -> String -> [String]
splitOn c str = go [] str
    where go cur [] = [reverse cur]
          go cur (x:xs) =
              if x /= c then go (x:cur) xs
                        else (reverse cur):(go [] xs)

parseLine line =
    let range:mid:str:[] = words line
        c = head mid
        rangeBegin:rangeEnd:[] = splitOn '-' range
        minFreq = (read :: String -> Int) rangeBegin
        maxFreq = (read :: String -> Int) rangeEnd
        in (c, minFreq, maxFreq, str)

count :: Eq a => [a] -> a -> Int
count [] _ = 0
count (x:xs) c = if c == x then 1 + count xs c
                           else count xs c

isValid (c, min_freq, max_freq, str) =
    let freq = count str c
        in min_freq <= freq && freq <= max_freq

isValidLine :: String -> Bool
isValidLine = isValid . parseLine

numValidPasswords :: String -> Int
numValidPasswords = flip count True . map isValidLine . lines

isValidPosition(c, pos1, pos2, str) =
    let x = str !! (pos1 - 1)
        y = str !! (pos2 - 1)
        in 1 == (fromEnum (c == x) + fromEnum (c == y))

numValidPasswordPositions :: String -> Int
numValidPasswordPositions = flip count True . map (isValidPosition . parseLine) . lines

main :: IO()
main = do
    print "hi"

    exampleContents  <- openFile "example.input" ReadMode >>= hGetContents
    print $ numValidPasswords exampleContents

    contents  <- openFile "problem.input" ReadMode >>= hGetContents
    print $ numValidPasswords contents

    print $ numValidPasswordPositions exampleContents
    print $ numValidPasswordPositions contents

    print "bye"
