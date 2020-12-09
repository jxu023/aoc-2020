import qualified Data.Array as Array

type Array = Array.Array

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = uncurry (:) . foldr f ([], [])
    where f c (w, ws) = if c /= x then (c:w, ws)
                                  else ([], w:ws)

type Answer = Char
type Person = [Answer]
type Group = [Person]

data GroupAnswer = GroupAnswer { gaNumPeople :: Int
                               , gaAnswerCount :: Array Int Int
                               } deriving (Show)

emptyGa :: GroupAnswer
emptyGa = GroupAnswer 0 (Array.array (0, 25) [(i, 0) | i <- [0..25]])

addAnswer :: Char -> GroupAnswer -> GroupAnswer
addAnswer c ga@(GroupAnswer _ seen) =
    let ind = fromEnum c - fromEnum 'a'
        prevCount = seen Array.! ind
        in ga { gaAnswerCount = seen Array.// [(ind, prevCount + 1)] }

addPerson :: Person -> GroupAnswer -> GroupAnswer
addPerson p ga@(GroupAnswer _ _) = incrementPeople . foldr addAnswer ga $ p
    where incrementPeople x = x { gaNumPeople = (gaNumPeople x) + 1}

countAnswered :: GroupAnswer -> Int
countAnswered (GroupAnswer _ seen) = length . filter (> 0) . Array.elems $ seen

countConsensusAnswers :: GroupAnswer -> Int
countConsensusAnswers (GroupAnswer num seen) = length . filter (== num) . Array.elems $ seen

countGroup :: (GroupAnswer -> Int) -> Group -> Int
countGroup counter = counter . foldr addPerson emptyGa

parseGroups :: String -> [Group]
parseGroups = splitOn [] . lines

countGroupAnswers :: (GroupAnswer -> Int) -> String -> Int
countGroupAnswers counter = sum . map (countGroup counter) . parseGroups

main :: IO()
main = do
    example <- readFile "../example.input"
    batch <- readFile "../batch.input"

    print . countGroupAnswers countAnswered $ example
    print . countGroupAnswers countAnswered $ batch

    print . countGroupAnswers countConsensusAnswers $ example
    print . countGroupAnswers countConsensusAnswers $ batch

