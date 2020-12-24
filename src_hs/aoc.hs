import qualified Data.Map as Map
import Data.Foldable (foldl')
import Debug.Trace (trace)

type Map = Map.Map

main :: IO()
main = do
    putStrLn "hi"
    example <- readFile "../example.input"
    batch <- readFile "../batch.input"
    putStrLn "bye"
