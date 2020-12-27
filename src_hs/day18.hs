import qualified Data.Map as Map
import Data.Foldable (foldl')
import Debug.Trace
import qualified Data.Sequence as Seq
import Data.Char (isDigit)
import Data.Maybe (fromJust)

type Map = Map.Map

expectEq :: (Eq a, Show a) => a -> a -> String -> IO ()
expectEq a b test = do
    if a /= b then putStrLn $ "FAILED " ++ test++ "\nresult: " ++ show a ++ "\nexpected: " ++ show b
              else putStrLn $ "PASS " ++ test
    putStrLn ""

data MathState = MathState { msStk :: Seq.Seq (Int -> Int)
                           , msCur :: Maybe Int
                           , msStr :: String
                           , msOp :: Int -> Int -> Int
                           }

evalExpression :: MathState -> String -> Int
evalExpression ms [] = fromJust $ msCur ms
evalExpression ms (x:xs) = evalExpression ms' xs
    where ms' | x == '*' || x == '+' = ms { msOp = if x == '*' then (*) else (+) }
              | x == ' ' = ms
              | isDigit x = if nextIsDigit then ms { msStr = str' }
                                           else msNewCur
              | x == '(' = pushCur
              | x == ')' = popToCur
              | otherwise = error $ "unexpected char " ++ show x
          nextIsDigit = null xs || isDigit (head xs)
          str' = x : msStr ms
          cur' = read str' :: Int
          msNewCur = ms { msCur = Just (maybe cur' (msOp ms cur') (msCur ms))
                        , msStr = ""
                        }
          pushCur = ms { msCur = Nothing
                       , msStk = msStk ms Seq.|> msOp ms (fromJust (msCur ms))
                       }
          popToCur = ms { msStk = Seq.drop 1 (msStk ms)
                        , msCur = Just $ Seq.index (msStk ms) 0 (fromJust (msCur ms))
                        }

-- evaluate an expression of numbers, *, +, and () to return its result
solveP1 :: String -> Int
solveP1 = evalExpression (MathState Seq.empty (Just 0) "" (+))

main :: IO()
main = do
    putStrLn "hi"
    expectEq (solveP1 "1 + 2 * 3 + 4 * 5 + 6") 71 "no parens"
    expectEq (solveP1 "1 + (2 * 3) + (4 * (5 + 6))") 51 "w/ parens"
    expectEq (solveP1 "2 * 3 + (4 * 5)") 26 "example3"
    expectEq (solveP1 "5 + (8 * 3 + 9 + 3 * 4 * 3)") 437 "example4"
    expectEq (solveP1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") 12240 "example5"
    expectEq (solveP1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 ") 13632 "example6"

    batch <- readFile "../batch.input"
    putStrLn "bye"
