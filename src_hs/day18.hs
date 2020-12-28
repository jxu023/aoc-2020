import qualified Data.Sequence as Seq
import Data.Char (isDigit)
import Debug.Trace

expectEq :: (Eq a, Show a) => a -> a -> String -> IO ()
expectEq a b test = do
    if a /= b then putStrLn $ "FAILED " ++ test++ "\nresult: " ++ show a ++ "\nexpected: " ++ show b
              else putStrLn $ "PASS " ++ test
    putStrLn ""

data MathState = MathState { msStk :: Seq.Seq (Int -> Int)
                           , msCur :: Int
                           , msStr :: String
                           , msOp :: Int -> Int -> Int
                           }

evalExpression :: MathState -> String -> MathState
evalExpression ms [] = ms
evalExpression ms@(MathState stk cur str op) (x:xs) = evalExpression ms' xs
    where ms' | x == '*' || x == '+' = ms { msOp = if x == '*' then (*) else (+) }
              | x == ' ' = ms
              | isDigit x && nextIsDigit = ms { msStr = str' }
              | isDigit x = ms { msCur = op (read str') cur
                               , msStr = ""
                               }
              | x == '(' = ms { msCur = 0
                              , msOp  = (+)
                              , msStk = op cur Seq.<| stk
                              }
              | x == ')' = ms { msStk = Seq.drop 1 stk
                              , msCur = Seq.index stk 0 cur
                              }
              | otherwise = error $ "unexpected char " ++ show x
          nextIsDigit = not (null xs) && isDigit (head xs)
          str' = x : str

-- evaluate an expression of numbers, *, +, and () to return its result
evalLine :: String -> Int
evalLine = msCur . evalExpression (MathState Seq.empty 0 "" (+))

solveP1 = sum . map evalLine . lines

-- do we need a syntax tree?
-- maybe not..
data AdvState = AdvState { asStr :: String
                         , asTotal :: Int
                         , asMultTotal :: Int
                         , asStk :: Seq.Seq (Int -> Int)
                         , asOp :: Int -> Int -> Int
                         }

isPlus op = op 1 1 == 2

evalAdvanced :: AdvState -> String -> AdvState
evalAdvanced as [] = as
evalAdvanced as@(AdvState str total multTotal stk op) (x:xs) = evalAdvanced as' xs
    where as' | traceShow (x, total, multTotal) False = undefined
              | x == '*' = as { asTotal = 0
                              , asMultTotal = total * multTotal
                              , asOp = (*)
                              }
              | x == '+' = as { asOp = (+) }
              | isDigit x && nextIsDigit = as { asStr = x:str }
              | isDigit x = as { asStr = ""
                               , asTotal = (read (x:str) + if isPlus op then total else 0)
                                    * if nextIsEndOfScope then multTotal else 1
                               , asMultTotal = if nextIsEndOfScope then 1 else multTotal
                               }
              | x == '(' = as { asTotal = 0
                              , asMultTotal = 1
                              , asOp = (+)
                              , asStk = addIfPlus $ (* multTotal) Seq.<| stk
                              }
              | x == ')' = as { asStk = Seq.drop 2 stk
                              , asTotal = Seq.index stk 0 (total * multTotal)
                                     * if nextIsEndOfScope then Seq.index stk 1 1 else 1
                              , asMultTotal = if nextIsEndOfScope then 1 else Seq.index stk 1 1
                              }
              | x == ' ' = as
              | otherwise = error $ "unexpected char " ++ show x
          nextIsDigit = not (null xs) && isDigit (head xs)
          nextIsEndOfScope = null xs || head xs == ')'
          addIfPlus s = (+ (if isPlus op then total else 0)) Seq.<| s

evalAdvancedLine = asTotal . (\x -> traceShow ("end", asTotal x, asMultTotal x) x) . evalAdvanced (AdvState "" 0 1 Seq.empty (+))

solveP2 = sum . map evalAdvancedLine . lines

main :: IO()
main = do
    putStrLn "hi"
    expectEq (solveP1 "1 + 2 * 3 + 4 * 5 + 6") 71 "no parens"
    expectEq (solveP1 "1 + (2 * 3)") 7 "simple"
    expectEq (solveP1 "1 + (2 * 3) + (4 * (5 + 6))") 51 "w/ parens"
    expectEq (solveP1 "2 * 3 + (4 * 5)") 26 "example3"
    expectEq (solveP1 "5 + (8 * 3 + 9 + 3 * 4 * 3)") 437 "example4"
    expectEq (solveP1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") 12240 "example5"
    expectEq (solveP1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 ") 13632 "example6"

    putStrLn "now doing P2 examples"

    expectEq (solveP2 "1 + 2 * 3 + 4 * 5 + 6") (3*7*11) "no parens"
    expectEq (solveP2 "1 + (2 * 3)") 7 "simple"
    expectEq (solveP2 "1 + (2 * 3) + (4 * (5 + 6))") 51 "w/ parens"
    expectEq (solveP2 "2 * 3 + (4 * 5)") 46 "example3"
    expectEq (solveP2 "5 + (8 * 3 + 9 + 3 * 4 * 3)") 1445 "example4"
    expectEq (solveP2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") 669060 "example5"
    expectEq (solveP2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") 23340 "example6"

    putStrLn "now doing batch file"
    batch <- readFile "../batch.input"
    print $ solveP1 batch
    print $ solveP2 batch
    putStrLn "bye"
