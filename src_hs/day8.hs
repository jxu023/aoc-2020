import Control.Monad.State.Strict
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type Map = Map.Map
type Set = Set.Set

data Instruction = IAcc Int | IJmp Int | INop Int
                 deriving (Show)

type Code = Map Int Instruction
parseCode :: String -> Code
parseCode = Map.fromList . zip [0..] . map (parseInstruction . words) . lines

toNum (c:str) = (read str :: Int) * case c of '+' -> 1
                                              '-' -> -1

parseInstruction :: [String] -> Instruction
parseInstruction [instr, amt] = dat (toNum amt)
    where dat = case instr of "acc" -> IAcc
                              "jmp" -> IJmp
                              "nop" -> INop
                              other -> error ("err: " ++ show other)

data CpuState = CpuState { cpuIp :: Int
                         , cpuAcc :: Int
                         , cpuCode :: Code
                         } deriving (Show)

step :: CpuState -> CpuState
step cpu@(CpuState ip acc code) = case code Map.! ip of
    IAcc num    -> cpu { cpuIp = ip + 1, cpuAcc = acc + num }
    IJmp offset -> cpu { cpuIp = ip + offset }
    INop _      -> cpu { cpuIp = ip + 1 }

runToEnd :: CpuState -> State (Set Int) CpuState
runToEnd cpu = do
    visited <- get
    let ip = cpuIp cpu
    let terminate = Set.member ip visited || ip == Map.size (cpuCode cpu)
    if terminate then return cpu
                 else do
                     modify' (Set.insert (cpuIp cpu))
                     runToEnd (step cpu)

accBeforeLoop :: String -> Int
accBeforeLoop contents = cpuAcc $ evalState (runToEnd (CpuState 0 0 (parseCode contents))) Set.empty

flipJop :: Instruction -> Maybe Instruction
flipJop (IJmp num) = Just $ INop num
flipJop (INop num) = Just $ IJmp num
flipJop _ = Nothing

changeCode :: Code -> Int -> Maybe Code
changeCode code ind = do
    let instr = code Map.! ind
    instr' <- flipJop instr
    return $ Map.insert ind instr' code

accAfterTerminate :: String -> Int
accAfterTerminate contents =
    let code = parseCode contents
        modifiedCodes = mapMaybe (changeCode code) [0..]
        cpuStates = map (flip evalState Set.empty . runToEnd . CpuState 0 0) modifiedCodes 
        in cpuAcc . head . filter ((== Map.size code) . cpuIp) $ cpuStates

main :: IO ()
main = do
    print "hi"

    example <- readFile "../example.input"
    batch <- readFile "../batch.input"

    print $ accBeforeLoop example
    print $ accBeforeLoop batch

    print $ accAfterTerminate example
    print $ accAfterTerminate batch

    print "bye"
