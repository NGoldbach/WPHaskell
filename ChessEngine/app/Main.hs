import Chess (cpuMove)
import Utility (adjustedTurns, readInt)

main :: IO b
main = do
        turnLoop ""

turnLoop :: [Char] -> IO b
turnLoop s = do
        putStrLn "\nEnter new moves: "
        turns <- getLine
        let allTurns = if (s /= "") then (s ++ " " ++ turns) else (turns)
        putStrLn "\nEnter Depth: "
        depth <- getLine
        let x = (readInt depth)
        putStrLn ""
        let adjustedInput = adjustedTurns allTurns (-1)
        let result = cpuMove adjustedInput x
        let adjustedResult = adjustedTurns result 1
        print("Chosen follow-up: "++adjustedResult)
        turnLoop allTurns


