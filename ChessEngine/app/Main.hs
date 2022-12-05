import Language.Haskell.TH.Syntax (Lit(IntegerL))
import EngineFunctions

-- The main function. 
main :: IO b
main = do
     turnLoop ""

--Recursive function, so that the player can keep playing/using the engine without interruptions
--Saves the string of moves between turns, so that they don't have to be re-entered.
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
        turnLoop (allTurns++" "++adjustedResult)