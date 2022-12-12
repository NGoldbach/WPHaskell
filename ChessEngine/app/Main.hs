import Language.Haskell.TH.Syntax (Lit(IntegerL))
import EngineFunctions

-- The main function. 
-- Note for compiling: We always ran the programm with the optimization variant of -O2
-- The speedup is fairly noticable.

main :: IO ()
main = do
     turnLoop ""

--Recursive function, so that the player can keep playing/using the engine without interruptions
--Saves the string of moves between turns, so that they don't have to be re-entered.
turnLoop :: String -> IO ()
turnLoop s = do
        putStrLn "\nEnter new moves: (type 'help' to get instructions on how to use this engine) "
        turns <- getLine
        if (turns == "help") then 
          putStrLn ("Play by entering moves."++ 
          "Moves are entered by writing where the piece is currently standing, and where you want to move it."++
          "\nExample: A2A3 moves the pawn from A2 to A3 in the starting configuration."++
          "\nIn order to enter castling, type CL and then the coordinate of where the King will go. Example: CLG0 is short castle for White."++
          "\nIf you play en Passant, write EP followed by LT or RT, depending on if you take from left or right(white's perspective)."++
          "\nIf the computer is supposed to do the first move, then enter nothing on the first turn."++
          "\nOhterwise, simply play by entering your own moves. You do not have to enter the moves the computer says that it selected."++
          "\nThose moves are automatically saved already.")
          else return()
        if (turns == "help") then turnLoop s else return() 
        let allTurns = if (s /= "") then (s ++ " " ++ turns) else (turns)
        putStrLn "\n"
        let adjustedInput = adjustedTurns allTurns (-1)
        let result = cpuMove adjustedInput 3
        let adjustedResult = adjustedTurns result 1
        if (adjustedResult == (take 4 (drop (length allTurns-9) allTurns))) then finishFunc allTurns else continueFunc adjustedResult allTurns

--Continues the Gameloop
continueFunc :: [Char] -> [Char] -> IO ()
continueFunc aR aT = do
     print("Chosen follow-up: "++aR)
     print("Game: " ++ aT++" "++aR)
     turnLoop (aT++" "++aR)


--Finishes the game and notifies the player of the final gamestate.
finishFunc :: String -> IO ()
finishFunc s = do
     let x = (cpuMove (adjustedTurns s (-1)) 2)
     let x2 = adjustedTurns x 1
     let y = drop (length s-4) s
     let zResult = "You are going to be checkmated in one move. The game is over. Press Enter to close the programm."
     if(x2 /= y) then putStrLn "I'm about to be checkmated. I surrender. Press Enter to close the programm." else putStrLn zResult
     z <- getLine
     putStrLn ""