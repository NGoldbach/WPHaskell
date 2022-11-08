{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Language.Haskell.TH.Syntax (Lit(IntegerL))
import Utility
import Data.List (isInfixOf)

starterBoard :: [Figur]
starterBoard = [F (-1) (-1) "" 'w'] ++ createFigures 'w' ++ createFigures 'b'


moveFigure :: [Figur] -> [Move] -> [[Figur]]
moveFigure _ [] = []
moveFigure b (m:ms) = updateBoard b m : moveFigure b ms 

makeMoveList :: [Figur] -> Figur -> [[Int]] -> [Move]
makeMoveList _ _ [] = []
makeMoveList b f (m:ms) = moveCheckResult ++ makeMoveList b f ms
                where moveCheckResult   | (validMove b f currentMove) =  [currentMove]
                                        | otherwise = []
                                         where currentMove = M (convToX(x f)) (y f) (convToX (x f + m!!0)) (y f + m!!1)

createBoardVariations :: [Figur] -> [Figur] -> [[Figur]]
createBoardVariations _ [] = []
createBoardVariations b (x:xs) = moveFigure b (makeMoveList b x moves) ++ createBoardVariations b xs
                where moves | name x == "knight" = knightMoves
                            | name x == "rook" = rookMoves
                            | name x == "bishop" = bishopMoves
                            | name x == "queen" = queenMoves
                            | name x == "king" = kingMoves
                            | (name x == "pawn") && (color x == 'w') = pawnMovesW
                            | (name x == "pawn") && (color x == 'b') = pawnMovesB
                            | otherwise = []

createAllBoardVariations :: [[Figur]] -> [[Figur]]
createAllBoardVariations [] = []
createAllBoardVariations [b] = createBoardVariations b b
createAllBoardVariations (b:bs) = (createBoardVariations b b) ++ createAllBoardVariations bs

createDepthBased :: [[Figur]] -> Int -> [[Figur]]
createDephtBased [] _ = []
createDepthBased b 1 = b ++ createAllBoardVariations b
createDepthBased b x = b ++ createDepthBased (createAllBoardVariations b) (x-1)

createLengthList :: [[Figur]] -> Int -> [Int]
createLengthList [] _ = []
createLengthList b 0 = [filterByLength b (length (name (head (head b))) `div` 4)]
createLengthList b x = [y] ++ createLengthList (drop y b) (x-1)
                        where y = filterByLength b val
                                where val = length (name (head (head b))) `div` 4



boardComparator :: [[Figur]] -> [[Figur]] -> Char -> [[Figur]]
boardComparator [] _ _ = []
boardComparator _ [] _ = []
boardComparator b (sb:sbs) c = chooseBestBoard y c : boardComparator (drop (length y) b) sbs c
                                where y = (filterByMove (take 100 b) (name (head sb)))

calcSetup :: [Figur] -> Int -> ([[Figur]],[Int])
calcSetup b x = (newB,createLengthList newB x) where newB = reverse (createDepthBased [b] x)

calcDepthBased :: ([[Figur]],[Int]) -> Int -> ([[Figur]],[Int])
calcDepthBased ([], _) _ = ([],[])
calcDepthBased (_, []) _ = ([],[])
calcDepthBased (b,l) 1 = ([chooseBestBoard (init b) (color(head(head b)))], l)
calcDepthBased (b,l) x = calcDepthBased (boardUpdate ++ (drop (l!!0+l!!1) b),(drop 1 l)) (x-1)
                where boardUpdate = colorSwap (boardComparator (take (l!!0) b) (take (l!!1) (drop (l!!0) b)) (color(head(head b))))

chooseBestBoard :: [[Figur]] -> Char -> [Figur]
chooseBestBoard [] _ = []
chooseBestBoard [x] _ = x
chooseBestBoard (x:x2:xs) c | evaluateChessboard x c >= evaluateChessboard x2 c = chooseBestBoard (x:xs) c
                            | otherwise = chooseBestBoard (x2:xs) c

evaluateChessboard :: [Figur] -> Char -> Double
evaluateChessboard [] _ = 0
evaluateChessboard (x:xs) c = y + evaluateChessboard xs c
                            where y | name x == "pawn" = 1 * i
                                    | name x == "knight" = 3 * i
                                    | name x == "bishop" = 3 * i
                                    | name x == "rook" = 5 * i
                                    | name x == "queen" = 9 * i
                                    | name x == "king" = 1000 * i
                                    | name x == "castle" = 2 * i
                                    | otherwise = 0
                                    where i | color x == c = -1
                                            | otherwise = 1

getMovesFromMemory :: [Figur] -> Int -> String
getMovesFromMemory b x = padString (reverse (take (x*4) (reverse (name (head b)))))

updateBoard :: [Figur] -> Move -> [Figur]
updateBoard [] _ = []
updateBoard b CastleLong = [] -- E0G0 und flip color in memory , bewege king, bewege rook, rochade figur außerhalb des boards mit punktzahl
updateBoard b CastleShort = []
updateBoard (x:xs) y = s  ++ updateBoard xs y 
                            where s | figurCheck x y == F2 = [] 
                                    | otherwise = [figurCheck x y] 

updateBoardAll :: [Figur] -> [Move] -> [Figur]
updateBoardAll l [] = l
updateBoardAll f (x:xs) = updateBoardAll (updateBoard f x) xs  

figurCheck :: Figur -> Move -> Figur
figurCheck f m  | x f == convX (xalt m) && y f == yalt m = F (convX (xnew m)) (ynew m) (name f) (color f) 
                | x f == convX (xnew m) && y f == ynew m = F2 
                | x f == -1 = F (x f) (y f) (name f++convFromMove m) (turnColor)
                | otherwise = f
                where turnColor | color f == 'w' = 'b'
                                | otherwise = 'w'

cpuMove :: String -> Int -> String
cpuMove s i = if (length (s) <= 20) then openingBook s else (getMovesFromMemory calcResult i)
        where calcResult = head(fst (calcDepthBased (calcSetup b1 i) i))
                where b1 = updateBoardAll starterBoard (convMoves s)

openingBook :: String -> String
openingBook s | turnNumber == 1 = "G1G2"
              | turnNumber == 2 = "G6G5"
              | turnNumber == 3 = "F0G1"
              | turnNumber == 4 = if ("C0B1" `isInfixOf` trimString s) then "G7F5" else "F7G6"
              | turnNumber == 5 = if (("E4E3" `isInfixOf` trimString s) || ("G4G3" `isInfixOf` trimString s)) then "E1E2" else "G0F2"
              | otherwise = "You shouldn't read this."
        where turnNumber = ((length (trimString s)) `div` 4) + 1

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

readInt :: String -> Int
readInt = read

adjustedTurns :: String -> Int -> String
adjustedTurns [] _ = []
adjustedTurns (c:cs) v = checkedChar : adjustedTurns cs v
        where checkedChar | (c < '0' || c > '8') = c
                          | otherwise = toEnum ((fromEnum c) + v)


-- calculateDepthBased :: [[Figur]] -> Int -> Int -> [[Figur]] --Funktioniert für Tiefe 0,1,2, aber nicht für höher? Muss bearbeitet werden, immernoch falsch
-- calculateDepthBased [] _ _ = []
-- calculateDepthBased b 0 0 = colorSwap [chooseBestBoard b (color (head (head b)))] 
-- calculateDepthBased b 0 1 = [chooseBestBoard b1 (color(head(head b1)))] where b1 = colorSwap b
-- calculateDepthBased (b:bs) x y = calculateDepthBased ((calculateDepthBased (createBoardVariations b b) (x-1) 0) ++ (calculateDepthBased bs (x)) 0) 0 y