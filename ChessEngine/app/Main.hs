import Language.Haskell.TH.Syntax (Lit(IntegerL))
import Utility
import Data.List (isInfixOf)
import GHC.Float (int2Double)

-- The board, when a chess game starts.
-- First figure is a so called memory figure,
-- that represents the color that is about to play.
starterBoard :: [Figur]
starterBoard = [F (-1) (-1) "" 'w'] ++ createFigures 'w' ++ createFigures 'b'


-- A function that takes a board and a list of moves
-- and returns a list of the resulting boards.
-- Each resulting board in the returned list
-- is a possible game situation after a move from the move-list has been played.
moveFigure :: [Figur] -> [Move] -> [[Figur]]
moveFigure _ [] = []
moveFigure b (m:ms) = boardCheck ++ moveFigure b ms
                where boardCheck | isCheck newBoard (getTileFromName newBoard "king") = []
                                 | otherwise = [newBoard]
                        where newBoard = updateBoard b m

-- A function that accepts a board
-- and returns a list of boards. 
-- It creates all the variations of a given board
-- considering the moves that the figures on the board are able to make.
createBoardVariations :: [Figur] -> [Figur] -> [[Figur]]
createBoardVariations _ [] = []
createBoardVariations b (x:xs) = moveFigure b moveList ++ createBoardVariations b xs
        where moveList = if (y x == (-1)) then (specialMoves b) else (makeMoveList b x moves) --Specialmoves für y x (-1) einfügen
                where moves | name x == "knight" = knightMoves
                            | name x == "rook" = rookMoves
                            | name x == "bishop" = bishopMoves
                            | name x == "queen" = queenMoves
                            | name x == "king" = kingMoves
                            | (name x == "pawn") && (color x == 'w') = pawnMovesW
                            | (name x == "pawn") && (color x == 'b') = pawnMovesB
                            | otherwise = []


-- A recursive function call of createBoardVariations.
-- It receives a list of boards and
-- returns a list of boards.
createAllBoardVariations :: [[Figur]] -> [[Figur]]
createAllBoardVariations [] = []
createAllBoardVariations [b] = createBoardVariations b b
createAllBoardVariations (b:bs) = (createBoardVariations b b) ++ createAllBoardVariations bs

-- creates all the board variations possible
-- given a number x, which is a natural number but not zero.
-- The number x represents the depth of the chess engine's forecasting ability.
-- The chess engine can currently generate boards for a depth up to 4, within minutes.  
createDepthBased :: [[Figur]] -> Int -> [[Figur]]
createDephtBased [] _ = []
createDepthBased b 1 = b ++ createAllBoardVariations b
createDepthBased b x = b ++ createDepthBased (createAllBoardVariations b) (x-1)


-- A function that takes a list of boards, an integer number
-- and returns a list of integer numbers.
--
createLengthList :: [[Figur]] -> Int -> [Int]
createLengthList [] _ = []
createLengthList b 0 = [filterByLength b (length (name (head (head b))) `div` 4)]
createLengthList b x = [y] ++ createLengthList (drop y b) (x-1)
                        where y = filterByLength b val
                                where val = length (name (head (head b))) `div` 4


-- A function that receives 2 lists of boards, the color
-- and returns a list of boards.
-- ******************************************
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
                where boardUpdate = colorSwap (filter (not.null) (boardComparator (take (l!!0) b) (take (l!!1) (drop (l!!0) b)) (color(head(head b)))))

-- A function, that expects a list of boards, the color
-- and returns a board.
-- The returned board is the one with the best evaluation.
chooseBestBoard :: [[Figur]] -> Char -> [Figur]
chooseBestBoard [] _ = []
chooseBestBoard [x] _ = x
chooseBestBoard (x:x2:xs) c | (evaluateChessboard x c + boardQuotient x c 0 0) >= (evaluateChessboard x2 c + boardQuotient x2 c 0 0) = chooseBestBoard (x:xs) c
                            | otherwise = chooseBestBoard (x2:xs) c

-- A simple function, that
-- takes a board and a character, which determines the color
-- and offers a Double, which is interpreted as the value of a board,
-- in terms of material count and material position.
evaluateChessboard :: [Figur] -> Char -> Double
evaluateChessboard [] _ = 0
evaluateChessboard (x:xs) c = y + evaluateChessboard xs c
                            where y | name x == "pawn" = (1+pawnEvaluation x) * i
                                    | name x == "knight" = (3+centerEvaluation x+aggressionEvaluation x) * i
                                    | name x == "bishop" = (3+centerEvaluation x+aggressionEvaluation x + longDiagonal x) * i
                                    | name x == "rook" = (5+centerEvaluation x+aggressionEvaluation x) * i
                                    | name x == "queen" = (9+centerEvaluation x+aggressionEvaluation x) * i
                                    | name x == "king" = 1000 * i
                                    | name x == "castle" = 0.9 * i
                                    | otherwise = 0
                                    where i | color x == c = -1
                                            | otherwise = 1

boardQuotient :: [Figur] -> Char -> Double -> Double -> Double
boardQuotient [] c oben unten = 10 * (oben/unten)
boardQuotient (f:fs) c oben unten = boardQuotient fs c (fst x) (snd x)
            where x = quotientUpdate (oben,unten) (name f) (color f /= c)

quotientUpdate :: (Double,Double) -> String -> Bool ->(Double,Double)
quotientUpdate (o,u) name oben | oben = (o+x,u)
                               | otherwise = (o,u+x)
                            where x | name == "pawn" = 1
                                    | name == "knight" = 3
                                    | name == "bishop" = 3
                                    | name == "rook" = 5
                                    | name == "queen" = 9
                                    | name == "king" = 1
                                    | otherwise = 0

pawnEvaluation :: Figur -> Double
pawnEvaluation p | color p == 'w' && (x p, y p) `elem` defPawnPositionsW = 0.5
                 | color p == 'b' && (x p, y p) `elem` defPawnPositionsB = 0.5
                 | color p == 'w' = int2Double (x p) * 0.05
                 | otherwise = int2Double (7-x p) * 0.05

centerEvaluation :: Figur -> Double
centerEvaluation p | (x p >=2 && x p <=5) && (y p >= 2 && y p <=5) = 0.25
                   | otherwise = 0

aggressionEvaluation :: Figur -> Double
aggressionEvaluation p | (x p >=1 && x p <=6) && (y p >= (4-colorDifference) && y p <= (6-colorDifference)) = 0.125
                       | otherwise = 0
                        where colorDifference | color p == 'w' = 0
                                              | otherwise = 3

longDiagonal :: Figur -> Double
longDiagonal p | (x p == 6 && y p == 1 && color p == 'w') || (x p == 6 && y p == 6 && color p == 'b') = 0.5 
               | x p == 1 && (y p == 1 || y p == 6) = 0.3
               | otherwise = 0

getMovesFromMemory :: [Figur] -> Int -> String
getMovesFromMemory b x = reverse (take 4 (drop ((x-1)*4) (reverse (name (head b)))))

updateBoard :: [Figur] -> Move -> [Figur]
updateBoard [] _ = []
updateBoard b CastleLong = castlingFunc b True (color (head b)) yPos where yPos =  if (color (head b) == 'w') then 0 else 7
updateBoard b CastleShort = castlingFunc b False (color (head b)) yPos where yPos =  if (color (head b) == 'w') then 0 else 7
updateBoard (x:xs) y = s  ++ updateBoard xs y 
                            where s | figurCheck x y == F2 = [] 
                                    | otherwise = [figurCheck x y] 

updateBoardAll :: [Figur] -> [Move] -> [Figur]
updateBoardAll l [] = l
updateBoardAll f (x:xs) = updateBoardAll (updateBoard f x) xs  

figurCheck :: Figur -> Move -> Figur
figurCheck f m  | x f == convX (xalt m) && y f == yalt m = F (convX (xnew m)) (ynew m) (if ((name f == "pawn") && (ynew m == 0 || ynew m == 7)) then "queen" else name f) (color f) 
                | x f == convX (xnew m) && y f == ynew m = F2 
                | x f == -1 = F (x f) (y f) (name f++convFromMove m) (turnColor (color f))
                | otherwise = f
                
castlingFunc:: [Figur] -> Bool -> Char -> Int -> [Figur]
castlingFunc [] long c num = [F (-2) (-2) "castle" c]
castlingFunc (f:fs) long c num = newFigur : castlingFunc fs long c num
                where newFigur | long && name f == "king" && color f ==  c = F 2 num "king" c
                               | not long && name f == "king" && color f ==  c = F 6 num "king" c
                               | long && x f == 0 && y f == num = F 3 num "rook" c
                               | not long && x f == 7 && y f == num = F 5 num "rook" c
                               | long && x f == (-1) = F (x f) (y f) (name f++"CLC"++[yChar]) (turnColor c)
                               | not long && x f == (-1) = F (x f) (y f) (name f++"CLG"++[yChar]) (turnColor c)
                               | otherwise = f
                               where yChar | c == 'w' = '0'
                                           | otherwise = '7'

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


-- The main function. 
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
        turnLoop (allTurns++" "++adjustedResult)










-- promoteFunc :: [Figur] -> Int -> Int -> Char -> [Figur]
-- promoteFunc [] _ _ _ _ = []
-- promoteFunc (f:fs) p x1 x2 c = newFigur ++ promoteFunc fs p x1 x2 c
--         where newFigur  | x f == (-1) = F (x f) (y f) (name f++"PR"++[convToX x1]++[convToX x2]) (turnColor c)
--                         | (x f == x2) && (f y == (if (c == 'w') then 7 else 0)) = [] 
--                         | (x f == x1) && (f y == (if (c == 'w') then 6 else 1)) = [F x2 ((if (c == 'w') then 7 else 0)) "queen" c]
--                         | otherwise = f
-- Promote {x1::Int x2::Int}
-- "PR" `isInfixOf` s = Promote (convX s!!2) (convX s!!3)