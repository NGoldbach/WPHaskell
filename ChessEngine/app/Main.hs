{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Language.Haskell.TH.Syntax (Lit(IntegerL))
import Utility

starterBoard :: [Figur]
starterBoard = [F (-1) (-1) "" 'w'] ++ createFigures 'w' ++ createFigures 'b'


--checkValidity :: [Figur] -> Move -> Bool

moveFigure :: [Figur] -> [Move] -> [[Figur]]
moveFigure _ [] = []
moveFigure b (m:ms) = updateBoard b m : moveFigure b ms 

makeMoveList :: Figur -> [[Integer]] -> [Move]
makeMoveList _ [] = []
makeMoveList f (m:ms) = M (convToX(x f)) (y f) (convToX (x f + m!!0)) (y f + m!!1) : makeMoveList f ms  

createPossibleBoards :: [Figur] -> [Figur] -> [[Figur]]
createPossibleBoards _ [] = []
createPossibleBoards b (x:xs) = moveFigure b (makeMoveList x moves) ++ createPossibleBoards b xs
                where moves | name x == "pawn" = pawnMoves
                            | name x == "knight" = knightMoves
                            | name x == "rook" = rookMoves
                            | name x == "bishop" = bishopMoves
                            | name x == "queen" = queenMoves
                            | name x == "king" = kingMoves
                            | otherwise = []
 
chooseBestBoard :: [[Figur]] -> [Figur] -- Für Farbe anpassen
chooseBestBoard [] = []
chooseBestBoard [x] = x
chooseBestBoard (x:x2:xs) | evaluateChessboard x >= evaluateChessboard x2 = chooseBestBoard (x:xs)
                          | otherwise = chooseBestBoard (x2:xs)


evaluateChessboard :: [Figur] -> Int
evaluateChessboard [] = 0
evaluateChessboard (x:xs) = y + evaluateChessboard xs
                            where y | name x == "pawn" = 1
                                    | name x == "knight" = 3
                                    | name x == "bishop" = 3
                                    | name x == "rook" = 5
                                    | name x == "queen" = 9
                                    | otherwise = 0

bestMove :: [Figur] -> Integer -> String
bestMove b x = reverse (take 4 (drop ((fromIntegral x-1)*4) (reverse (name (head b)))))

updateBoard :: [Figur] -> Move -> [Figur]
updateBoard [] _ = []
updateBoard (x:xs) y = s  ++ updateBoard xs y 
                            where s | figurCheck x y == F2 = [] 
                                    | otherwise = [figurCheck x y] 

updateBoardAll :: [Figur] -> [Move] -> [Figur]
updateBoardAll l [] = l
updateBoardAll f (x:xs) = updateBoardAll (updateBoard f x) xs  

figurCheck :: Figur -> Move -> Figur
figurCheck f m  | x f == convX (xalt m) && y f == yalt m = F (convX (xnew m)) (ynew m) (name f) (color f) 
                | x f == convX (xnew m) && y f == ynew m = F2 
                | x f == -1 = F (x f) (y f) (name f++convFromMove m) (color f)
                | otherwise = f

cpuMove :: String -> Integer -> String
cpuMove s i = bestMove (chooseBestBoard (createPossibleBoards b1 b1)) i
        where b1 = updateBoardAll starterBoard (convMoves s)

boardDuplicator :: Int -> [[Figur]]
boardDuplicator 0 = []
boardDuplicator x = [starterBoard] ++ boardDuplicator (x-1)