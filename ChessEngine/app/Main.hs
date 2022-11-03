{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Language.Haskell.TH.Syntax (Lit(IntegerL))
import Utility

starterBoard :: [Figur]
starterBoard = [F (-1) (-1) "" 'w'] ++ createFigures 'w' ++ createFigures 'b'


moveFigure :: [Figur] -> [Move] -> [[Figur]]
moveFigure _ [] = []
moveFigure b (m:ms) = updateBoard b m : moveFigure b ms 

makeMoveList :: [Figur] -> Figur -> [[Integer]] -> [Move]
makeMoveList _ _ [] = []
makeMoveList b f (m:ms) = moveCheckResult ++ makeMoveList b f ms
                where moveCheckResult   | (validMove b f currentMove) =  [currentMove]
                                        | otherwise = []
                                         where currentMove = M (convToX(x f)) (y f) (convToX (x f + m!!0)) (y f + m!!1)

createBoardVariations :: [Figur] -> [Figur] -> [[Figur]]
createBoardVariations _ [] = []
createBoardVariations b (x:xs) = moveFigure b (makeMoveList b x moves) ++ createBoardVariations b xs
                where moves | name x == "pawn" = pawnMoves
                            | name x == "knight" = knightMoves
                            | name x == "rook" = rookMoves
                            | name x == "bishop" = bishopMoves
                            | name x == "queen" = queenMoves
                            | name x == "king" = kingMoves
                            | otherwise = []

createAllBoardVariations :: [[Figur]] -> [[Figur]]
createAllBoardVariations [] = []
createAllBoardVariations [b] = createBoardVariations b b
createAllBoardVariations (b:bs) = (createBoardVariations b b) ++ createAllBoardVariations bs

calculateDepthBased :: [[Figur]] -> Int -> Int -> [[Figur]] --Funktioniert für Tiefe 0,1,2, aber nicht für höher? Muss bearbeitet werden, immernoch falsch
calculateDepthBased [] _ _ = []
calculateDepthBased b 0 0 = colorSwap [chooseBestBoard b (color (head (head b)))] 
calculateDepthBased b 0 1 = [chooseBestBoard b1 (color(head(head b1)))] where b1 = colorSwap b
calculateDepthBased (b:bs) x y = calculateDepthBased ((calculateDepthBased (createBoardVariations b b) (x-1) 0) ++ (calculateDepthBased bs (x)) 0) 0 y

chooseBestBoard :: [[Figur]] -> Char -> [Figur] -- Für Farbe anpassen
chooseBestBoard [] _ = []
chooseBestBoard [x] _ = x
chooseBestBoard (x:x2:xs) c | evaluateChessboard x c >= evaluateChessboard x2 c = chooseBestBoard (x:xs) c
                            | otherwise = chooseBestBoard (x2:xs) c

evaluateChessboard :: [Figur] -> Char -> Int
evaluateChessboard [] _ = 0
evaluateChessboard (x:xs) c = y + evaluateChessboard xs c
                            where y | name x == "pawn" = 1 * i
                                    | name x == "knight" = 3 * i
                                    | name x == "bishop" = 3 * i
                                    | name x == "rook" = 5 * i
                                    | name x == "queen" = 9 * i
                                    | otherwise = 0
                                    where i | color x == c = -1
                                            | otherwise = 1

bestMove :: [Figur] -> Int -> String
bestMove b x = padString (reverse (take (x*4) (reverse (name (head b)))))

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
                | x f == -1 = F (x f) (y f) (name f++convFromMove m) (turnColor)
                | otherwise = f
                where turnColor | color f == 'w' = 'b'
                                | otherwise = 'w'

cpuMove :: String -> Int -> String
cpuMove s i = bestMove (head (calculateDepthBased b1 i 1)) i
        where b1 = [updateBoardAll starterBoard (convMoves s)]

cpuMoveTest :: String -> Int -> String
cpuMoveTest s i = bestMove (head (calculateDepthBased b1 i 1)) i
         where b1 = [updateBoardAll testB(convMoves s)]
