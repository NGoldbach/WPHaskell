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

createVariationsDepthBased :: [[Figur]] -> Int -> [[Figur]]
createVariationsDepthBased [] x = []
createVariationsDepthBased b 0 = b
createVariationsDepthBased b 1 = createAllBoardVariations b
createVariationsDepthBased b x =  createVariationsDepthBased (createAllBoardVariations b) (x-1)


chooseBestBoard :: [[Figur]] -> [Figur] -- Für Farbe anpassen
chooseBestBoard [] = []
chooseBestBoard [x] = x
chooseBestBoard (x:x2:xs) | evaluateChessboard x y >= evaluateChessboard x2 y = chooseBestBoard (x:xs)
                          | otherwise = chooseBestBoard (x2:xs)
                          where y = color (head x)


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
                | x f == -1 = F (x f) (y f) (name f++convFromMove m) (turnColor)
                | otherwise = f
                where turnColor | color f == 'w' = 'b'
                                | otherwise = 'w'

cpuMove :: String -> Int -> String
cpuMove s i = bestMove (chooseBestBoard (createVariationsDepthBased b1 i)) i
        where b1 = [updateBoardAll starterBoard (convMoves s)]