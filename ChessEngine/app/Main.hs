{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Lib ()
import Language.Haskell.TH.Syntax (Lit(IntegerL))


data Figur = F {x::Integer, y::Integer, name::String, color::Char} | F2 {} deriving (Show, Eq, Ord)

data Move = M {xalt::Char, yalt::Integer, xnew::Char, ynew::Integer} deriving Show 


createpawns :: Char -> [Figur]
createpawns c = [F x y "pawn" c | x <- [0..7]]
                where y | c == 'w' = 1
                        | c == 'b' = 6
                        | otherwise = -1 -- Besser: Fehler Nachricht

            
createRest :: Char -> [Figur]
createRest c = [F 0 y "rook" c, F 1 y "knight" c, F 2 y "bishop" c, F 3 y "queen" c,
               F 4 y "king" c, F 5 y "bishop" c, F 6 y "knight" c, F 7 y "rook" c]  
               where y | c == 'w' = 0
                       | c == 'b' = 7
                       | otherwise = -1

createFigures :: Char -> [Figur]
createFigures c = createRest c ++ createpawns c 

pawnMoves :: [[Integer]]
pawnMoves = [[0,1],[0,2],[1,1],[-1,1]] 

knightMoves :: [[Integer]]
knightMoves = [[a,b] | a <- [-2,-1,1,2], b <- [-2,-1,1,2], a + b == -3 || a + b == -1 || a + b == 1 || a + b == 3]

kingMoves :: [[Integer]]
kingMoves = [[a,b] | a <- [-1,0,1], b <- [-1,0,1], a /= 0 || b /= 0]

bishopMoves :: [[Integer]]
bishopMoves = [[a,b] | a <- [-7..7], b <- [-7..7], abs a == abs b && a /= 0]  

rookMoves :: [[Integer]]
rookMoves = [[a,b] | a <- [-7..7], b <- [-7..7], (a == 0) /= (b == 0)] 

queenMoves :: [[Integer]]
queenMoves = bishopMoves ++ rookMoves


--checkValidity :: [Figur] -> Move -> Bool

moveFigure :: [Figur] -> [Move] -> [[Figur]]
moveFigure _ [] = []
moveFigure b (m:ms) = updateBoard b m : moveFigure b ms 

makeMoveList :: Figur -> [[Integer]] -> [Move]
makeMoveList _ [] = []
makeMoveList f (m:ms) = M (toEnum (fromIntegral(x f + 65))) (y f) (toEnum (fromIntegral(x f + 65 + m!!0))) (y f + m!!1) : makeMoveList f ms  

moveFigures :: [Figur] -> [Figur] -> [[Figur]]
moveFigures _ [] = []
moveFigures b (x:xs) = moveFigure b (makeMoveList x moves) ++ moveFigures b xs
                where moves | name x == "pawn" = pawnMoves
                            | name x == "knight" = knightMoves
                            | name x == "rook" = rookMoves
                            | name x == "bishop" = bishopMoves
                            | name x == "queen" = queenMoves
                            | otherwise = kingMoves
 
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


--checkBestMove :: [Figur] -> [Figur] -> Move

--calculateMove :: [Figur] -> Move

updateBoard :: [Figur] -> Move -> [Figur]
updateBoard [] _ = []
updateBoard (x:xs) y = s  ++ updateBoard xs y 
                            where s | figurCheck x y == F2 = [] 
                                    | otherwise = [figurCheck x y] 

updateBoardAll :: [Figur] -> [Move] -> [Figur]
updateBoardAll l [] = l
updateBoardAll f (x:xs) = updateBoardAll (updateBoard f x) xs  

figurCheck :: Figur -> Move -> Figur
figurCheck f m  | fromIntegral (x f) == fromEnum (xalt m)-65 && y f == yalt m = F (toInteger(fromEnum (xnew m)-65)) (ynew m) (name f) (color f) 
                | fromIntegral (x f) == fromEnum (xnew m)-65 && y f == ynew m = F2 
                | otherwise = f



