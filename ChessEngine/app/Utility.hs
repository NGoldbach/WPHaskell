module Utility where
import Data.Char(digitToInt)

data Figur = F {x::Integer, y::Integer, name::String, color::Char} | F2 {} deriving (Show, Eq, Ord)
data Move = M {xalt::Char, yalt::Integer, xnew::Char, ynew::Integer} deriving Show 

convX :: Char -> Integer
convX c = toInteger(fromEnum c-65)

convToX :: Integer -> Char
convToX x =  toEnum (fromIntegral(x + 65))

convToMove :: String -> Move
convToMove s = 
            let     a = s!!0
                    b = toInteger (digitToInt (s!!1))
                    c = s!!2
                    d = toInteger (digitToInt (s!!3))
            in M a b c d

convMoves :: String -> [Move]
convMoves [] = []
convMoves s = [convToMove (take 4 s)] ++ x
             where x    | length s == 4 = []
                        | otherwise = convMoves (drop 5 s) 

convFromMove :: Move -> [Char]
convFromMove (M a b c d) = [a] ++ [convToX (b-17)]++[c] ++ [convToX (d-17)]

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


colorSwap :: [[Figur]] -> [[Figur]]
colorSwap [] = []
colorSwap (b:bs) = [y : tail b] ++ colorSwap bs
        where y = F (-1) (-1) (name (head b)) (c)
                where c | color (head b) == 'w' = 'b'
                        | otherwise = 'w'

validMove :: [Figur] -> Figur -> Move -> Bool
validMove [] _ _ = False
validMove b f (M x1 y1 x2 y2)
        |color f /= color (head b) = False
        |convX (x2) < 0 || convX (x2) > 7 || y2 < 0 || y2 > 7 = False
        |isOccupied b (color (head b)) (convX x2) y2 = False 
        |otherwise = True

isOccupied :: [Figur] -> Char -> Integer -> Integer -> Bool
isOccupied [] _ _ _ = False
isOccupied (f:fs) c xn yn = (color f == c && x f == xn && y f == yn) || isOccupied fs c xn yn

padString :: String -> String
padString [] = []
padString s = take 4 s ++ " " ++ padString (drop 4 s) 

--chain is occupied on each square by dividing the elements of the tupel containing the positional difference with the highest value to get proper increments to check procedually


--Test Kram
f0 :: Figur
f0 = F (-1) (-1) "" 'w'
f1 :: Figur
f1 = F 4 4 "knight" 'w'
f2 :: Figur
f2 = F 0 1 "pawn" 'b'
f3 :: Figur
f3 = F 5 6 "queen" 'b'
testB = testB1
testB1 = [f0,f1,f2,f3]
testB2 = [f0,f1,f2]