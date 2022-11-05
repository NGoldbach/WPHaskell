module Utility where
import Data.Char(digitToInt)

data Figur = F {x::Int, y::Int, name::String, color::Char} | F2 {} deriving (Show, Eq, Ord)
data Move = M {xalt::Char, yalt::Int, xnew::Char, ynew::Int} deriving Show 

convX :: Char -> Int
convX c = fromEnum c-65
convToX :: Int -> Char
convToX x =  toEnum (x + 65)

convToMove :: String -> Move
convToMove s = 
            let     a = s!!0
                    b = digitToInt (s!!1)
                    c = s!!2
                    d = digitToInt (s!!3)
            in M a b c d

convMoves :: String -> [Move]
convMoves [] = []
convMoves s = [convToMove (take 4 s)] ++ x
             where x    | length s == 4 = []
                        | otherwise = convMoves (drop 5 s) 

convFromMove :: Move -> [Char]
convFromMove (M a b c d) = [a] ++ [convToX (b-17)]++[c] ++ [convToX (d-17)]

pawnMovesW :: [[Int]]
pawnMovesW = [[0,1],[0,2],[1,1],[-1,1]] 

pawnMovesB :: [[Int]]
pawnMovesB = [[0,-1],[0,-2],[1,-1],[-1,-1]] 

knightMoves :: [[Int]]
knightMoves = [[a,b] | a <- [-2,-1,1,2], b <- [-2,-1,1,2], a + b == -3 || a + b == -1 || a + b == 1 || a + b == 3]

kingMoves :: [[Int]]
kingMoves = [[a,b] | a <- [-1,0,1], b <- [-1,0,1], a /= 0 || b /= 0]

bishopMoves :: [[Int]]
bishopMoves = [[a,b] | a <- [-7..7], b <- [-7..7], abs a == abs b && a /= 0]  

rookMoves :: [[Int]]
rookMoves = [[a,b] | a <- [-7..7], b <- [-7..7], (a == 0) /= (b == 0)] 

queenMoves :: [[Int]]
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
validMove b f move@(M x1 y1 x2 y2)
        |color f /= color (head b) = False
        |convX (x2) < 0 || convX (x2) > 7 || y2 < 0 || y2 > 7 = False
        |isOccupied b amZug (convX x2) y2 = False 
        |(name f /= "knight") && (isBlocked b amZug (createCoordList move)) = False
        -- |(name f == "pawn") = isDiagonalAndValid b f amZug move
        |otherwise = True
        where amZug =  color (head b)

isDiagonalAndValid :: [Figur] -> Figur -> Char -> Move -> Bool
isDiagonalAndValid b f c (M x1 y1 x2 y2) = (x1 == x2) || isOccupied b tc (convX x2) y2 
                        where tc | c == 'w' = 'b'
                                 | otherwise = 'w'

isOccupied :: [Figur] -> Char -> Int -> Int -> Bool
isOccupied [] _ _ _ = False
isOccupied (f:fs) c xn yn = (color f == c && x f == xn && y f == yn) || isOccupied fs c xn yn

isBlocked :: [Figur] -> Char -> [(Int,Int)] -> Bool
isBlocked b c [] = False
isBlocked b c (m:ms) = isOccupied b c (fst m) (snd m) || isBlocked b c (ms)

createCoordList :: Move -> [(Int,Int)]
createCoordList move@(M x1 y1 x2 y2) =  makeIncrementList (convX x1) y1 (fst t) (snd t)  where t = divideMove move

makeIncrementList :: Int -> Int -> (Int,Int) -> Int -> [(Int,Int)]
makeIncrementList x y t 1 = []
makeIncrementList x y t i = tn : makeIncrementList (fst tn) (snd tn) t (i-1) where tn = ((x+fst t),(y+snd t))

--divides a move into single steps
divideMove :: Move -> ((Int,Int),Int)
divideMove (M x1 y1 x2 y2) = (((convX x2 - convX x1) `div` y ,  (y2-y1) `div` y ),y)       
                        where y | abs (convX x2 - convX x1) > abs (y2-y1) = abs(convX x2 - convX x1)
                                | otherwise = abs(y2-y1)


padString :: String -> String
padString [] = []
padString s = take 4 s ++ " " ++ padString (drop 4 s) 


--Test Kram
f0 :: Figur
f0 = F (-1) (-1) "" 'w'
f1 :: Figur
f1 = F 5 5 "knight" 'w'
f2 :: Figur
f2 = F 0 0 "king" 'b'
f3 :: Figur
f3 = F 6 7 "queen" 'b'
f4 = F 0 1 "pawn" 'w'
f5 = F 7 0 "pawn" 'b'
testB = testB1
testB1 = [f0,f1,f2,f3,f4,f5]
testB2 = [f0,f1,f2]