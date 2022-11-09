module Utility where
import Data.Char(digitToInt, isDigit)
import Data.List ( isInfixOf )

data Figur = F {x::Int, y::Int, name::String, color::Char} | F2 {} deriving (Show, Eq, Ord)
data Move = M {xalt::Char, yalt::Int, xnew::Char, ynew::Int} | CastleShort | CastleLong deriving Show 

convX :: Char -> Int
convX c = fromEnum c-65
convToX :: Int -> Char
convToX x =  toEnum (x + 65)

convToMove :: String -> Move
convToMove s = 
            let     a = s!!0
                    b = if(isDigit (s!!1)) then digitToInt (s!!1) else 0
                    c = s!!2
                    d = if(isDigit (s!!1)) then digitToInt (s!!3) else 0
            in if(isDigit (s!!1)) then M a b c d else convSpecialMove s

convSpecialMove :: String -> Move
convSpecialMove s | s == "CLC0" || s == "CLC7" = CastleLong
                  | s == "CLG0" || s == "CLG7" = CastleShort

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

defPawnPositionsW :: [(Int,Int)]
defPawnPositionsW = [(5,1),(6,2),(7,1)]

defPawnPositionsB :: [(Int,Int)]
defPawnPositionsB = [(5,6),(6,5),(7,6)]

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
        |(name f /= "knight") && (isBlocked b (createCoordList move)) = False
        |(name f == "pawn") && (isInvalidPawnMove b f amZug move) = False
        |otherwise = True
        where amZug =  color (head b)

castleCheck :: [Figur] -> String -> Char -> [Move] -- Hässlichste Funktion
castleCheck b m c 
                        | hasMoved m (if (c == 'w') then "E0" else "E7") = [] 
                        | hasMoved m (if (c == 'w') then "CLC0" else "CLC7") || hasMoved m (if (c == 'w') then "CLG0" else "CLG7") = []
                        | rookUnavailable b m c 0 && rookUnavailable b m c 1 = [] 
                        | not (rookUnavailable b m c 0) && rookUnavailable b m c 1 = [CastleLong]
                        | rookUnavailable b m c 0 &&  not (rookUnavailable b m c 1) = [CastleShort]
                        | otherwise = [CastleLong, CastleShort]
            
specialMoves :: [Figur] -> [Move]
specialMoves b = castleCheck b (name memory) (color memory) where memory = head b

hasMoved :: String -> String -> Bool
hasMoved moves pos = pos `isInfixOf` moves

rookUnavailable :: [Figur] -> String -> Char -> Int -> Bool
rookUnavailable b m c 0 = hasMoved  m (if (c == 'w') then "A0" else "A7") || isBlocked b (createCoordList (M 'A' v 'E' v)) where v = if (c == 'w') then 0 else 7
rookUnavailable b m c 1 = hasMoved  m (if (c == 'w') then "H0" else "H7") || isBlocked b (createCoordList (M 'H' v 'E' v)) where v = if (c == 'w') then 0 else 7

isInvalidPawnMove :: [Figur] -> Figur -> Char -> Move -> Bool
isInvalidPawnMove b f c (M x1 y1 x2 y2) = ((x1 /= x2) && (not (isOccupied b tc (convX x2) y2))) || ((x1 == x2) && (isOccupied b tc (convX x2) y2)) || ((abs (y1-y2) == 2) && ((y1 /= 0 && c == 'w')||(y1 /= 7 && c == 'b')))
                        where tc | c == 'w' = 'b'
                                 | otherwise = 'w'

isOccupied :: [Figur] -> Char -> Int -> Int -> Bool
isOccupied [] _ _ _ = False
isOccupied (f:fs) c xn yn = (color f == c && x f == xn && y f == yn) || isOccupied fs c xn yn

isBlocked :: [Figur] -> [(Int,Int)] -> Bool
isBlocked b [] = False
isBlocked b (m:ms) = isOccupied b 'w' (fst m) (snd m) || isOccupied b 'b' (fst m) (snd m) || isBlocked b (ms)

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

trimString :: [Char] -> [Char]
trimString [] = []
trimString (c:cs) = y ++ trimString cs 
        where y | c == ' ' = []
                | otherwise = [c] 

pullMemory :: [[Figur]] -> [String]
pullMemory [] = []
pullMemory (b:bs) = name (head b) : pullMemory bs

pullLength :: [[Figur]] -> [Int]
pullLength [] = []
pullLength (b:bs) = length b : pullLength bs


filterByLength :: [[Figur]] -> Int -> Int
filterByLength [] _ = 0
filterByLength (b:bs) x = boardCheck + filterByLength bs x
                        where boardCheck | (length (name (head b))) == x*4 = 1
                                         | otherwise = 0

filterByMove :: [[Figur]] -> String -> [[Figur]]
filterByMove [] _ = []
filterByMove (b:bs) s = boardCheck ++ filterByMove bs s
                where boardCheck | (s == (take (length s)(name (head b)))) = [b]
                                 | otherwise = []

adjustedTurns :: String -> Int -> String
adjustedTurns [] _ = []
adjustedTurns (c:cs) v = checkedChar : adjustedTurns cs v
                where checkedChar       | (c < '0' || c > '8') = c
                                        | otherwise = toEnum ((fromEnum c) + v)

readInt :: String -> Int
readInt = read

turnColor :: Char -> Char
turnColor c | c == 'w' = 'b'
            | otherwise = 'w'

f0 = F 1 1 "pawn" 'w'

f1 = F 2 1 "pawn" 'w'

f2 = F 3 1 "pawn" 'w'

f3 = F 4 1 "pawn" 'b'

f4 = F 5 1 "pawn" 'b'
testB = [f0,f1,f2,f3,f4]


testString = adjustedTurns "G2G3 G1F3 F1G2 D7D5 G1F3 C7C6 CLG1 C8F5 B2B3 " (-1)
-- calculateDepthBased :: [[Figur]] -> Int -> Int -> [[Figur]] --Funktioniert für Tiefe 0,1,2, aber nicht für höher? Muss bearbeitet werden, immernoch falsch
-- calculateDepthBased [] _ _ = []
-- calculateDepthBased b 0 0 = colorSwap [chooseBestBoard b (color (head (head b)))] 
-- calculateDepthBased b 0 1 = [chooseBestBoard b1 (color(head(head b1)))] where b1 = colorSwap b
-- calculateDepthBased (b:bs) x y = calculateDepthBased ((calculateDepthBased (createBoardVariations b b) (x-1) 0) ++ (calculateDepthBased bs (x)) 0) 0 y
                                 