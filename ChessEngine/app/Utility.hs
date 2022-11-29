module Utility where
import Data.Char(digitToInt, isDigit)
import Data.List ( isInfixOf )

-- A data type for a figure.
-- It has a x- and a y-coordinate,
-- a name and a color.
-- F2 shall represent the non-existance of a figure.
data Figur = F {x::Int, y::Int, name::String, color::Char} | F2 {} deriving (Show, Eq, Ord)

-- A data type for a move.
-- It has an old x- and y-coordinate,
-- and a new x- and y-coordinate.
-- CastleShort and CastleLong are special move Constructors
-- designed for the Castling Move.
data Move = M {xalt::Char, yalt::Int, xnew::Char, ynew::Int} | CastleShort | CastleLong deriving Show 

-- Converts a character to an int.
convX :: Char -> Int
convX c = fromEnum c-65

-- Converts an int to a character.
convToX :: Int -> Char
convToX x =  toEnum (x + 65)

-- Converts a String to a move.
convToMove :: String -> Move
convToMove s = 
            let     a = s!!0
                    b = if(isDigit (s!!1)) then digitToInt (s!!1) else 0
                    c = s!!2
                    d = if(isDigit (s!!1)) then digitToInt (s!!3) else 0
            in if(isDigit (s!!1)) then M a b c d else convSpecialMove s

-- Converts a String for a special Castling move to a move.
convSpecialMove :: String -> Move
convSpecialMove s | s == "CLC0" || s == "CLC7" = CastleLong
                  | s == "CLG0" || s == "CLG7" = CastleShort

-- Converts a String to a list of moves.
convMoves :: String -> [Move]
convMoves [] = []
convMoves s = [convToMove (take 4 s)] ++ x
             where x    | length s == 4 = []
                        | otherwise = convMoves (drop 5 s) 

-- Converts a move to a String.
convFromMove :: Move -> [Char]
convFromMove (M a b c d) = [a] ++ [convToX (b-17)]++[c] ++ [convToX (d-17)]


-- A list with the possible moves a white pawn can make.
pawnMovesW :: [[Int]]
pawnMovesW = [[0,1],[0,2],[1,1],[-1,1]] 

-- A list with the possible moves a black pawn can make.
pawnMovesB :: [[Int]]
pawnMovesB = [[0,-1],[0,-2],[1,-1],[-1,-1]] 

-- A list with the possible moves a knight can make.
knightMoves :: [[Int]]
knightMoves = [[a,b] | a <- [-2,-1,1,2], b <- [-2,-1,1,2], a + b == -3 || a + b == -1 || a + b == 1 || a + b == 3]

-- A list with the possible moves a king can make.
kingMoves :: [[Int]]
kingMoves = [[a,b] | a <- [-1,0,1], b <- [-1,0,1], a /= 0 || b /= 0]

-- A list with the possible moves a bishop can make.
bishopMoves :: [[Int]]
bishopMoves = [[a,b] | a <- [-7..7], b <- [-7..7], abs a == abs b && a /= 0]  

-- A list with the possible moves a rook can make.
rookMoves :: [[Int]]
rookMoves = [[a,b] | a <- [-7..7], b <- [-7..7], (a == 0) /= (b == 0)] 

-- A list with the possible moves a queen can make.
queenMoves :: [[Int]]
queenMoves = bishopMoves ++ rookMoves

-- A list of tuples, containing the defending positions of a white pawn.
-- These are the ones defending the king after a CastleShort move.
defPawnPositionsW :: [(Int,Int)]
defPawnPositionsW = [(5,1),(6,2),(7,1)]

-- A list of tuples, containing the defending positions of a black pawn.
-- These are the ones defending the king after a CastleShort move.
defPawnPositionsB :: [(Int,Int)]
defPawnPositionsB = [(5,6),(6,5),(7,6)]

-- A function that creates all the pawn figures for both colors.
createpawns :: Char -> [Figur]
createpawns c = [F x y "pawn" c | x <- [0..7]]
                where y | c == 'w' = 1
                        | c == 'b' = 6
                        | otherwise = -1 -- Besser: Fehler Nachricht

-- A function that creates the rest figures for both colors.
createRest :: Char -> [Figur]
createRest c = [F 0 y "rook" c, F 1 y "knight" c, F 2 y "bishop" c, F 3 y "queen" c,
               F 4 y "king" c, F 5 y "bishop" c, F 6 y "knight" c, F 7 y "rook" c]  
               where y | c == 'w' = 0
                       | c == 'b' = 7
                       | otherwise = -1

-- Creates all the figures. 
createFigures :: Char -> [Figur]
createFigures c = createRest c ++ createpawns c

-- A function that swaps the turn. 
colorSwap :: [[Figur]] -> [[Figur]]
colorSwap [] = []
colorSwap (b:bs) = [y : tail b] ++ colorSwap bs
        where y = F (-1) (-1) (name (head b)) (c)
                where c | color (head b) == 'w' = 'b'
                        | otherwise = 'w'

-- A function that,
-- given a board, a figure and a move
-- can determine if the move on the current board,
-- for the particular figure is valid.
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

-- A function that checks if a Castling move is possible, 
-- given the current board, a string containing moves and a color.
-- Returns an empty list if Castling is not possible,
-- otherwise the possible Castling moves are returned. 
castleCheck :: [Figur] -> String -> Char -> [Move]
castleCheck b m c 
                        | hasMoved m (if (c == 'w') then "E0" else "E7") = [] 
                        | hasMoved m (if (c == 'w') then "CLC0" else "CLC7") || hasMoved m (if (c == 'w') then "CLG0" else "CLG7") = []
                        | rookUnavailable b m c 0 && rookUnavailable b m c 1 = [] 
                        | not (rookUnavailable b m c 0) && rookUnavailable b m c 1 = [CastleLong]
                        | rookUnavailable b m c 0 &&  not (rookUnavailable b m c 1) = [CastleShort]
                        | otherwise = [CastleLong, CastleShort]

-- A list of played special Moves
specialMoves :: [Figur] -> [Move]
specialMoves b = castleCheck b (name memory) (color memory) where memory = head b

-- A function that, given a string that contains moves
-- and a string that represents a single move,
-- checks if this single string is a substring of the other one.
-- This essentially means that a figure has been moved.
hasMoved :: String -> String -> Bool
hasMoved moves pos = pos `isInfixOf` moves


-- This function checks if a rook has moved or if it is blocked.
-- If either of those two is true, then the rook cannot be used for Castling.
rookUnavailable :: [Figur] -> String -> Char -> Int -> Bool
rookUnavailable b m c 0 = hasMoved  m (if (c == 'w') then "A0" else "A7") || isBlocked b (createCoordList (M 'A' v 'E' v)) where v = if (c == 'w') then 0 else 7
rookUnavailable b m c 1 = hasMoved  m (if (c == 'w') then "H0" else "H7") || isBlocked b (createCoordList (M 'H' v 'E' v)) where v = if (c == 'w') then 0 else 7


--------------------------------------------------------------IS A FIGURE ARGUMENT NEEDED?

-- A function that receives a board, a figure, the color which is about to play and a move
-- and returns a boolean.
-- It's purpose is to check if the received move is valid given the current board situation.
isInvalidPawnMove :: [Figur] -> Figur -> Char -> Move -> Bool
isInvalidPawnMove b f c (M x1 y1 x2 y2) = ((x1 /= x2) && (not (isOccupied b tc (convX x2) y2))) || 
                        ((x1 == x2) && ((isOccupied b 'b' (convX x2) y2) || 
                        (isOccupied b 'w' (convX x2) y2))) || 
                        ((abs (y1-y2) == 2) && ((y1 /= 1 && c == 'w') || 
                        (y1 /= 6 && c == 'b')))

                        where tc | c == 'w' = 'b'
                                 | otherwise = 'w'

-- This function tests if a square on the board is occupied 
-- by a figure of the same color.
isOccupied :: [Figur] -> Char -> Int -> Int -> Bool
isOccupied [] _ _ _ = False
isOccupied (f:fs) c xn yn = (color f == c && x f == xn && y f == yn) || isOccupied fs c xn yn

-- A function that checks if a figure's path is blocked 
-- by other figures standing on its way. 
-- It is essentially a recursive call of isOccupied.
isBlocked :: [Figur] -> [(Int,Int)] -> Bool
isBlocked b [] = False
isBlocked b (m:ms) = isOccupied b 'w' (fst m) (snd m) || isOccupied b 'b' (fst m) (snd m) || isBlocked b (ms)


createCoordList :: Move -> [(Int,Int)]
createCoordList move@(M x1 y1 x2 y2) =  makeIncrementList (convX x1) y1 (fst t) (snd t)  where t = divideMove move


makeIncrementList :: Int -> Int -> (Int,Int) -> Int -> [(Int,Int)]
makeIncrementList x y t 1 = []
makeIncrementList x y t i = tn : makeIncrementList (fst tn) (snd tn) t (i-1) where tn = ((x+fst t),(y+snd t))

--It divides a move into single steps.
divideMove :: Move -> ((Int,Int),Int)
divideMove (M x1 y1 x2 y2) = (((convX x2 - convX x1) `div` y ,  (y2-y1) `div` y ),y)       
                        where y | abs (convX x2 - convX x1) > abs (y2-y1) = abs(convX x2 - convX x1)
                                | otherwise = abs(y2-y1)

isCheck :: [Figur] -> (Int, Int) -> Bool
isCheck b (x1,y1) = let s1 = tileScope (x1,y1)
                        s2 = filterByScope s1 b (color(head b))
                        s3 = scopeFigureMoves b s2
                        in doScopedMovesCheck s3 (x1,y1)

doScopedMovesCheck :: [Move] -> (Int, Int) -> Bool
doScopedMovesCheck [] _ = False
doScopedMovesCheck (m:ms) t = (convX (xnew m) == fst t && ynew m == snd t) || doScopedMovesCheck ms t
                        
tileScope :: (Int, Int) -> [(Int, Int)]
tileScope x = createScope x queenMoves ++ createScope x knightMoves

createScope :: Num b => (b, b) -> [[b]] -> [(b, b)]
createScope x [] = []
createScope x (y:ys) = (fst x + head y, snd x + last y) : createScope x ys

filterByScope :: [(Int, Int)] -> [Figur] -> Char -> [Figur]
filterByScope [] _ _ = []
filterByScope (t:ts) b c = figurAtTile ++ filterByScope ts b c
                where figurAtTile       | isOccupied b c (fst t) (snd t) = getFigurFromTile b t c
                                        | otherwise = []

getFigurFromTile :: [Figur] -> (Int, Int) -> Char -> [Figur]
getFigurFromTile [] _ _ = []
getFigurFromTile (f:fs) (tx,ty) c = if (x f == tx && y f == ty && color f == c && (x f >= 0)) then [f] else getFigurFromTile fs (tx,ty) c

getTileFromName [] _ = (8,8)
getTileFromName (f:fs) s = if (name f == s) then (x f, y f) else getTileFromName fs s


scopeFigureMoves b [] = []
scopeFigureMoves b (x:xs) = makeMoveList b x moves ++ scopeFigureMoves b xs
        where moves | name x == "knight" = knightMoves
                | name x == "rook" = rookMoves
                | name x == "bishop" = bishopMoves
                | name x == "queen" = queenMoves
                | name x == "king" = kingMoves
                | (name x == "pawn") && (color x == 'w') = pawnMovesW
                | (name x == "pawn") && (color x == 'b') = pawnMovesB
                | otherwise = []

makeMoveList :: [Figur] -> Figur -> [[Int]] -> [Move]
makeMoveList _ _ [] = []
makeMoveList b f (m:ms) = moveCheckResult ++ makeMoveList b f ms
                where moveCheckResult   | (validMove b f currentMove) =  [currentMove]
                                        | otherwise = []
                                                where currentMove = M (convToX(x f)) (y f) (convToX (x f + m!!0)) (y f + m!!1)

-- A function that separates a string using whitespace.
padString :: String -> String
padString [] = []
padString s = take 4 s ++ " " ++ padString (drop 4 s) 

-- A function that removes whitespaces from a string.
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

-- reads a string input and converts it to an Int.
readInt :: String -> Int
readInt = read

-- This function switches the color.
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
                                 