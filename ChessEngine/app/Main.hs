import Lib ()
import Language.Haskell.TH.Syntax (Lit(IntegerL))


data Figur = F {x::Integer, y::Integer, name::String, color::Char} deriving Show

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

figures :: [Figur]
figures = createFigures 'w'

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

--movePawns :: [Figur] -> [[Figur]]

--moveTowers :: [Figur] -> [[Figur]]

--moveKnights :: [Figur] -> [[Figur]]

--moveBishops :: [Figur] -> [[Figur]]

--moveQueens :: [Figur] -> [[Figur]]

--moveKing :: [Figur] -> [[Figur]]

--generateBoards :: [Figur] -> [[Figur]]

--chooseBestBoard :: [[Figur]] -> [Figur]

--checkBestMove :: [Figur] -> [Figur] -> Move

--calculateMove :: [Figur] -> Move

--updateBoard :: [Move] -> [Figur]