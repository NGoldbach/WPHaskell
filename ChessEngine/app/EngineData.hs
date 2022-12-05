module EngineData where
import Data.Char(digitToInt, isDigit)
import Data.List (isInfixOf)
import GHC.Float (int2Double)
import Data.Map hiding (take,drop)
import qualified Data.Map as Map
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
data Move = M {xalt::Char, yalt::Int, xnew::Char, ynew::Int} | CastleShort | CastleLong | EnPassantL | EnPassantR deriving Show 

enPassantMapBlack::Map String [(Int,Int)]
enPassantMapBlack = fromList([("A1A3",[(1,3)]),
                ("B1B3",[(0,3),(2,3)]),
                ("C1C3",[(1,3),(3,3)]),
                ("D1D3",[(2,3),(4,3)]),
                ("E1E3",[(3,3),(5,3)]),
                ("F1F3",[(4,3),(6,3)]),
                ("G1G3",[(5,3),(7,3)]),
                ("H1H3",[(6,3)])])
        
enPassantMapWhite::Map String [(Int,Int)]
enPassantMapWhite = fromList([("A6A4",[(1,4)]),
                ("B6B4",[(0,4),(2,4)]),
                ("C6C4",[(1,4),(3,4)]),
                ("D6D4",[(2,4),(4,4)]),
                ("E6E4",[(3,4),(5,4)]),
                ("F6F4",[(4,4),(6,4)]),
                ("G6G4",[(5,4),(7,4)]),
                ("H6H4",[(6,4)])])

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