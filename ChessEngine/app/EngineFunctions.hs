module EngineFunctions where
import Data.Char(digitToInt, isDigit)
import Data.List (isInfixOf)
import GHC.Float (int2Double)
import Data.Map hiding (take,drop)
import qualified Data.Map as Map
import EngineData

--------------------------- Test Variables etc. -----------
testString :: String
testString = adjustedTurns "G2G3 F1G2 G1F3 F3H4 G2B7 B7A8 H1G1 E7E5 D7D6 F8E7 E7H4 G3H4 D8H4 H4H2 C8G4 G8F6 G1H1 G4H3" (-1)










-------------------------------------- Change Board-------------------------
-- The board, when a chess game starts.
-- First figure is a so called memory figure,
-- that "remembers" the color that is about to play,
-- aswell as the moves that have been played up to now.
starterBoard :: [Figur]
starterBoard = [F (-1) (-1) "" 'w'] ++ createFigures 'w' ++ createFigures 'b'

-- A function that takes a board and a list of moves
-- and returns a list of the resulting boards.
-- Each resulting board in the returned list
-- is a possible game situation after a move from the move-list has been played.
moveFigure :: [Figur] -> [Move] -> [[Figur]]
moveFigure _ [] = []
moveFigure b (m:ms) = boardCheck ++ moveFigure b ms
                where boardCheck | isCheck newBoard (getTileFromName newBoard "king" (color (head b))) = []
                                 | otherwise = [newBoard]
                        where newBoard = updateBoard b m

-- Takes a board and a move
-- and offers an updated board.
-- The updated board is the result of the move, that got played.
-- Castling is treated as a special move, because
-- of it consists of two moves.
updateBoard :: [Figur] -> Move -> [Figur]
updateBoard [] _ = []
updateBoard b EnPassantL = 
        let     memory = (name (head b))
                relevantM = (drop (length memory-2) memory)
                xDelete = convX (relevantM!!0)
                xMove = xDelete-1
                y = digitToInt (relevantM!!1)
        in enPassantFunc b True xDelete xMove y (color(head b))
updateBoard b EnPassantR = 
        let     memory = (name (head b))
                relevantM = (drop (length memory-2) memory)
                xDelete = convX (relevantM!!0)
                xMove = xDelete+1
                y = digitToInt (relevantM!!1)
        in enPassantFunc b False xDelete xMove y (color(head b))
updateBoard b CastleLong = castlingFunc b True (color (head b)) yPos where yPos =  if (color (head b) == 'w') then 0 else 7
updateBoard b CastleShort = castlingFunc b False (color (head b)) yPos where yPos =  if (color (head b) == 'w') then 0 else 7
updateBoard (x:xs) y = s  ++ updateBoard xs y 
                            where s | figurCheck x y == F2 = [] 
                                    | otherwise = [figurCheck x y] 

-- A recursive call of updateBoard.
updateBoardAll :: [Figur] -> [Move] -> [Figur]
updateBoardAll l [] = l
updateBoardAll f (x:xs) = updateBoardAll (updateBoard f x) xs  

-- Receives a figure and a move and returns a figure.
-- The returned figure can be the given figure after a move,
-- the given figure untouched or nothing, if the figure has been taken.
figurCheck :: Figur -> Move -> Figur
figurCheck f m  | x f == convX (xalt m) && y f == yalt m = F (convX (xnew m)) (ynew m) (if ((name f == "pawn") && (ynew m == 0 || ynew m == 7)) then "queen" else name f) (color f) 
                | x f == convX (xnew m) && y f == ynew m = F2 
                | x f == -1 = F (x f) (y f) (name f++convFromMove m) (turnColor (color f))
                | otherwise = f

-- Changes Board based on which side the castling happened. Similar logic as figurCheck
castlingFunc:: [Figur] -> Bool -> Char -> Int -> [Figur]
castlingFunc [] long c num = [F (-2) (-2) "castle" c]
castlingFunc (f:fs) long c num = newFigur : castlingFunc fs long c num
                where newFigur | long && name f == "king" && color f ==  c = F 2 num "king" c
                               | not long && name f == "king" && color f ==  c = F 6 num "king" c
                               | long && x f == 0 && y f == num = F 3 num "rook" c
                               | not long && x f == 7 && y f == num = F 5 num "rook" c
                               | long && x f == (-1) = F (x f) (y f) (name f++"CLC"++[yChar]) (turnColor c)
                               | not long && x f == (-1) = F (x f) (y f) (name f++"CLG"++[yChar]) (turnColor c)
                               | otherwise = f
                               where yChar | c == 'w' = '0'
                                           | otherwise = '7'

-- Changes Board based on which pawn took en Passant. Similiar logic as figurCheck
enPassantFunc:: [Figur] -> Bool -> Int -> Int -> Int -> Char -> [Figur]
enPassantFunc [] _ _ _ _ _ = []
enPassantFunc (f:fs) fromLeft xDelete xMove yDM c = newFigur ++ enPassantFunc fs fromLeft xDelete xMove yDM c
                where newFigur  | x f == xDelete && y f == yDM = []
                                | x f == xMove && y f == yDM = [F xDelete (if c == 'w' then yDM+1 else yDM-1) (name f) (color f)]
                                | x f == (-1) = [F (x f) (y f) (name f++enPassantFuncHelper fromLeft xDelete yDM (if c == 'w' then 1 else (-1))) (turnColor (color f))]
                                | otherwise = [f]
-- Creates the string that will be added to the memory figure after en Passant
enPassantFuncHelper:: Bool->Int->Int->Int->String
enPassantFuncHelper fromLeft xDelete yDM offset = convToX (xDelete+(if fromLeft then (-1) else 1)) : convToX (yDM-17) : convToX xDelete : [convToX (yDM-17+offset)]


-------------------------------------------------- Rules / Check Validity ----------------------------------------
--Checks if any en Passant move is possible
enPassantCheck:: [Figur] -> String -> Char -> [Move] 
enPassantCheck b s c = enPassantMoveCheck b c (enPassantStringCheck s c)

--Checks if there is a pawn on the coordinates from which one could take with en Passant
ePFCHelper :: [Figur] -> (Int, Int) -> Char -> Bool
ePFCHelper b cc c = not (Prelude.null [figur | figur /= F2 && name figur == "pawn"]) 
                where figur = if fList /= [] then fList!!0 else F2
                        where fList = getFigurFromTile b cc c

--Uses the helper function above to check for every single coordinate pair that was given, if there is a pawn that can capture
-- and returns a list of possible en Passant moves for this turn
enPassantMoveCheck :: [Figur] -> Char -> [(Int,Int)] -> [Move]
enPassantMoveCheck _ _ [] = []
enPassantMoveCheck b c [x] = if ePFCHelper b x c then (if fst x == 1 then [EnPassantR] else [EnPassantL]) else []
enPassantMoveCheck b c (c1:c2:_) = [EnPassantL | ePFCHelper b c1 c] ++ [EnPassantR | ePFCHelper b c2 c]

-- Checks the moves saved in the memory figure
-- to see if the last move allowed for en Passant to be possible now
enPassantStringCheck :: String -> Char -> [(Int,Int)]
enPassantStringCheck s c = if (member (drop (length s-4) s) colorMap && not((take 2 (drop (length s-4) s)) `isInfixOf`(take (length s-4) s)))
                        then colorMap ! (drop (length s-4) s) else [] 
                         where colorMap = (if (c == 'w') then enPassantMapWhite else enPassantMapBlack)

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
        |(name f == "pawn") && (isInvalidPawnMove b amZug move) = False
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
            
-- Creates a list of all possible special Moves for the current turn
specialMoves :: [Figur] -> [Move]
specialMoves b = castleCheck b (name memory) (color memory) ++ enPassantCheck b (name memory) (color memory) 
                 where memory = head b

-- This function checks if a rook has moved or if it is blocked.
-- If either of those two is true, then the rook cannot be used for Castling.
rookUnavailable :: [Figur] -> String -> Char -> Int -> Bool
rookUnavailable b m c 0 = hasMoved  m (if (c == 'w') then "A0" else "A7") || isBlocked b (createCoordList (M 'A' v 'E' v)) where v = if (c == 'w') then 0 else 7
rookUnavailable b m c 1 = hasMoved  m (if (c == 'w') then "H0" else "H7") || isBlocked b (createCoordList (M 'H' v 'E' v)) where v = if (c == 'w') then 0 else 7


-- A function that receives a board, a figure, the color which is about to play and a move
-- and returns a boolean.
-- It's purpose is to check if the received move is valid given the current board situation.
isInvalidPawnMove :: [Figur] -> Char -> Move -> Bool
isInvalidPawnMove b c (M x1 y1 x2 y2) = ((x1 /= x2) && (not (isOccupied b tc (convX x2) y2))) || 
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


-- This function takes a move and returns a list of coordinates that can be reached with this move.
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
-- Tests, if the King is currently in check
--(this is used AFTER a move has been played, board will not be used after creation if so)
isCheck :: [Figur] -> (Int, Int) -> Bool
isCheck b (x1,y1) = let s1 = tileScope (x1,y1)
                        s2 = filterByScope s1 b (color(head b))
                        s3 = scopeFigureMoves b s2
                        in doScopedMovesCheck s3 (x1,y1)

--Checks if any of the valid moves, which were created in scopeFigureMoves, attack the King
doScopedMovesCheck :: [Move] -> (Int, Int) -> Bool
doScopedMovesCheck [] _ = False
doScopedMovesCheck (m:ms) t = (convX (xnew m) == fst t && ynew m == snd t) || doScopedMovesCheck ms t

-- Creates all positions, from which a King could be in check in theory by using createScope
tileScope :: (Int, Int) -> [(Int, Int)]
tileScope x = createScope x queenMoves ++ createScope x knightMoves

--Creates the possible positions, from which a certain tile could be attacked, based on a list of coordinates
createScope :: Num b => (b, b) -> [[b]] -> [(b, b)]
createScope x [] = []
createScope x (y:ys) = (fst x + head y, snd x + last y) : createScope x ys

-- Creates a list of figures, which might be able to attack the king,
-- based on the list of positions created in tileScope
filterByScope :: [(Int, Int)] -> [Figur] -> Char -> [Figur]
filterByScope [] _ _ = []
filterByScope (t:ts) b c = figurAtTile ++ filterByScope ts b c
                where figurAtTile       | isOccupied b c (fst t) (snd t) = getFigurFromTile b t c
                                        | otherwise = []

--Creates a list of all valid moves for the figures that were found in filterByScope
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

--Creates a list of valid moves for every single figure on the board, after checking if they are legitimate
makeMoveList :: [Figur] -> Figur -> [[Int]] -> [Move]
makeMoveList _ _ [] = []
makeMoveList b f (m:ms) = moveCheckResult ++ makeMoveList b f ms
                where moveCheckResult   | (validMove b f currentMove) =  [currentMove]
                                        | otherwise = []
                                                where currentMove = M (convToX(x f)) (y f) (convToX (x f + m!!0)) (y f + m!!1)


















------------------------------------------------------ Create Boards -------------------------------------




-- A function that accepts a board
-- and returns a list of boards. 
-- It creates all the variations of a given board
-- considering the moves that the figures on the board are able to make.
createBoardVariations :: [Figur] -> [Figur] -> [[Figur]]
createBoardVariations _ [] = []
createBoardVariations b (x:xs) = moveFigure b moveList ++ createBoardVariations b xs
        where moveList = if (y x == (-1)) then (specialMoves b) else (makeMoveList b x moves) --Specialmoves für y x (-1) einfügen
                where moves | name x == "knight" = knightMoves
                            | name x == "rook" = rookMoves
                            | name x == "bishop" = bishopMoves
                            | name x == "queen" = queenMoves
                            | name x == "king" = kingMoves
                            | (name x == "pawn") && (color x == 'w') = pawnMovesW
                            | (name x == "pawn") && (color x == 'b') = pawnMovesB
                            | otherwise = []


-- A recursive function call of createBoardVariations.
-- It receives a list of boards and
-- returns a list of boards.
createAllBoardVariations :: [[Figur]] -> [[Figur]]
createAllBoardVariations [] = []
createAllBoardVariations [b] = createBoardVariations b b
createAllBoardVariations (b:bs) = (createBoardVariations b b) ++ createAllBoardVariations bs


--A list of preset moves, which help the engine get a stable start for any given game, as it gets close to castling.
--The list is fairly small, as after a few moves trades could occurr, which require the engine's own decisions.
openingBook :: String -> String
openingBook s | turnNumber == 1 = "G1G2"
              | turnNumber == 2 = "G6G5"
              | turnNumber == 3 = "F0G1"
              | turnNumber == 4 = if ("C0B1" `isInfixOf` trimString s) then "G7F5" else "F7G6"
              | turnNumber == 5 = if (("E4E3" `isInfixOf` trimString s) || ("G4G3" `isInfixOf` trimString s)) then "E1E2" else "G0F2"
              | otherwise = "You shouldn't read this."
        where turnNumber = ((length (trimString s)) `div` 4) + 1

















------------------------------------------------------ Evaluation -------------------------------------

-- A function, that expects a list of boards, the color
-- and returns a board.
-- The returned board is the one with the best evaluation.
chooseBestBoard :: [[Figur]] -> Char -> [Figur]
chooseBestBoard [] _ = []
chooseBestBoard [x] _ = x
chooseBestBoard (x:x2:xs) c | (evaluateChessboard x c + boardQuotient x c 0 0) >= (evaluateChessboard x2 c + boardQuotient x2 c 0 0) = chooseBestBoard (x:xs) c
                            | otherwise = chooseBestBoard (x2:xs) c

-- A simple function, that
-- takes a board and a character, which determines the color
-- and offers a Double, which is interpreted as the value of a board,
-- in terms of material count and material position.
evaluateChessboard :: [Figur] -> Char -> Double
evaluateChessboard [] _ = 0
evaluateChessboard (x:xs) c = y + evaluateChessboard xs c
                            where y | name x == "pawn" = (1+pawnEvaluation x) * i
                                    | name x == "knight" = (3+centerEvaluation x+aggressionEvaluation x) * i
                                    | name x == "bishop" = (3+centerEvaluation x+aggressionEvaluation x + longDiagonal x) * i
                                    | name x == "rook" = (5+centerEvaluation x+aggressionEvaluation x) * i
                                    | name x == "queen" = (9+centerEvaluation x+aggressionEvaluation x) * i
                                    | name x == "king" = 1000 * i
                                    | name x == "castle" = 0.9 * i
                                    | otherwise = 0
                                    where i | color x == c = -1
                                            | otherwise = 1

--Calcualtes a valued based on the ratio of pieces to enemy pieces, which is added to the evaluation.
--This helps push the computer to trade pieces, when it has an advantage.
boardQuotient :: [Figur] -> Char -> Double -> Double -> Double
boardQuotient [] c oben unten = 10 * (oben/unten)
boardQuotient (f:fs) c oben unten = boardQuotient fs c (fst x) (snd x)
            where x = quotientUpdate (oben,unten) (name f) (color f /= c)

--Changes the ratio figure by figure
quotientUpdate :: (Double,Double) -> String -> Bool ->(Double,Double)
quotientUpdate (o,u) name oben | oben = (o+x,u)
                               | otherwise = (o,u+x)
                            where x | name == "pawn" = 1
                                    | name == "knight" = 3
                                    | name == "bishop" = 3
                                    | name == "rook" = 5
                                    | name == "queen" = 9
                                    | name == "king" = 1
                                    | otherwise = 0

-- A function that evaluates a pawn
-- depending on its position on board.
pawnEvaluation :: Figur -> Double
pawnEvaluation p | color p == 'w' && (x p, y p) `elem` defPawnPositionsW = 0.5
                 | color p == 'b' && (x p, y p) `elem` defPawnPositionsB = 0.5
                 | color p == 'w' = int2Double (x p) * 0.05
                 | otherwise = int2Double (7-x p) * 0.05

-- A function that gives extra points 
-- to the evaluation score of a figure
-- if it's positioned at the centre.
centerEvaluation :: Figur -> Double
centerEvaluation p | (x p >=2 && x p <=5) && (y p >= 2 && y p <=5) = 0.25
                   | otherwise = 0

-- A function that gives extra points
-- to the evaluation score of a figure
-- if it's on an aggressive position.     
aggressionEvaluation :: Figur -> Double
aggressionEvaluation p | (x p >=1 && x p <=6) && (y p >= (4-colorDifference) && y p <= (6-colorDifference)) = 0.125
                       | otherwise = 0
                        where colorDifference | color p == 'w' = 0
                                              | otherwise = 3

-- A function that gives extra points
-- to the evaluation score of a bishop
-- if it's on the edges of the board.
longDiagonal :: Figur -> Double
longDiagonal p | (x p == 6 && y p == 1 && color p == 'w') || (x p == 6 && y p == 6 && color p == 'b') = 0.5 
               | x p == 1 && (y p == 1 || y p == 6) = 0.3
               | otherwise = 0






------------------------------------------------------ Recursion/Depth -------------------------------------

-- creates all the board variations possible
-- given a number x, which is a natural number but not zero.
-- The number x represents the depth of the chess engine's forecasting ability.
-- The chess engine can currently generate boards for a depth up to 4, within minutes.  
createDepthBased :: [[Figur]] -> Int -> [[Figur]]
createDephtBased [] _ = []
createDepthBased b 1 = b ++ createAllBoardVariations b
createDepthBased b x = b ++ createDepthBased (createAllBoardVariations b) (x-1)


-- A function that takes a list of boards, the depth
-- and returns the number of boards per depth
createLengthList :: [[Figur]] -> Int -> [Int]
createLengthList [] _ = []
createLengthList b 0 = [filterByLength b (length (name (head (head b))) `div` 4)]
createLengthList b x = [y] ++ createLengthList (drop y b) (x-1)
                        where y = filterByLength b val
                                where val = length (name (head (head b))) `div` 4


-- A function that receives 2 lists of boards, the color
-- and returns a list of boards.
-- ******************************************
boardComparator :: [[Figur]] -> [[Figur]] -> Char -> [[Figur]]
boardComparator [] _ _ = []
boardComparator _ [] _ = []
boardComparator b (sb:sbs) c = chooseBestBoard y c : boardComparator (drop (length y) b) sbs c
                                where y = (filterByMove (take 100 b) (name (head sb)))
--Creates a Tuple. The first part of the tuple are all the boards creates up to a depth of x.
--The second part of the tuple is a the list, which contains the amount of boards per depth.
calcSetup :: [Figur] -> Int -> ([[Figur]],[Int])
calcSetup b x = (newB,createLengthList newB x) where newB = reverse (createDepthBased [b] x)

calcDepthBased :: ([[Figur]],[Int]) -> Int -> ([[Figur]],[Int])
calcDepthBased ([], _) _ = ([],[])
calcDepthBased (_, []) _ = ([],[])
calcDepthBased (b,l) 1 = ([chooseBestBoard (init b) (color(head(head b)))], l)
calcDepthBased (b,l) x = calcDepthBased (boardUpdate ++ (drop (l!!0+l!!1) b),(drop 1 l)) (x-1)
                where boardUpdate = colorSwap (Prelude.filter (not.Prelude.null) (boardComparator (take (l!!0) b) (take (l!!1) (drop (l!!0) b)) (color(head(head b)))))

--Executes recursive best-move-calculation and returns the calculated move in the form of a String
cpuMove :: String -> Int -> String
cpuMove s i = if (length (s) <= 20) then openingBook s else (getMovesFromMemory calcResult i)
        where calcResult = head(fst (calcDepthBased (calcSetup b1 i) i))
                where b1 = updateBoardAll starterBoard (convMoves s)















---------------------------------------------------- Utility Functions -----------------------------------------------

-- Gets the moves from the memory figure
-- in form of a String.
getMovesFromMemory :: [Figur] -> Int -> String
getMovesFromMemory b x = reverse (take 4 (drop ((x-1)*4) (reverse (name (head b)))))


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
                  | s == "EPLT" = EnPassantL
                  | s == "EPRT" = EnPassantR

-- Converts a String to a list of moves.
convMoves :: String -> [Move]
convMoves [] = []
convMoves s = [convToMove (take 4 s)] ++ x
             where x    | length s == 4 = []
                        | otherwise = convMoves (drop 5 s) 

-- Converts a move to a String.
convFromMove :: Move -> [Char]
convFromMove (M a b c d) = [a] ++ [convToX (b-17)]++[c] ++ [convToX (d-17)]

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


-- Returns the played moves from the memory figures of every board in a list of boards
-- and merges them, resulting in a list of move sequences.
pullMemory :: [[Figur]] -> [String]
pullMemory [] = []
pullMemory (b:bs) = name (head b) : pullMemory bs

-- Returns a list of the amount of figures each board in a list of boards has.
pullLength :: [[Figur]] -> [Int]
pullLength [] = []
pullLength (b:bs) = length b : pullLength bs

--Checks how many boards have a certain amount of moves played
filterByLength :: [[Figur]] -> Int -> Int
filterByLength [] _ = 0
filterByLength (b:bs) x = boardCheck + filterByLength bs x
                        where boardCheck | (length (name (head b))) == x*4 = 1
                                         | otherwise = 0
-- Returns all boards, which have played all moves up to a certain point
filterByMove :: [[Figur]] -> String -> [[Figur]]
filterByMove [] _ = []
filterByMove (b:bs) s = boardCheck ++ filterByMove bs s
                where boardCheck | (s == (take (length s)(name (head b)))) = [b]
                                 | otherwise = []

--Changes the string/chars for moves, so that there is consistency between internal logic and external input
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

-- A function that, given a string that contains moves
-- and a string that represents a single move,
-- checks if this single string is a substring of the other one.
-- This essentially means that a figure has been moved.
hasMoved :: String -> String -> Bool
hasMoved moves pos = pos `isInfixOf` moves

--Get a figure based on coordinates. If there is none, returns an empty list
getFigurFromTile :: [Figur] -> (Int, Int) -> Char -> [Figur]
getFigurFromTile [] _ _ = []
getFigurFromTile (f:fs) (tx,ty) c = if (x f == tx && y f == ty && color f == c && (x f >= 0)) then [f] else getFigurFromTile fs (tx,ty) c

--Returns the coordinates of the first tile, which has a figure of the chosen name and color
--Returns (8,8) if there is no such figure, as this tile can never be occupied.
getTileFromName :: [Figur] -> String -> Char-> (Int, Int)
getTileFromName [] _ _ = (8,8)
getTileFromName (f:fs) s c = if (name f == s && c == color f) then (x f, y f) else getTileFromName fs s c