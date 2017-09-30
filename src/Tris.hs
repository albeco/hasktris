{-# LANGUAGE UnicodeSyntax #-}
{-|
Module      : Tris
Description : a library with TicTacToe types and functions
Copyright   : (c) Alberto Comin, 2017
License     : GPL-3
Maintainer  : albeko@gmail.com
Stability   : experimental

Tris is a text-based TicTacToe game.
To start a game use the function 'startGame' with the
algorithm to use (e.g. "startGame alphabeta").
There are two implemented algorithms: 'negamax', which does
not do any pruning, and 'alphabeta', which does alpha-beta
pruning.
-}
module Tris where

import System.Random
import Control.Monad (guard)
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!?), (//))

-- | 'Player' is defined as Int, with the symbolic constants:
-- 'circle' (1), 'cross' (-1) and 'empty' (0).
-- Each 'Board' cell is assigned to a 'Player',
-- the empty cells are assigned to 'empty'.
type Player = Int

circle, cross, empty :: Player
circle = 1
cross = -1
-- | used to mark empty 'Board' cells.
empty = 0 

-- | a string representation of a 'Player' (\"O\", \"X\" or \" \").
showPlayer :: Player -> String
showPlayer ply | ply == circle = "O"
               | ply == cross  = "X"
               | otherwise     = " "

-- | The TicTacToe 'Board' is represented as an array of cells.
-- A cell can be occupied by 'circle', by 'cross' or be empty (occupied
-- by the 'empty' Player).
newtype Board = Board {getCells :: V.Vector Player} deriving (Eq, Ord)

instance Show Board where
  showsPrec _ (Board brd)
    = showLine . showRow [a,b,c]
    . showLine . showRow [d,e,f]
    . showLine . showRow [g,h,i] . showLine
    where showLine
            = showString (replicate 13 '-') . showChar '\n'
          showRow xs
            = showString "| "
            . showString (intercalate " | " (fmap showPlayer xs))
            . showString " |\n"
          [a,b,c,d,e,f,g,h,i] = V.toList brd

-- | Number of cells in the TicTacToe 'Board' (9).
boardSize :: Int
boardSize = 9

-- | An empty TicTacToe 'Board'.
emptyBoard :: Board
emptyBoard = Board $ V.replicate boardSize empty

-- | List of the 'Board' rows, columns and diagonals.
boardLines :: [(Int, Int, Int)]
boardLines = [ (0,1,2), (3,4,5), (6,7,8)
             , (0,3,6), (1,4,7), (2,5,8)
             , (0,4,8), (2,4,6)]

type Move = Int

-- | A type for the user commands during the game:
-- perform a new move or quit the game.
data Command = Quit | Perform Move deriving (Eq, Show)

type GameScore = Int

-- | The type for encoding the status of the game.
data GameStatus
  = GameDraw
  | GameRunning
  | GameWon Player
  deriving (Eq, Show)

type SearchDepth = Int

-- | The maximum search depth, by default the number of 'Board' cells (9).
maxSearchDepth :: SearchDepth
maxSearchDepth = boardSize

victoryScore, defeatScore, nullScore :: GameScore
-- | The maximum positive score.
victoryScore = maxBound
-- | The minimum negative score,
-- defined so that (@'defeatScore' == - 'victoryScore'@).
defeatScore = minBound + 1
-- | The score indicating a draw.
nullScore = 0

-- | List of free positions on the 'Board'.
moveList :: Board → [Move]
moveList (Board brd)
  = V.toList
  . V.map fst
  . V.filter ((==empty) . snd)
  $ V.zip (V.enumFromN 0 boardSize) brd

-- | does a 'Move' and returns an updated 'Board'.
doMove :: Player → Board → Move → Maybe Board
doMove ply (Board brd) mv 
  = do
    cell <- brd !? mv
    guard (cell == empty)
    pure $ Board (brd // [(mv, ply)])
    
-- | undoes a 'Move' by marking a position as free.
undoMove :: Board → Move → Maybe Board
undoMove (Board brd) mv
  = do
    cell <- brd !? mv
    guard (cell /= empty)
    pure $ Board (brd // [(mv, empty)])

-- | does a 'Move' without checking if the position is free.
unsafeDoMove :: Player → Board → Move → Board
unsafeDoMove ply (Board brd) mv
  = Board $ brd // [(mv, ply)]

-- | undoes a 'Move' without checking if the position is occupied.
unsafeUndoMove :: Board -> Move → Board
unsafeUndoMove (Board brd) mv
  = Board $ brd // [(mv, empty)] 

-- | checks if the 'Board' is full.
checkFull :: Board -> Bool
checkFull (Board brd) = V.all (/=empty) brd

-- | checks a line (row, column or diagonal) to see if a 'Player' has won
checkLine :: Board -> (Int, Int, Int) -> Player
checkLine (Board cells) (x,y,z)
  = if cells V.! x == cells V.! y && cells V.! y == cells V.! z
    then cells V.! x
    else empty

-- | checks all possible lines to see if a 'Player' has won.
checkAllLines :: Board -> [Player]
checkAllLines brd = checkLine brd <$> boardLines

-- | checks if the game is won or the 'Board' is full.
checkGameStatus :: Board -> GameStatus
checkGameStatus brd
  | null winningLines = if checkFull brd
                        then GameDraw
                        else GameRunning
  | otherwise         = GameWon (head winningLines)
  where winningLines = filter (/=empty) (checkAllLines brd)

-- | A trivial evaluation of the 'Board'.
evaluation :: Player -> Board -> GameScore
evaluation ply brd = case checkGameStatus brd of
  GameWon winner -> if winner == ply
                   then victoryScore
                   else defeatScore
  _              -> nullScore

type GameAlgorithm = Player -> SearchDepth -> Board -> GameScore

-- | evaluates a 'Move' using negamax.
negamax :: GameAlgorithm
negamax ply depth brd
  = case checkGameStatus brd of
      GameDraw    -> nullScore
      GameWon wp  -> if ply == wp then victoryScore else defeatScore
      GameRunning | depth == 0 -> evaluation ply brd
                  | otherwise  -> maximum selfScores
  where moves = moveList brd
        nextPos = unsafeDoMove ply brd <$> moves
        opponentScores = negamax (negate ply) (depth-1) <$> nextPos
        selfScores = negate <$> opponentScores

-- | evaluates a 'Move' using negamax with alpha-beta pruning.
alphabeta :: GameAlgorithm
alphabeta = alphabeta' defeatScore victoryScore

-- | A helper function for 'alphabeta'.
alphabeta' ::  GameScore -> GameScore -> GameAlgorithm
alphabeta' alpha beta ply depth brd
  = case checkGameStatus brd of
      GameDraw    -> nullScore
      GameWon wp  -> if ply == wp then victoryScore else defeatScore
      GameRunning | depth == 0 -> evaluation ply brd
                  | otherwise  -> maximizeAB defeatScore alpha beta selfScores
  where moves = moveList brd
        nextPos = unsafeDoMove ply brd <$> moves
        getOppScores  = alphabeta' (-beta) (-alpha) (negate ply) (depth-1)
        selfScores = (negate . getOppScores) <$> nextPos

-- A helper function for 'alphabeta'.        
maximizeAB :: GameScore -> GameScore -> GameScore -> [GameScore] -> GameScore
maximizeAB best _ _ [] = best
maximizeAB best alpha beta (v:vs)
  | alpha >= beta = best
  | otherwise = maximizeAB best' alpha' beta vs
  where best' = max best v
        alpha' = max alpha best'

-- | chooses a random 'Move' from a list of possibilities.
chooseRandomMove :: [Move] -> IO Move
chooseRandomMove lst = do
  n <- randomRIO (0, length lst - 1)
  pure (lst !! n)

-- | chooses a 'Move' using a given version of the negamax algorithm.
chooseMove :: Player -> SearchDepth -> Board -> GameAlgorithm -> IO Move
chooseMove ply depth brd alg
  = chooseRandomMove bestMoves
  where moves = moveList brd
        nextPos = unsafeDoMove ply brd <$> moves
        opponentScores = alg (negate ply) (depth-1) <$> nextPos
        selfScores = negate <$> opponentScores
        maxScore = maximum selfScores
        bestMoves = [mv | (mv, sc) <- zip moves selfScores, sc == maxScore]

-- | parses user commands during a Game.
parseCommand :: Board -> String -> Either String Command
parseCommand _ "q" = Right Quit
parseCommand brd cmd
  = case reads cmd of
      []            -> Left $ "unsupported command: " ++ cmd
      [(row, cmd')] -> if row < 1 || row > 3
                      then Left "row outside range [1,3]"
                      else case reads cmd' of
                        [(col, [])] ->
                          if col < 1 || col >3
                          then Left "column outside range [1,3]"
                          else let mv = (3 * (row - 1) + col - 1)
                               in if mv `elem` moveList brd
                                  then Right $ Perform mv
                                  else Left "invalid move (place occupied)"
                        _           -> Left $ "invalid command: " ++ cmd
                                              
-- | asks the user to provide the next move.       
askMove :: Board -> IO Command
askMove brd = do
  putStrLn "new move (\"row col\" or \"q\" for quit)? "
  yourMove <- getLine
  case parseCommand brd yourMove of
    Right cmd -> pure cmd
    Left msg  -> putStrLn msg >> askMove brd

-- | asks if the user wants to do the first move.
askStart :: IO Bool
askStart = do
  putStrLn "do you want to start (y/n)? "
  reply <- getLine
  case reply of
    "y" -> pure True
    "n" -> pure False
    _   -> putStrLn "please reply \"y\" or \"n\"" >> askStart

-- | starts a game using a given search algorithm.
startGame :: GameAlgorithm -> IO ()
startGame alg = do
  putStrLn ""
  putStrLn "haskTris  -  a TicTacToe game written in Haskell"
  putStrLn "------------------------------------------------"
  putStrLn "Enter moves as \"row column\" e.g. \"1 1\"."
  putStrLn "Rows and columns are counted from the top left"
  putStrLn "corner starting from 1."
  putStrLn "------------------------------------------------"
  putStrLn ""
  randFirstMove <- chooseRandomMove [0..boardSize - 1]
  userStart <- askStart
  let brd = if userStart
            then emptyBoard
            else unsafeDoMove cross emptyBoard randFirstMove
  playgame alg maxSearchDepth brd

-- | The main function of the TicTacToe game.
playgame :: GameAlgorithm -> SearchDepth -> Board -> IO ()
playgame alg initSearchDepth brd = do
  yourMove <- print brd >> askMove brd
  case yourMove of
    Quit             -> finishGame brd
    Perform mv -> do
      let brd' = unsafeDoMove circle brd mv
      if checkGameStatus brd' /= GameRunning
        then finishGame brd'
        else do
          myMove <- chooseMove cross initSearchDepth brd' alg
          let brd'' = unsafeDoMove cross brd' myMove
          if checkGameStatus brd'' /= GameRunning
            then print brd'' >> finishGame brd''
            else playgame alg initSearchDepth brd''
  
-- | handles the end of the game.
finishGame :: Board -> IO ()
finishGame brd = case checkGameStatus brd of
  GameDraw   -> putStrLn "Game even!"
  GameWon wp -> if wp == circle
                then putStrLn "You Won"
                else putStrLn "I Won"
  _          -> print "game interrupted"
