import Prelude hiding (flip)
import System.Environment
import System.Process (readProcess)
import qualified Data.Map as M
import Data.List

data Color = Black | White deriving (Eq, Show)
type Pos = (Integer,Integer)
type Dir = (Integer,Integer)
type Board = M.Map Pos Color

-- # Helpers
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n list = part:(chunk n rest)
  where (part,rest) = splitAt n list

-- # Data
opponent Black = White
opponent White = Black

outOfBounds :: Pos -> Bool
outOfBounds (x,y) = x < 1 || x > 8 || y < 1 || y > 8

-- # Logic
-- Flips pieces in a given direction until a piece of the same color is found.
flip :: Pos -> Color -> Dir -> Board -> Board
flip (x,y) c (dx,dy) board =
  case nextPos `M.lookup` board of
    Nothing -> board
    Just c2 -> if c == c2
               then board
               else flip nextPos c (dx,dy) (M.insert nextPos c board)
  where nextPos = (x+dx, y+dy)

-- Places a piece on the board and flips affected pieces. Throws error on illegal move.
place :: Pos -> Color -> Board-> Board
place pos color board
  | pos `M.member` board = errorMsg
  | outOfBounds pos  = errorMsg
  | otherwise =
    let directions = [(x,y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]
        nextBoard = foldr (flip pos color) board directions
    in if board == nextBoard
       then errorMsg
       else (M.insert pos color nextBoard)
  where errorMsg = error $ "Illegal move " ++ show pos ++ " by " ++ show color ++ "."

initialBoard = M.fromList [((4,4),White),((4,5),Black),((5,4),Black),((5,5),White)]

-- # Interfacing
posToChar :: Board -> Pos -> Char
posToChar board pos = case M.lookup pos board of
  Nothing -> 'E'
  Just Black -> 'X'
  Just White -> 'O'

boardToStr :: Board -> Color -> String
boardToStr b c = (if c == Black then 'B' else 'W'):[posToChar b (x,y) | x <- [1..8], y <- [1..8]]

-- returns (Black count, White count)
pieceCount :: Board -> (Integer, Integer)
pieceCount board = M.foldr count (0,0) board
  where count Black (b,w) = (b+1, w  )
        count White (b,w) = (b  , w+1)

prettyBoard :: Board -> String
prettyBoard board =  (intercalate "\n" $ chunk 8 boardStr) ++
                     "\n(black,white): " ++ show (pieceCount board)
   where boardStr = [posToChar board (x,y) | x <- [1..8], y <- [1..8]]

playLoop :: String -> String -> Color -> Integer -> Board -> Integer -> IO Board
playLoop playerProg oppProg color time board passCount =
  if passCount == 2
  then return board
  else do putStrLn $ prettyBoard board
          let nextLoop = playLoop oppProg playerProg (opponent color) time
          move <- readProcess playerProg [boardToStr board color, show time] ""
          putStrLn $ show color ++ " played " ++ show move
          case reads move of
            [(pos,_)] -> nextLoop (place pos color board) 0
            _       -> nextLoop board (passCount + 1)

main :: IO ()
main = do args <- getArgs
          case args of
            [wprog,bprog,time] -> playLoop bprog wprog Black (read time) initialBoard 0 >>= print . prettyBoard
            _ -> getProgName >>= \n -> print $ "Usage: " ++ n ++ " white_player black_player time"
