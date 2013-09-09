
import Prelude hiding (flip)
import System.Environment
import System.Process (readProcess)
import qualified Data.Map as M

data Color = Black | White deriving (Eq, Show)
type Pos = (Integer,Integer)
type Dir = (Integer,Integer)
type Board = M.Map Pos Color

opponent Black = White
opponent White = Black

outOfBounds :: Pos -> Bool
outOfBounds (x,y) =
  x < 1 || x > 8 || y < 1 || y > 8

flip :: Pos -> Color -> Dir -> Board -> Board
flip (x,y) c (dx,dy) board =
  case nextPos `M.lookup` board of
    Nothing -> board
    Just c2 -> if c == c2
               then board
               else flip nextPos c (dx,dy) (M.insert nextPos c board)
  where nextPos = (x+dx, y+dy)

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

boardToStr :: Board -> Color -> String
boardToStr b c = (if c == Black then 'B' else 'W'):(map translate positions)
  where positions = [(x,y) | x <- [1..8], y <- [1..8]]
        translate (x,y) = case M.lookup (x,y) b of
          Nothing -> 'E'
          Just Black -> 'X'
          Just White -> 'O'

playLoop :: String -> String -> Color -> Integer -> Board -> Integer -> IO Board
playLoop playerProg oppProg color time board passCount =
  if passCount == 2
  then return board
  else do let nextLoop = playLoop oppProg playerProg (opponent color) time
          move <- readProcess playerProg [boardToStr board color, show time] ""
          putStrLn $ show color ++ " played " ++ show move
          case reads move of
            [(pos,_)] -> nextLoop (place pos color board) 0
            _       -> nextLoop board (passCount + 1)

main :: IO ()
main = do args <- getArgs
          case args of
            [wprog,bprog,time] -> playLoop bprog wprog Black (read time) initialBoard 0 >>= print
            _ -> getProgName >>= \n -> print $ "Usage: " ++ n ++ " white_player black_player time"
