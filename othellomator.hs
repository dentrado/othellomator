
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

place :: Pos -> Color -> Board-> Maybe Board
place pos color board
  | pos `M.member` board = Nothing
  | outOfBounds pos  = Nothing
  | otherwise =
    let directions = [(x,y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]
        nextBoard = foldr (flip pos color) board directions
    in if board == nextBoard then Nothing else Just (M.insert pos color nextBoard)

initialBoard = M.fromList [((4,4),White),((4,5),Black),((5,4),Black),((5,5),White)]

boardToStr :: Board -> Color -> String
boardToStr b c = (if c == Black then 'B' else 'W'):(map translate positions)
  where positions = [(x,y) | x <- [1..8], y <- [1..8]]
        translate (x,y) = case M.lookup (x,y) b of
          Nothing -> 'E'
          Just Black -> 'X'
          Just White -> 'O'



playLoop :: String -> String -> Board -> Color -> Color



main :: IO ()
main = do args <- getArgs
          case args of
            [white:black:time] ->
            _ -> print "Usage: " ++ getProgName ++ "white_player black_player time"
