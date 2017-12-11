import System.Random
import System.Console.ANSI
import Prelude hiding (Either(..))

data Tile = Mine | Numeric Int deriving Eq
data Status = Hidden | Opened deriving (Eq,Show)
data Cell = Cell {status :: Status, tile :: Tile}
type Board = [[Cell]]
type Coord = (Int, Int)
data Input = Up
           | Down
           | Left
           | Right
           | Exit
           deriving (Eq,Show)


main :: IO()
main = do
  --board <- makeBoard
  --printBoard board
  inp <- getInput
  print inp

instance Show Tile where
  show Mine = "💥"
  show (Numeric i) = show i

instance Show Cell where
  show (Cell Hidden _) = "."
  show (Cell _ tile) = show tile

dirToCoord :: Input -> Coord
dirToCoord d
  | d == Up    = (0, -1)
  | d == Down  = (0,  1)
  | d == Left  = (-1, 0)
  | d == Right = (1,  0)
  | otherwise  = (0,  0)

getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    'w' -> return Up
    's' -> return Down
    'a' -> return Left
    'd' -> return Right
    _ -> getInput

allBlankBoard :: Board
allBlankBoard = replicate 9 (replicate 9 (Cell Hidden (Numeric 9)))

printBoard :: Board -> IO ()
printBoard board = putStrLn $ concat [printBoard' x | x <- board]
  where printBoard' [] = "\n"
        printBoard' (x:xs) = show x ++ " " ++ printBoard' xs

(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

valueAtCoord :: Board -> Coord -> Tile
valueAtCoord board (x,y) = tile $ (board!!y)!!x

statusAtCoord :: Board -> Coord -> Status
statusAtCoord board (x,y) = status $ (board!!y)!!x

openTile :: Board -> Coord -> Board
openTile board (x,y) = [if iRow == y
                        then (replace row x)
                        else row | (iRow,row) <- zip [0..] board]
  where replace r p = [if iColumn == p
                      then (Cell Opened (tile cell))
                      else cell | (iColumn,cell) <- zip [0..] r]

--chainBlanks :: Coord -> Board
