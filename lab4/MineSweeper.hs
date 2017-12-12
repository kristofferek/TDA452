import System.Random
import System.Console.ANSI
import Prelude hiding (Either(..))
import System.IO

data Tile = Mine | Numeric Int deriving Eq
data Status = Hidden | Opened deriving (Eq,Show)
data Cell = Cell {status :: Status, tile :: Tile}
type Board = [[Cell]]
type Coord = (Int, Int)
data Input = Up
           | Down
           | Left
           | Right
           | Open
           | Exit
           deriving (Eq,Show)


main :: IO()
main = do
  clearScreen
  showCursor
  setCursorPosition 0 0
  setSGR [SetColor Background Vivid Red]
  setSGR [SetColor Foreground Vivid Black]
  putStrLn "\t\tMine Sweeper\n"
  setSGR [Reset]
  hSetEcho stdin False
  printBoard allBlankBoard
  setCursorPosition 0 2
  gameLoop allBlankBoard (0,2)

instance Show Tile where
  show Mine = "💥"
  show (Numeric i) = show i

instance Show Cell where
  show (Cell Hidden _) = "."
  show (Cell _ tile) = show tile

gameLoop :: Board -> Coord -> IO ()
gameLoop b marker = do
  drawMarker marker
  input <- getInput
  case input of
    Exit -> handleExit
    Open -> drawOpen b marker
    _    -> handleDir b marker input

drawOpen b m = do
  putChar 'X'
  gameLoop b m

drawMarker :: Coord -> IO()
drawMarker (xMarker, yMarker) = setCursorPosition yMarker xMarker

handleDir :: Board -> Coord -> Input -> IO ()
handleDir b marker input = gameLoop b newMarker
  where newMarker = marker |+| dirToCoord input

dirToCoord :: Input -> Coord
dirToCoord d
  | d == Up    = (0, -2)
  | d == Down  = (0,  2)
  | d == Left  = (-5, 0)
  | d == Right = (5,  0)
  | otherwise  = (0,  0)

getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    '\n'-> return Open
    'w' -> return Up
    's' -> return Down
    'a' -> return Left
    'd' -> return Right
    _ -> getInput

allBlankBoard :: Board
allBlankBoard = replicate 9 (replicate 9 (Cell Opened Mine))

printBoard :: Board -> IO ()
printBoard board = putStrLn $ concat [printBoard' x | x <- board]
  where printBoard' [] = "\n\n"
        printBoard' (x:xs) = show x ++ "    " ++ printBoard' xs

coordOutOfBound :: Coord -> Bool
coordOutOfBound (x,y) = x < 0 && x > 9 && y < 0 && y > 9

(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

valueAtCoord :: Board -> Coord -> Tile
valueAtCoord board (x,y) = tile $ (board!!y)!!x

statusAtCoord :: Board -> Coord -> Status
statusAtCoord board (x,y) = status $ (board!!y)!!x

openTile :: Board -> Coord -> Board
openTile board (x,y)  | statusAtCoord board (x,y) == Opened = board
                      | valueAtCoord board (x,y) == Mine = newBoard
                      | otherwise = newBoard
    where replace r p = [if iColumn == p
                        then Cell Opened (tile cell)
                        else cell | (iColumn,cell) <- zip [0..] r]
          newBoard = [if iRow == y
                      then replace row x
                      else row | (iRow,row) <- zip [0..] board]

nbrMinesAround :: Board -> Coord -> Int
nbrMinesAround board (x,y) = sum [minesAround' board (iColumn,iRow) | (iColumn,iRow) <- (castProd [x-1,x,x+1] [y-1,y,y+1])]
  where castProd xlist ylist= [(xs,ys)| xs <- xlist, ys <- ylist,(xs /= x || ys /= y)]
        minesAround' board coord | coordOutOfBound coord = 0
                                 | valueAtCoord board coord == Mine = 1
                                 | otherwise = 0

handleExit :: IO ()
handleExit = do
  clearScreen
  setSGR [ Reset ]
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"
