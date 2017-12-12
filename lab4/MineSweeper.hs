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
  drawGame allBlankBoard
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
    Exit -> handleExit b
    Open -> if isCoordValid marker then tryOpen b marker else handleExit b
    _    -> handleDir b marker input

tryOpen :: Board -> Coord -> IO ()
tryOpen b m = case valueAtCoord b m of
    Mine -> handleExit b
    _    -> handleOpen b m

handleOpen :: Board -> Coord -> IO ()
handleOpen b (mX, mY) = do
  let newBoard = openSpace b (markerToBoardCoord (mX,mY)) :: Board
  drawGame newBoard
  setCursorPosition mX mY
  gameLoop newBoard (mX, mY)

markerToBoardCoord :: Coord -> Coord
markerToBoardCoord (x,y) = ((x+1) `div` 5, (y-2) `div` 2)


drawGame :: Board -> IO ()
drawGame b = do
  clearScreen
  showCursor
  setCursorPosition 0 0
  setSGR [SetColor Background Vivid Red]
  setSGR [SetColor Foreground Vivid Black]
  putStrLn "\t\tMine Sweeper\n"
  setSGR [Reset]
  setSGR [ SetConsoleIntensity BoldIntensity]
  hSetEcho stdin False
  printBoard b


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
allBlankBoard = replicate 9 testRow

testRow :: [Cell]
testRow = concat [replicate 4 (Cell Hidden (Numeric 2)), [Cell Hidden Mine], replicate 4 (Cell Hidden (Numeric 4))]


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

openSpace :: Board -> Coord -> Board
openSpace b c
  | not (canOpen b c) = b
  | otherwise = openSpace' b c
  where
    openLeft b (x,y)        = openSpace b (x-1,y)
    openRight b (x,y)       = openSpace (openLeft b (x,y)) (x+1,y)
    openUp b (x,y)          = openSpace (openRight b (x,y)) (x,y-1)
    openDown b (x,y)        = openSpace (openUp b (x,y)) (x,y+1)
    openSpace' b c          = openDown (openTile b c) c

canOpen :: Board -> Coord -> Bool
canOpen b c = isCoordValid c && valueAtCoord b c /= Mine && statusAtCoord b c /= Opened

isCoordValid :: Coord -> Bool
isCoordValid (x,y) = x>=0 && x<9 && y>=0 && y<9

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

revealBoard :: Board -> Board
revealBoard b = [[Cell Opened (tile cell) | cell <- row] | row <- b]

handleExit :: IO ()
handleExit = do
  drawGame (revealBoard b)
  setSGR [ Reset ]
  setCursorPosition 0 0
  showCursor
  putStrLn "You loose sucker!"
  putStrLn "Better luck next time!"
  setSGR [ Reset ]
