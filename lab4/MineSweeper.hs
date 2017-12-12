import System.Random
import System.Console.ANSI
import Prelude hiding (Either(..))
import System.IO
import Test.QuickCheck

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
  gameLoop allBlankBoard (0,2)

instance Show Tile where
  show Mine = "💥"
  show (Numeric 0) = "□"
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
    Open -> handleOpen b marker
    _    -> handleDir b marker input

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
  setCursorPosition 0 2


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
allBlankBoard = replicate 9 (kindaCells)
kindaCells = concat [replicate 4 (Cell Hidden (Numeric 0)), [Cell Hidden Mine], replicate 4 (Cell Hidden (Numeric 4))]

makeBoard :: IO Board
makeBoard = do
  g <- newStdGen
  return randomBoard g

randomBoard :: StdGen -> [[Cell]] -> Board
randomBoard g b rows =

 where
   (i, g') = randomR (0,10) g
   tile = randomTileValue i

randomTileValue :: Tile
randomTileValue i = if i <= 2 then Mine else (Numeric 0)

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

--genCell :: Gen (Tile)
--genCell = frequency  [(8, return (Numeric 0)),
--                    (2, return Mine)]

handleExit :: IO ()
handleExit = do
  clearScreen
  setSGR [ Reset ]
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"
