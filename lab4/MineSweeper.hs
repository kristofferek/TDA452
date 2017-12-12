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
data Input = Up | Down | Left | Right | Open | Exit
               deriving (Eq,Show)

main :: IO()
main = do
  b <- makeBoard
  let board = calcBlankValues b
  drawGame board
  setCursorPosition 1 2
  gameLoop board (1,2)

instance Show Tile where
  show Mine = "💣"
  show (Numeric 0) = " "
  show (Numeric i) = show i


instance Show Cell where
  show (Cell Hidden _) = "■"
  show (Cell _ tile) = show tile

calcBlankValues :: Board -> Board
calcBlankValues b =
  [[Cell (status cell) (value b (x,y) (tile cell))
    | (x, cell) <- zip [0..] row] | (y, row) <- zip [0..] b]
    where
      value board pos Mine = Mine
      value board pos _    = Numeric (nbrMinesAround b pos)

gameLoop :: Board -> Coord -> IO ()
gameLoop b marker = do
  drawMarker marker
  if playerHasWon b then handleWin b else handleInput b marker

handleInput :: Board -> Coord -> IO ()
handleInput b m = do
  input <- getInput
  case input of
    Exit -> handleLoose b
    Open -> if isCoordValid (markerToBoardCoord m)
            then tryOpen b m
            else gameLoop b m
    _    -> handleDir b m input

playerHasWon :: Board -> Bool
playerHasWon b = (and . concat) [[ checkCell cell | cell <- row] | row <- b ]
  where
    checkCell (Cell Opened _)    = True
    checkCell (Cell Hidden Mine) = True
    checkCell _                  = False

tryOpen :: Board -> Coord -> IO ()
tryOpen b m = case valueAtCoord b (markerToBoardCoord m) of
    Mine -> handleLoose b
    _    -> handleOpen b m

handleOpen :: Board -> Coord -> IO ()
handleOpen b (mX, mY) = do
  let newBoard = openSpace b (markerToBoardCoord (mX,mY)) :: Board
  drawGame newBoard
  setCursorPosition mX mY
  gameLoop newBoard (mX, mY)

markerToBoardCoord :: Coord -> Coord
markerToBoardCoord (x,y) = ((x+3) `div` 5, (y-2) `div` 2)

drawGame :: Board -> IO ()
drawGame b = do
  setSGR [ Reset ]
  clearScreen
  showCursor
  setCursorPosition 0 0
  setSGR [SetColor Background Vivid Red,
          SetColor Foreground Vivid Black,
          SetConsoleIntensity BoldIntensity]
  putStrLn "\t\tMine Sweeper\n"
  setSGR [Reset]
  setSGR [SetColor Background Vivid White,
          SetColor Foreground Vivid Black,
          SetConsoleIntensity BoldIntensity]
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
allBlankBoard = replicate 18 (replicate 18 (Cell Hidden (Numeric 0)))

makeBoard :: IO Board
makeBoard = do
  g <- newStdGen
  return (randomBoard g)

randomBoard :: StdGen -> Board
randomBoard g = randomBoard' g 18
  where
    randomBoard' g' 0 = []
    randomBoard' g' rowCount = randRow:randomBoard' g'' (rowCount-1)
      where
        (randRow,g'') = randomRow g'

randomRow :: StdGen -> ([Cell], StdGen)
randomRow g = (randomRow' g 18, stepGen g 18)
  where
    stepGen gR 0 = gR
    stepGen gR ind = stepGen gR' (ind-1)
      where (_, gR') = randomR (0,10) gR :: (Int,StdGen)

    randomRow' g' 0 = []
    randomRow' g' cellCount = Cell Hidden tile:randomRow' g'' (cellCount-1)
      where
        (i, g'') = randomR (0,10) g' :: (Int,StdGen)
        tile = if i <= 3 then Mine else Numeric 0

printBoard :: Board -> IO ()
printBoard board = putStrLn $ " " ++ init (concat [printBoard' x | x <- board])
  where printBoard' [] = "\n " ++ replicate (5*18) ' ' ++"\n "
        printBoard' (x:xs) = show x ++ "    " ++ printBoard' xs

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
isCoordValid (x,y) = x>=0 && x<18 && y>=0 && y<18

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
nbrMinesAround board (x,y) = sum [minesAround' board (iColumn,iRow) | (iColumn,iRow) <- cartisProd [x-1,x,x+1] [y-1,y,y+1]]
  where cartisProd xlist ylist= [(xs,ys)| xs <- xlist, ys <- ylist,xs /= x || ys /= y]
        minesAround' board coord | not (isCoordValid coord) = 0
                                 | valueAtCoord board coord == Mine = 1
                                 | otherwise = 0

revealBoard :: Board -> Board
revealBoard b = [[Cell Opened (tile cell) | cell <- row] | row <- b]

drawEndBoard :: Board -> IO ()
drawEndBoard b = do
  (drawGame . revealBoard) b
  setCursorPosition 40 0
  showCursor

handleWin :: Board -> IO ()
handleWin b = do
  drawEndBoard b
  setSGR [Reset, SetConsoleIntensity BoldIntensity]
  putStrLn "You win!!"
  putStrLn "Lucky bastard"
  setSGR [ Reset ]

handleLoose :: Board -> IO ()
handleLoose b = do
  drawEndBoard b
  setSGR [Reset, SetConsoleIntensity BoldIntensity]
  putStrLn "You loose sucker!"
  putStrLn "Better luck next time!"
  setSGR [ Reset ]
