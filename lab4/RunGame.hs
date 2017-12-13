module RunGame where

import System.Console.ANSI
import System.IO
import System.Random
import Prelude hiding (Either(..))
import Data.Colour
import Data.Colour.SRGB
import DataTypes

data Input = Up | Down | Left | Right | Open | Exit
               deriving (Eq,Show)

data Interface = Interface
  { iAllBlankBoard    :: Board,
    iRandomBoard      :: StdGen -> Board,
    iOpenSpace        :: Board -> Coord -> Board,
    iIsCoordValid     :: Coord -> Bool,
    iValueAtCoord     :: Board -> Coord -> Tile,
    iCalcBlankValues  :: Board -> Board,
    iPlayerHasWon     :: Board -> Bool,
    iAddCoords        :: Coord -> Coord -> Coord,
    iRevealBoard      :: Board -> Board
  }

runGame :: Interface -> IO()
runGame i = do
  b <- makeBoard i
  let board = iCalcBlankValues i b
  drawGame board
  setCursorPosition 0 4
  gameLoop i board (0,4)

makeBoard :: Interface -> IO Board
makeBoard i = do
  g <- newStdGen
  return (iRandomBoard i g)

gameLoop :: Interface -> Board -> Coord -> IO ()
gameLoop i b marker = do
  drawMarker marker
  if iPlayerHasWon i b then handleWin i b else handleInput i b marker

handleInput :: Interface -> Board -> Coord -> IO ()
handleInput i b m = do
  input <- getInput
  case input of
    Exit -> handleLoose i b
    Open -> if iIsCoordValid i (markerToBoardCoord m)
            then tryOpen i b m
            else gameLoop i b m
    _    -> handleDir i b m input

tryOpen :: Interface -> Board -> Coord -> IO ()
tryOpen i b m = case iValueAtCoord i b (markerToBoardCoord m) of
    Mine -> handleLoose i b
    _    -> handleOpen i b m

handleOpen :: Interface -> Board -> Coord -> IO ()
handleOpen i b (mX, mY) = do
  let newBoard = iOpenSpace i b (markerToBoardCoord (mX,mY)) :: Board
  drawGame newBoard
  setCursorPosition mX mY
  gameLoop i newBoard (mX, mY)

markerToBoardCoord :: Coord -> Coord
markerToBoardCoord (x,y) = ((x+1) `div` 3, (y-2) `div` 2)

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
  putStrLn "q - Exit"
  hSetEcho stdin False
  printBoard b

drawMarker :: Coord -> IO()
drawMarker (xMarker, yMarker) = setCursorPosition yMarker xMarker

handleDir :: Interface -> Board -> Coord -> Input -> IO ()
handleDir i b marker input = gameLoop i b newMarker
  where newMarker = iAddCoords i marker (dirToCoord input)

dirToCoord :: Input -> Coord
dirToCoord d
  | d == Up    = (0, -1)
  | d == Down  = (0,  1)
  | d == Left  = (-3, 0)
  | d == Right = (3,  0)
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

printBoard :: Board -> IO ()
printBoard board = sequence_ $ concat [putChar '\n':[printCell cell | cell <- row ] | row <- board]

printCell :: Cell -> IO ()
printCell (Cell Opened (Numeric i)) = do
  --
  setSGR [ Reset,
    SetRGBColor Foreground (getColor i)]
  putStr $ show (Cell Opened (Numeric i)) ++ "  "
  setSGR [ Reset ]

printCell c = do
  setSGR [ Reset ]
  putStr $ show c ++ "  "
  setSGR [ Reset ]


getColor 8 = sRGB 1.0 0.0 0.0
getColor 7 = sRGB 1.0 0.1 0.0
getColor 6 = sRGB 1.0 0.2 0.0
getColor 5 = sRGB 1.0 0.3 0.0
getColor 4 = sRGB 1.0 0.4 0.0
getColor 3 = sRGB 1.0 0.6 0.2
getColor 2 = sRGB 1.0 0.8 0.6
getColor _ = sRGB 1.0 1.0 1.0

drawEndBoard :: Interface -> Board -> IO ()
drawEndBoard i b = do
  (drawGame . iRevealBoard i) b
  setCursorPosition 40 0
  showCursor

handleWin :: Interface -> Board -> IO ()
handleWin i b = do
  drawEndBoard i b
  setSGR [Reset, SetConsoleIntensity BoldIntensity]
  putStrLn "You win!!"
  putStrLn "Lucky bastard"
  setSGR [ Reset ]

handleLoose :: Interface -> Board -> IO ()
handleLoose i b = do
  drawEndBoard i b
  setSGR [Reset, SetConsoleIntensity BoldIntensity]
  putStrLn "You loose sucker!"
  putStrLn "Better luck next time!"
  setSGR [ Reset ]
