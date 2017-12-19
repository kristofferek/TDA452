module RunGame where

import System.Console.ANSI
import System.IO
import Prelude hiding (Either(..))
import Data.Colour
import Data.Colour.SRGB
import DataTypes
import Test.QuickCheck

data Input = Up | Down | Left | Right | Open | Exit | FlagMark
               deriving (Eq,Show)

data Interface = Interface
  { iAllBlankBoard    :: Board,
    iOpenSpace        :: Board -> Coord -> Board,
    iIsCoordValid     :: Coord -> Bool,
    iValueAtCoord     :: Board -> Coord -> Tile,
    iStatusAtCoord    :: Board -> Coord -> Status,
    iCalcBlankValues  :: Board -> Board,
    iPlayerHasWon     :: Board -> Bool,
    iAddCoords        :: Coord -> Coord -> Coord,
    iRevealBoard      :: Board -> Board,
    iFlagTile         :: Board -> Coord -> Board,
    iHiddenBlanks     :: Board -> Int
  }

runGame :: Interface -> IO()
runGame i = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  b <- generate arbitrary :: IO Board
  let board = iCalcBlankValues i b -- Calculates the right values for all blanks
  drawGame i board
  setCursorPosition 0 4
  gameLoop i board (0,4)

gameLoop :: Interface -> Board -> Coord -> IO ()
gameLoop i b marker = do
  drawMarker marker
  if iPlayerHasWon i b then handleWin i b else handleInput i b marker

-- Redirects input to the right method
handleInput :: Interface -> Board -> Coord -> IO ()
handleInput i b m = do
  input <- getInput
  case input of
    Exit     -> handleLoose i b
    Open     -> if iIsCoordValid i (markerToBoardCoord m)
                      && iStatusAtCoord i b (markerToBoardCoord m) /= Opened
                then tryOpen i b m
                else gameLoop i b m
    FlagMark -> if iIsCoordValid i (markerToBoardCoord m)
                then handleFlagMark i b m
                else gameLoop i b m
    _        -> handleDir i b m input

-- If the selected cell is a mine the player loose, else we open it
tryOpen :: Interface -> Board -> Coord -> IO ()
tryOpen i b m = case iValueAtCoord i b (markerToBoardCoord m) of
    Mine -> handleLoose i b
    _    -> handleOpen i b m

-- Opens up the blank space around the coordinate
handleOpen :: Interface -> Board -> Coord -> IO ()
handleOpen i b (mX, mY) = do
  let newBoard = iOpenSpace i b (markerToBoardCoord (mX,mY))
  drawGame i newBoard
  setCursorPosition mX mY
  gameLoop i newBoard (mX, mY)

-- Moves the marker in the given direction
handleDir :: Interface -> Board -> Coord -> Input -> IO ()
handleDir i b marker input = gameLoop i b newMarker
  where newMarker = iAddCoords i marker (dirToCoord input)

-- Tries to mark a cell. Do nothing if it's opened
handleFlagMark :: Interface -> Board -> Coord -> IO ()
handleFlagMark i b m = case iStatusAtCoord i b (markerToBoardCoord m) of
    Opened      -> gameLoop i b m
    _           -> flagCell i b m

-- Marks a specific cell with a flag. If it's flagged then unflag it
flagCell :: Interface -> Board -> Coord -> IO ()
flagCell i b (mX, mY) = do
  let (boardX, boardY) = markerToBoardCoord (mX, mY)
  let newBoard = iFlagTile i b (boardX, boardY)
  putStr $ show $ (rows newBoard !! boardY) !! boardX
  setCursorPosition mX mY
  gameLoop i newBoard (mX, mY)

markerToBoardCoord :: Coord -> Coord
markerToBoardCoord (x,y) = ((x+1) `div` 3, y-4)

drawMarker :: Coord -> IO()
drawMarker (xMarker, yMarker) = setCursorPosition yMarker xMarker

-- Draws the whole Game UI
drawGame :: Interface -> Board -> IO ()
drawGame i b = do
  setSGR [ Reset ]
  clearScreen
  showCursor
  setCursorPosition 0 0
  setSGR [SetColor Background Vivid Black,
          SetColor Foreground Vivid Green,
          SetConsoleIntensity BoldIntensity]
  putStrLn "\t\tMINESWEEPER\n"
  setSGR [Reset, SetConsoleIntensity BoldIntensity]
  putStrLn "q - Exit\tSpace - Flag/Unflag Cell\tEnter - Open Cell"
  hSetEcho stdin False
  drawBoard b
  putStr "\n\nSafe cells left: "
  print $ iHiddenBlanks i b

-- Draws the board
drawBoard :: Board -> IO ()
drawBoard board = sequence_ $ concat [putChar '\n':[drawCell cell | cell <- row ] | row <- rows board]

drawCell :: Cell -> IO ()
drawCell (Cell Opened (Numeric i)) = do
  setSGR [ Reset, SetRGBColor Foreground (getColor i)]
  putStr $ show (Cell Opened (Numeric i)) ++ "  "
  setSGR [ Reset ]

drawCell c = do
  setSGR [ Reset ]
  putStr $ show c ++ "  "
  setSGR [ Reset ]

-- A tile with many mines around them are printed with a more red color
getColor 8 = sRGB 1.0 0.0 0.0
getColor 7 = sRGB 1.0 0.1 0.0
getColor 6 = sRGB 1.0 0.2 0.0
getColor 5 = sRGB 1.0 0.3 0.0
getColor 4 = sRGB 1.0 0.4 0.0
getColor 3 = sRGB 1.0 0.6 0.2
getColor 2 = sRGB 1.0 0.8 0.6
getColor _ = sRGB 1.0 1.0 1.0 -- 1 or no mines has a white color

drawEndBoard :: Interface -> Board -> IO ()
drawEndBoard i b = do
  (drawGame i . iRevealBoard i) b
  setCursorPosition 25 0
  showCursor

handleWin :: Interface -> Board -> IO ()
handleWin i b = do
  drawEndBoard i b
  setSGR [Reset, SetConsoleIntensity BoldIntensity]
  putStrLn "You win!!"
  putStrLn "Beginners Luck!"
  handleRestart i

handleLoose :: Interface -> Board -> IO ()
handleLoose i b = do
  drawEndBoard i b
  setSGR [Reset, SetConsoleIntensity BoldIntensity]
  putStrLn "You loose!"
  putStrLn "Better luck next time!"
  handleRestart i

handleRestart :: Interface -> IO ()
handleRestart i = do
  putStrLn "Do you want to play again? (y/n)"
  setSGR [ Reset ]
  char <- getChar
  case char of
    'y' -> runGame i
    'n' -> return ()
    'q' -> return ()
    _   -> handleRestart i

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
    ' ' -> return FlagMark
    'w' -> return Up
    's' -> return Down
    'a' -> return Left
    'd' -> return Right
    _ -> getInput
