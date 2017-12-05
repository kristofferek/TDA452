data Tile = Mine | Numeric Int deriving Eq
data Status = Hidden | Opened deriving (Eq,Show)
data Cell = Cell {status :: Status, tile :: Tile}
type Pos = (Int,Int)
type Board = [[Cell]]

instance Show Tile where
  show Mine = "*"
  show (Numeric i) = show i

instance Show Cell where
  show (Cell Hidden _) = "."
  show (Cell _ tile) = show tile

allBlankBoard :: Board
allBlankBoard = replicate 9 (replicate 9 (Cell Opened (Numeric 0)))

printBoard :: Board -> IO ()
printBoard board = putStrLn $ concat [printBoard' x | x <- board]
  where printBoard' [] = "\n"
        printBoard' (x:xs) = show x ++ " " ++ printBoard' xs
