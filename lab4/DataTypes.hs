module DataTypes where

{-
Create all the necessary data types
-}
data Tile = Mine | Numeric Int deriving Eq

instance Show Tile where
  show Mine = "💣"
  show (Numeric 0) = " "
  show (Numeric i) = show i

data Status = Hidden | Opened deriving (Eq,Show)

data Cell = Cell {status :: Status, tile :: Tile}

instance Show Cell where
  show (Cell Hidden _) = "■"
  show (Cell _ tile) = show tile

type Board = [[Cell]]

type Coord = (Int, Int)
