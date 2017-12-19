module DataTypes where

import Test.QuickCheck
import Test.QuickCheck.Gen

{-
Create all the necessary data types
-}
data Tile = Mine | Numeric Int deriving Eq

instance Show Tile where
  show Mine = "💣"
  show (Numeric 0) = " "
  show (Numeric i) = show i

data Status = Hidden | Opened | Flag deriving (Eq,Show)

data Cell = Cell {status :: Status, tile :: Tile}

instance Show Cell where
  show (Cell Hidden _) = "■"
  show (Cell Flag _) = "🚩"
  show (Cell _ tile) = show tile

newtype Board = Board {rows :: [[Cell]]} deriving Show

type Coord = (Int, Int)


-- Generator for one cell
cell :: Gen Cell
cell = frequency [(1, return (Cell Hidden Mine)),
                  (2, return (Cell Hidden (Numeric 0)))]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Board where
  arbitrary = do
    grid <- vectorOf 18 (vectorOf 18 cell)
    return (Board grid)
