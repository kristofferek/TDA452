import RunGame

import Test.QuickCheck
import DataTypes


implementation = Interface
  { iAllBlankBoard    = allBlankBoard,
    iOpenSpace        = openSpace,
    iIsCoordValid     = isCoordValid,
    iValueAtCoord     = valueAtCoord,
    iStatusAtCoord    = statusAtCoord,
    iCalcBlankValues  = calcBlankValues,
    iPlayerHasWon     = playerHasWon,
    iAddCoords        = (|+|),
    iRevealBoard      = revealBoard,
    iFlagTile         = flagTile,
    iHiddenBlanks     = hiddenBlanks
  }

main :: IO ()
main = runGame implementation

{-
Calculate the amount of mines around all the blank cells
-}
calcBlankValues :: Board -> Board
calcBlankValues b =
  Board [[Cell (status cell) (value b (x,y) (tile cell))
    | (x, cell) <- zip [0..] row] | (y, row) <- zip [0..] (rows b)]
    where
      value board pos Mine = Mine
      value board pos _    = Numeric (nbrMinesAround b pos)

{-
Return true if there is no mines blank cells left
-}
playerHasWon :: Board -> Bool
playerHasWon b = (and . concat) [[ checkCell cell | cell <- row] | row <- rows b ]
  where
    checkCell (Cell Opened _)    = True
    checkCell (Cell Hidden Mine) = True
    checkCell (Cell Flag Mine)   = True
    checkCell _                  = False

{-
Create a board with only hidden blank cells
-}
allBlankBoard :: Board
allBlankBoard = Board $ replicate 18 $ replicate 18 $ Cell Hidden (Numeric 0)

-- Adds two coords together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

{-
Returns mine or nbr of mines around the cells
-}
valueAtCoord :: Board -> Coord -> Tile
valueAtCoord board (x,y) = tile $ (rows board !! y) !! x

{-
Get the the status of a specific coord
-}
statusAtCoord :: Board -> Coord -> Status
statusAtCoord board (x,y) = status $ (rows board !! y) !! x

{-
Open all the blank cells connected to the inputed cell
-}
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

prop_openSpace :: Board -> Coord -> Bool
prop_openTile b (x,y) = if valueAtCoord b (x',y') == Mine
                        then statusAtCoord (openSpace b (x',y')) (x',y') == Hidden
                        else statusAtCoord (openSpace b (x',y')) (x',y') == Opened
  where
    x' = x `mod` 18
    y' = y `mod` 18

{-
Return true if the coord is inside the board
-}
canOpen :: Board -> Coord -> Bool
canOpen b c = isCoordValid c && valueAtCoord b c /= Mine && statusAtCoord b c /= Opened

isCoordValid :: Coord -> Bool
isCoordValid (x,y) = x>=0 && x<18 && y>=0 && y<18

{-
Open the choosen cell
-}
openTile :: Board -> Coord -> Board
openTile board (x,y)  | statusAtCoord board (x,y) == Opened = board
                      | otherwise = Board newBoard
    where
      newBoard = [if iRow == y
                  then replace row x
                  else row | (iRow,row) <- zip [0..] (rows board)]
      replace r p = [if iColumn == p
                     then Cell Opened (tile cell)
                     else cell | (iColumn,cell) <- zip [0..] r]

{-
Return the nbr of mines around the choosen cell
-}
nbrMinesAround :: Board -> Coord -> Int
nbrMinesAround board (x,y) = sum [minesAround' board (iColumn,iRow) | (iColumn,iRow) <- cartisProd [x-1,x,x+1] [y-1,y,y+1]]
  where cartisProd xlist ylist= [(xs,ys)| xs <- xlist, ys <- ylist,xs /= x || ys /= y]
        minesAround' board coord | not (isCoordValid coord) = 0
                                 | valueAtCoord board coord == Mine = 1
                                 | otherwise = 0

-- Marks the cell at the given coordinate with a flag
flagTile :: Board -> Coord -> Board
flagTile board (x,y)
  | statusAtCoord board (x,y) == Opened = board
  | otherwise = Board newBoard
    where
      newBoard = [if iRow == y
                  then replace row x
                  else row | (iRow,row) <- zip [0..] (rows board)]
      replace r p = [if iColumn == p
                     then if status cell == Flag
                          then Cell Hidden (tile cell)
                          else Cell Flag (tile cell)
                     else cell | (iColumn,cell) <- zip [0..] r]

-- Returns the number of not opened safe tiles that are left
hiddenBlanks :: Board -> Int
hiddenBlanks b = (sum . concat) [[hiddenBlanks' cell | cell <- row] | row <- rows b]
  where
    hiddenBlanks' (Cell Hidden (Numeric i)) = 1
    hiddenBlanks' _ = 0

{-
Reveal the board
-}
revealBoard :: Board -> Board
revealBoard b = Board [[Cell Opened (tile cell) | cell <- row] | row <- rows b]
