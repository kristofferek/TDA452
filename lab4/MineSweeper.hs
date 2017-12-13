import RunGame

import System.Random
import DataTypes


implementation = Interface
  { iAllBlankBoard    = allBlankBoard,
    iRandomBoard      = randomBoard,
    iOpenSpace        = openSpace,
    iIsCoordValid     = isCoordValid,
    iValueAtCoord     = valueAtCoord,
    iCalcBlankValues  = calcBlankValues,
    iPlayerHasWon     = playerHasWon,
    iAddCoords        = (|+|),
    iRevealBoard      = revealBoard
  }

main :: IO ()
main = runGame implementation

calcBlankValues :: Board -> Board
calcBlankValues b =
  [[Cell (status cell) (value b (x,y) (tile cell))
    | (x, cell) <- zip [0..] row] | (y, row) <- zip [0..] b]
    where
      value board pos Mine = Mine
      value board pos _    = Numeric (nbrMinesAround b pos)

playerHasWon :: Board -> Bool
playerHasWon b = (and . concat) [[ checkCell cell | cell <- row] | row <- b ]
  where
    checkCell (Cell Opened _)    = True
    checkCell (Cell Hidden Mine) = True
    checkCell _                  = False

allBlankBoard :: Board
allBlankBoard = replicate 18 (replicate 18 (Cell Hidden (Numeric 0)))

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
