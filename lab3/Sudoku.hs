import Test.QuickCheck
import Data.Char
import Data.List

data Sudoku = Sudoku { rows :: [[Maybe Int]] } deriving Show

-- A1
-- Create a 9x9 blank sudoku
-- number of rows and columns can be changed
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (blankSudoku 9 9)
  where blankSudoku x y = replicate x (replicate y n)
        n = Nothing

-- A2
-- Check if input is a legit sudoku
isSudoku :: Sudoku -> Bool
isSudoku sudoku = (all (\x -> length x == 9) s) && (length s == 9) && (all (\x -> checkAllValue x) s)
  where s = (rows sudoku)

checkAllValue :: [Maybe Int] -> Bool
checkAllValue list = all (\x -> checkOneValue x) list
  where checkOneValue (Just x) = x > 0 && x < 10
        checkOneValue Nothing = True

-- A3
-- Check if the sudoku is filled with numbers
isFilled :: Sudoku -> Bool
isFilled sudoku = all (\x -> checkAllFilled x) s
  where s = (rows sudoku)

checkAllFilled :: [Maybe Int] -> Bool
checkAllFilled list = all (\x -> checkOneFilled x) list
  where checkOneFilled Nothing = False
        checkOneFilled _ = True

-- B1
-- Print the Sudoku
printSudoku :: Sudoku -> IO()
printSudoku sudoku = putStrLn (concat (map oneRowToStr s))
  where s = (rows sudoku)

-- Transforms one  sudoku row to a readable string
oneRowToStr :: [Maybe Int] -> String
oneRowToStr [] = "\n\n"
oneRowToStr (x:xs) = (maybeToStr x) ++ oneRowToStr xs
  where maybeToStr (Just x) = (show x) ++ "\t"
        maybeToStr Nothing =".\t"

-- B2
-- Reads a sudoku from a file
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do sudoku <- readFile f
                  return (divideSudoku (lines sudoku))

divideSudoku :: [[Char]] -> Sudoku
divideSudoku s = Sudoku (map row s)
  where row l = (map temp2 l)
        temp2 '.' = Nothing
        temp2 c = Just(digitToInt c)

-- C1
-- Generator for one cell
cell :: Gen (Maybe Int)
cell = frequency [(9,return Nothing),
                   (1, do
                     n <- randInt
                     return (Just n))]

randInt :: Gen Int
randInt = elements [1..9]

-- C2
-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- C3
-- Checks if a sudoku has correct size and has valid values
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

-- Represents a row, column or a 3x3 block
type Block = [Maybe Int]

-- D1
-- Returns true if a block has no duplicates
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (Nothing:xs) = isOkayBlock xs
isOkayBlock (x:xs) = if (x `elem` xs) then False
  else (isOkayBlock xs)

-- D2
-- Returns an array of all different blocks that exists in a sudoku (27 blocks)
blocks :: Sudoku -> [Block]
blocks sudoku = s  ++ (transpose s) ++ (rows' s)
  where
      s = (rows sudoku)
      rows' [] = []
      rows' x = (cells (transpose (take 3 x))) ++ rows' (drop 3 x)
      cells [] = []
      cells x = (concat (take 3 x)):(cells (drop 3 x))

-- D3
-- Checks that no block in a sudoku contains the same digit twice
isOkay :: Sudoku -> Bool
isOkay s =  all (\x -> isOkayBlock x) (blocks s)
