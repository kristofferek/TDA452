import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe

newtype Sudoku = Sudoku { rows :: [[Maybe Int]] } deriving Show

example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just
-- A1
-- Create a 9x9 blank sudoku
-- number of rows and columns can be changed
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- A2
-- Check if input is a legit sudoku
isSudoku :: Sudoku -> Bool
isSudoku sudoku = all (\x -> length x == 9) s && length s == 9 && all checkAllValue s
  where s = rows sudoku

checkAllValue :: [Maybe Int] -> Bool
checkAllValue list = all checkOneValue list
  where checkOneValue (Just x) = x > 0 && x < 10
        checkOneValue Nothing = True

-- A3
-- Check if the sudoku is filled with numbers
isFilled :: Sudoku -> Bool
isFilled sudoku = all checkAllFilled s
  where s = rows sudoku

checkAllFilled :: [Maybe Int] -> Bool
checkAllFilled list = all isJust list

-- B1
-- Print the Sudoku
printSudoku :: Sudoku -> IO()
printSudoku sudoku = putStrLn $ concatMap oneRowToStr s
  where s = rows sudoku

-- Transforms one  sudoku row to a readable string
oneRowToStr :: [Maybe Int] -> String
oneRowToStr [] = "\n\n"
oneRowToStr (x:xs) = maybeToStr x ++ oneRowToStr xs
  where maybeToStr (Just x) = show x ++ "\t"
        maybeToStr Nothing =".\t"

-- B2
-- Reads a sudoku from a file
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do sudoku <- readFile f
                  return (divideSudoku (lines sudoku))

divideSudoku :: [String] -> Sudoku
divideSudoku s = Sudoku (map row s)
  where row l = map temp2 l
        temp2 '.' = Nothing
        temp2 c = Just(digitToInt c)

-- C1
-- Generator for one cell
cell :: Gen (Maybe Int)
cell = frequency [(9,return Nothing),
                  (1, elements [Just n | n <- [1..9]])]

-- C2
-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- C3
-- Checks if a sudoku has correct size and has valid values
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-- Represents a row, column or a 3x3 block
type Block = [Maybe Int]

-- D1
-- Returns true if a block has no duplicates
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (Nothing:xs) = isOkayBlock xs
isOkayBlock (x:xs) = notElem x xs && isOkayBlock xs

-- D2
-- Returns an array of all different blocks that exists in a sudoku (27 blocks)
blocks :: Sudoku -> [Block]
blocks sudoku = s  ++ transpose s ++ rows' s
  where
      s = rows sudoku
      rows' [] = []
      rows' x = cells (transpose (take 3 x)) ++ rows' (drop 3 x)
      cells [] = []
      cells x = concat (take 3 x):cells (drop 3 x)

-- D3
-- Checks that no block in a sudoku contains the same digit twice
isOkay :: Sudoku -> Bool
isOkay s =  all isOkayBlock (blocks s)


------------- PART B -----------------------------------------------------------

type Pos = (Int,Int)

-- E1
-- Returns all positios that are still blank in the sudoku
blanks :: Sudoku -> [Pos]
blanks s =
  [(y,x)                             -- generate a Pos pair
    | (y, row) <- zip [0..] (rows s) -- for each row with its coordinate
    , (x, cell) <- zip [0..] row   -- for each tile in the row (with coordinate)
    , isNothing cell]                -- if the cell is nothing

prop_blanks :: Sudoku -> Bool
prop_blanks s = all (prop_blanks' indexedSud) (blanks s)
  where
    indexedSud = zip [0..] $ map (zip [0..]) (rows s)
    --first indexing all cell with x-values and then indexing the rows
    prop_blanks' s (y,x)
      | isNothing (snd (snd (s !! y) !! x)) = True
      | otherwise = False

-- E2
-- Updates the given list with the new value at the given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (index,a) = if index > length l then error "Index out of bounds"
  else [if i == index then a else v | (i,v) <- zip [0..] l]

prop_uList :: Eq a => [a] -> (Int,a) -> Property
prop_uList l (i,v) = i >= 0 && i < length l ==> ((l !!= (i,v)) !! i) == v

-- E3
-- Updates the value at a given position in a sudoku
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s (iRow,iColumn) v =
  Sudoku [if iRow == tempIndex
          then row !!= (iColumn,v)
          else row | (tempIndex,row) <- zip [0..] (rows s)]

prop_update :: Sudoku -> Pos -> Maybe Int -> Property
prop_update s (y,x) i = prop_validPos (y,x) ==>
                        (rows (update s (y,x) i) !! y) !! x == i

prop_validPos :: Pos -> Bool
prop_validPos (y,x) = min y x >= 0 && max y x < 9

-- E4
-- Returns all values that can be legally inserted at a given position
candidates :: Sudoku -> Pos -> [Int]
candidates s pos = [x | x <- [1..9], isOkay (updatedSudoku x)]
  where updatedSudoku x = update s pos (Just x)


-- F1
solve :: Sudoku -> Maybe Sudoku
solve sud | not (isOkay sud && isSudoku sud) = Nothing
          | otherwise = solve' sud (blanks sud)
  where
    solve' s [] = Just s
    solve' s (x:xs)
      | null (candidates s x) = Nothing
      | otherwise = (listToMaybe . catMaybes)
                    [ solve' (update s x (Just c)) xs | c <- candidates s x ]
        -- creates a deep nested list which then flattens when recursion starts
        -- to return up

-- F2
-- takes in a filepath, solves a sudoku and prints it out
readAndSolve :: FilePath -> IO ()
readAndSolve f = do s <- readSudoku f
                    printSudoku $ fromJust $ solve s

-- F3
-- Checks if the first sudoku is a valid solution and returns true if
-- all non-empty cells in the second argument exists in the first argument
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2
  | not (isOkay s1 && isSudoku s1 && isFilled s1) = False
  | otherwise = and $ zipWith
                      isNothingOrEqual ((concat . rows)  s1) ((concat . rows) s2)

isNothingOrEqual :: Maybe Int -> Maybe Int -> Bool
isNothingOrEqual _ Nothing = True
isNothingOrEqual c1 c2 = fromJust c1 == fromJust c2

-- F4
-- Checks if a solution actually is a solution of the original sudoku
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isOkay s ==> (sol `isSolutionOf` s)
  where sol = fromJust $ solve s
