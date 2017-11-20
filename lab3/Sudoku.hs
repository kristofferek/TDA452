import Data.Char

data Sudoku = Sudoku { rows :: [[Maybe Int]] }

example :: Sudoku
example = Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3] ]
  where
    n = Nothing
    j = Just

-- Create a 9x9 blank sudoku
-- number of rows and collumns can be changed

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (blankSudoku 9 9)
  where blankSudoku x y = replicate x (replicate y n)
        n = Nothing

-- Check if input is a legit sudoku

isSudoku :: Sudoku -> Bool
isSudoku sudoku = (all (\x -> length x == 9) s) && (length s == 9) && (all (\x -> checkAllValue x) s)
  where s = (rows sudoku)

checkAllValue :: [Maybe Int] -> Bool
checkAllValue list = all (\x -> checkOneValue x) list
  where checkOneValue (Just x) = x > 0 && x < 10
        checkOneValue Nothing = True

-- Check if the sudoku is filled with numbers

isFilled :: Sudoku -> Bool
isFilled sudoku = all (\x -> checkAllFilled x) s
  where s = (rows sudoku)

checkAllFilled :: [Maybe Int] -> Bool
checkAllFilled list = all (\x -> checkOneFilled x) list
  where checkOneFilled Nothing = False
        checkOneFilled _ = True

-- Print the Sudoku

printSudoku :: Sudoku -> IO()
printSudoku sudoku = putStrLn (concat (map oneRowToStr s))
  where s = (rows sudoku)

oneRowToStr :: [Maybe Int] -> String
oneRowToStr [] = "\n\n"
oneRowToStr (x:xs) = (maybeToStr x) ++ oneRowToStr xs
  where maybeToStr (Just x) = (show x) ++ "\t"
        maybeToStr Nothing =".\t"

readSudoku :: FilePath -> IO Sudoku
readSudoku f = do sudoku <- readFile f
                  return (divideSudoku (lines sudoku))

divideSudoku :: [[Char]] -> Sudoku
divideSudoku s = Sudoku (map row s)
  where row l = (map temp2 l)
        temp2 '.' = Nothing
        temp2 c = Just(digitToInt c)
