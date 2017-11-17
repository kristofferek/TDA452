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
isSudoku (Sudoku s) = (all (\x -> length x == 9) s) && (length s == 9) && (all (\x -> checkAllValue x) s)

checkAllValue :: [Maybe Int] -> Bool
checkAllValue list = all (\x -> checkOneValue x) list
  where checkOneValue (Just x) = if x > 0 && x < 10 then True else False
        checkOneValue Nothing = True

-- Check if the sudoku is filled with numbers

isFilled :: Sudoku -> Bool
isFilled (Sudoku s) = all (\x -> checkAllFilled x) s

checkAllFilled :: [Maybe Int] -> Bool
checkAllFilled list = all (\x -> checkOneFilled x) list
  where checkOneFilled Nothing = False
        checkOneFilled _ = True

printSudoku :: Sudoku -> IO()
printSudoku (Sudoku s) = putStrLn (concat (map oneRowToStr s))

oneRowToStr :: [Maybe Int] -> String
oneRowToStr [] = "\n"
oneRowToStr (x:xs) = (maybeToStr x) ++ oneRowToStr xs
  where maybeToStr (Just x) = show x
        maybeToStr Nothing ="."
