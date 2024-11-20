module Main where

import Ex4

main 
  -- Print introductory lines
  =putStrLn $ unlines
      [ "Running Exercise4."
      , "You should modify this program as follows:"
      , "It should open and read a file called `input.dat`"
      , "This file contains a number of Ints, each on its own line"
      , "There is a list of functions defined in variable `ops` in Ex4.hs"
      , "Your `ops` list has length N="++nops
      , "The 1st function is applied to the 1st number read,"
      , "The 2nd function is applied to the 2nd number read,"
      , " proceed like this until:"
      , "The "++nops++"th function is applied to the "++nops++"th number read."
      , "Processing then moves back to the 1st function in the list, so..."
      , "The 1st function is applied to the "++nops'++"th number read,"
      , "The 2nd function is applied to the "++nops''++"th number read,"
      , "and so on..."
      , "Continue until all input numbers have been processed."
      , "The resulting numbers should be written, one per line, to `output.dat`"
      ]
  -- Read the numbers from input.dat
--  inputNumbers <- readInputFile "input.dat"
  
  -- Apply the operations
--  let results = applyOperations ops inputNumbers
  
  -- Write the results to output.dat
--  writeOutputFile "output.dat" results
  where
    len    = length ops 
    nops   = show len
    nops'  = show (len + 1)
    nops'' = show (len + 2)

-- Helper function to read input from a file
--readInputFile :: FilePath -> IO [Int]
--readInputFile path = do
--  content <- readFile path
--  return $ map read (lines content)

-- Helper function to apply operations
--applyOperations :: [Int -> Int] -> [Int] -> [Int]
--applyOperations ops numbers = zipWith ($) (cycle ops) numbers

-- Helper function to write output to a file
--writeOutputFile :: FilePath -> [Int] -> IO ()
--writeOutputFile path results = writeFile path (unlines (map show results))