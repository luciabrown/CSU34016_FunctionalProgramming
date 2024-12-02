module Main where

import Ex4

main :: IO ()
main = do
  -- Read the contents of the input file
  input <- readFile "input.dat"
  
  -- Parse the input as a list of Ints
  let numbers = map read (lines input) :: [Int]
  
  -- Convert the Ints to Integers for compatibility with ops
  let numbersAsInteger = map fromIntegral numbers
  
  -- Apply the operations cyclically
  let results = zipWith (\num op -> op num) numbersAsInteger (cycle ops)
  
  -- Write the results to the output file
  writeFile "output.dat" (unlines $ map show results)

  -- Print a summary message
  putStrLn $ unlines
    [ "Running Exercise4."
    , "Processed input from `input.dat` and wrote results to `output.dat`."
    , "Summary:"
    , "Number of operations in `ops`: " ++ nops
    , "Input size: " ++ show (length numbers)
    , "Output size: " ++ show (length results)
    ]

  where
    len    = length ops 
    nops   = show len
    nops'  = show (len + 1)
    nops'' = show (len + 2)