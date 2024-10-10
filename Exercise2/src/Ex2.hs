module Ex2 where

add :: Int -> Int -> Int
add x y = (x+y) `mod` 65563

mul :: Int -> Int -> Int
mul x y
  | p == 0    = 1
  | otherwise = p
  where p = (x*y) `mod` 65563

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Hint for Q1,2,3: taking every 3rd element of [1..13] returns [3,6,9,12]

-- *** Q1 (3 marks)
-- returns a list of every 113th element of its input
f1 :: [a] -> [a]
f1 xs = [x | (x, i) <- zip xs [1..], i `mod` 113 == 0]  --Pair with index, extract every 113 elements

-- *** Q2 (3 marks)
-- sums every 277th element of its input
-- if list is too short it returns 0
-- you can use `add` or `(+)` here - won't effect grading
f2 :: [Int] -> Int
f2 ns = foldl add 0 [x | (x, i) <- zip ns [1..], i `mod` 277 == 0]

-- *** Q3 (4 marks)
-- multiplies every 316th element of its input
-- if list is too short it returns 1
-- you can use `mul` or `(*)` here - won't effect grading
f3 :: [Int] -> Int
f3 ns = foldl mul 1 [x | (x, i) <- zip ns [1..], i `mod` 316 == 0]

-- *** Q4 (8 marks)
-- Operation Table (See Exercise2 description on BB)
--    ___________________________________________
--    | opcode | operation | operands | Nothing |
--    -------------------------------------------
--    |   60   |    add    | fixed 3  | term    |
--    |   28   |    add    | fixed 3  | skip    |
--    |   49   |    add    | fixed 5  | 4       |
--    |   53   |    add    | stop@ 3  | term    |
--    |   66   |    add    | stop@ 4  | skip    |
--    |   18   |    add    | stop@ 6  | 8       |
--    |   73   |    mul    | fixed 4  | term    |
--    |   44   |    mul    | fixed 3  | skip    |
--    |   50   |    mul    | fixed 3  | 6       |
--    |   47   |    mul    | stop@ 4  | term    |
--    |   57   |    mul    | stop@ 4  | skip    |
--    |   76   |    mul    | stop@ 5  | 7       |
--    -------------------------------------------
-- initially, skip any number that is not an opcode
-- if called with [], return `(0,[])`
-- if no numbers found after an `add` opcode, return (0,[])
-- if no numbers found after an `mul` opcode, return (1,[])
-- if list ends midway through opcode processing, return result so far
-- if a Nothing is skipped for a fixed N opcode,
--    that Nothing does not contribute to the count.
-- Hint:
--   When building a list for test purposes,
--   remember a value of type `Maybe a` needs to be built
--   using one of the two data constructors of the `Maybe` type.
-- Operation Table
-- Initially, skip any number that is not an opcode
-- If called with [], return `(0,[])`
-- If no numbers found after an `add` opcode, return (0,[])
-- If no numbers found after an `mul` opcode, return (1,[])
-- If list ends midway through opcode processing, return result so far
-- If a Nothing is skipped for a fixed N opcode,
-- that Nothing does not contribute to the count.
-- Hint: A value of type `Maybe a` needs to be built using one of the two data constructors of the `Maybe` type.

f4 :: [Maybe Int] -> (Int, [Maybe Int])
f4 mis = functionFour mis 0  -- Initialize 'ans' to 0

functionFour :: [Maybe Int] -> Int -> (Int, [Maybe Int])
functionFour [] ans = (ans, [])
functionFour (Just opcode : operands) ans =
  case opcode of
    60 -> fixed operands 3 ans True add                    -- add fixed 3, terminate on Nothing
    28 -> fixed operands 3 ans True add                    -- add fixed 3, skip on Nothing
    49 -> fixed operands 5 ans True(addHandler 4)         -- add fixed 5, treat 4 as value
    53 -> stopping operands 3 ans True add                     -- add  3, terminate on Nothing
    66 -> stopping operands 4 ans True add                    -- add  4, skip on Nothing
    18 -> stopping operands 6 ans True (addHandler 8)      -- add  6, treat 8 as value
    73 -> fixed operands 4 ans False mul                        -- mul fixed 4, terminate on Nothing
    44 -> fixed operands 3 ans False mul                        -- mul fixed 3, skip on Nothing
    50 -> fixed operands 3 ans False (mulHandler 6)         -- mul fixed 3, treat 6 as value
    47 -> stopping operands 4 ans False mul                     -- mul  4, terminate on Nothing
    57 -> stopping operands 4 ans False mul                     -- mul  4, skip on Nothing
    76 -> stopping operands 5 ans False (mulHandler 7)      -- mul  5, treat 7 as value
    _  -> functionFour operands ans                   -- Skip any unrecognized opcode
functionFour (Nothing : operands) ans = functionFour operands ans

fixed :: [Maybe Int] -> Int -> Int -> Bool -> (Int -> Int -> Int) -> (Int, [Maybe Int])
fixed fixedOperands count ans isAdd operation
    | length fixedOperands >= count =
      let (current, remaining) = splitAt count fixedOperands

          accumulator = if isAdd then 0 else 1
          -- Adjust fold logic based on the operation (add or mul)
          processed = foldl (\acc x -> 
                              case x of 
                                Nothing -> acc
                                Just v  -> if isAdd then add acc v else mul acc v
                            ) accumulator current
          newResult = operation ans processed
      in functionFour remaining newResult
    | otherwise = (ans, fixedOperands)

stopping :: [Maybe Int] -> Int -> Int ->  Bool ->(Int -> Int -> Int) -> (Int, [Maybe Int])
stopping stoppingOperands stoppingNumber ans isAdd operation
    | length stoppingOperands >= stoppingNumber =
      let (current, remaining) = splitAt stoppingNumber stoppingOperands

          accumulator = if isAdd then 0 else 1
          -- Adjust fold logic based on the operation (add or mul)
          processed = foldl (\acc x -> 
                              case x of 
                                Nothing -> acc
                                Just v  -> if isAdd then add acc v else mul acc v
                            ) accumulator current
          newResult = operation ans processed
      in (newResult, remaining)
    | otherwise = (ans, stoppingOperands)    

addHandler :: Int -> Int -> Int -> Int
addHandler result operand replacement
    | operand == 4 = replacement
    | otherwise = result + operand

mulHandler :: Int -> Int -> Int -> Int
mulHandler result operand replacement
    | operand == 6 = replacement
    | otherwise = result * operand




-- *** Q5 (2 marks)
-- uses `f4` to process all the opcodes in the maybe list,
-- by repeatedly applying it to the leftover part
-- Note: this will be tested against a correct version of `f4`,
--       rather than your submission.
f5 :: [Maybe Int] -> [Int]
f5 mis = undefined

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

-- *** Test cases for f1 function
testF1_1 = f1 [1..120] == [113]                   -- Only one element at the 113th position
testF1_2 = f1 [1..500] == [113, 226, 339, 452]    -- Multiple elements at 113, 226, 339, 452
testF1_3 = f1 [1..112] == []                      -- No element because the list has fewer than 113 elements

-- *** Test cases for f2 function
testF2_1 = f2 [1..500] == 277                     -- The sum of only the 277th element
testF2_2 = f2 [1..600] == add 277 554             -- Sum of 277th and 554th element
testF2_3 = f2 [1..276] == 0                       -- The list is too short to include a 277th element

-- *** Test cases for f3 function
testF3_1 = f3 [1..200] == 1                       -- No element at the 316th position, so return 1
testF3_2 = f3 [1..500] == 316                     -- Product of only the 316th element
testF3_3 = f3 [1..900] == mul 316 632             -- Product of 316th and 632nd element

-- *** Test cases for f4 function
testF4_1 = f4 [Just 60, Just 3,  Just 4, Just 5] == (12, [])

testF4_2 = f4 [Just 28, Just 4, Just 5, Nothing, Just 6] == (12, [Just 4, Just 5, Just 6])
testF4_3 = f4 [Just 49, Just 4, Just 5, Just 6, Just 7, Just 8] == (18, [Just 4, Just 5, Just 6, Just 7, Just 8])
testF4_4 = f4 [Just 53, Just 4, Just 5, Nothing, Just 6] == (12, [Just 4, Just 5, Nothing])
testF4_5 = f4 [Just 66, Just 1, Just 2, Nothing, Just 3] == (9, [Just 1, Just 2, Nothing, Just 3])
testF4_6 = f4 [Just 18, Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8] == (20, [Just 8])
testF4_7 = f4 [Just 73, Just 1, Just 2, Just 3, Just 4, Nothing, Just 5] == (0, [Just 1, Just 2, Just 3, Just 4])
testF4_8 = f4 [Just 44, Just 6, Just 7, Nothing, Just 3] == (42, [Just 6, Just 7, Nothing, Just 3])
testF4_9 = f4 [Just 50, Just 6, Just 3, Just 4, Just 5] == (36, [Just 3, Just 4, Just 5])
testF4_10 = f4 [Just 47, Just 1, Just 2, Nothing, Just 4, Just 5] == (0, [Just 1, Just 2, Nothing])
testF4_11 = f4 [Just 57, Just 2, Nothing, Just 4, Just 5] == (6, [Just 2, Nothing])
testF4_12 = f4 [Just 76, Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7] == (42, [Just 6, Just 7])
testF4_13 = f4 [] == (0, [])                            -- Edge case: Empty input list
testF4_14 = f4 [Nothing, Nothing, Nothing] == (0, [])   -- Edge case: All Nothing values
testF4_15 = f4 [Just 73] == (1, [])                     -- Edge case: Opcode with no operands