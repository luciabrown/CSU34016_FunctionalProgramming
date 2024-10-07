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
f4 :: [Maybe Int] -> (Int,[Maybe Int])
f4 mis = undefined

-- *** Q5 (2 marks)
-- uses `f4` to process all the opcodes in the maybe list,
-- by repeatedly applying it to the leftover part
-- Note: this will be tested against a correct version of `f4`,
--       rather than your submission.
f5 :: [Maybe Int] -> [Int]
f5 mis = undefined

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

