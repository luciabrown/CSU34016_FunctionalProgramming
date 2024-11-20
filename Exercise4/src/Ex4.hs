module Ex4 where

--required for Q1
data MathExpr -- the expression datatype
  = Number Float -- floating-point value
  | Ident String -- variable/identifier name
  | DvdBy MathExpr MathExpr -- divide first by second
  | Prod MathExpr MathExpr -- multiplies both
  | Negate MathExpr -- numerical negation (-x)
  -- the following are boolean expressions (using numbers)
  -- the number 0.0 represents False, all others represent True.
  | Not MathExpr -- logical not
  | SameOrLess MathExpr MathExpr -- True if first is less than or equal to second
  | IsNull MathExpr -- True if numeric value is zero
  deriving (Eq,Ord,Show)

type Dict = [(String,Float)]
insert :: String -> Float -> Dict -> Dict
insert s f d = (s,f):d
find :: MonadFail m => String -> Dict -> m Float
find s [] = fail (s++" not found")
find s ((t,f):d)
  | s==t       =  return f
  | otherwise  =  find s d

-- required for Q2
x `incfst` _  =  x + 1
_ `incsnd` y  =  1 + y
type Thing = (Float,[Int])

-- required for all Qs:

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
-- implement the following function (which should always return a value):
mdeval :: MonadFail m => Dict -> MathExpr -> m Float

mdeval _ (Number n) = return n  -- If it's a number, return it

mdeval dict (Ident s) =
  case lookup s dict of
    Just v  -> return v  
    Nothing -> fail ("Identifier not found: " ++ s)  -- If not found, fail

mdeval dict (DvdBy e1 e2) = do
  v1 <- mdeval dict e1
  v2 <- mdeval dict e2
  if v2 == 0 then fail "Division by zero" else return (v1 / v2)

mdeval dict (Prod e1 e2) = do
  v1 <- mdeval dict e1
  v2 <- mdeval dict e2
  return (v1 * v2)

mdeval dict (Negate e) = do
  v <- mdeval dict e
  return (-v)

mdeval dict (Not e) = do
  v <- mdeval dict e
  return (if v == 0 then 1 else 0)

mdeval dict (SameOrLess e1 e2) = do
  v1 <- mdeval dict e1
  v2 <- mdeval dict e2
  return (if v1 <= v2 then 1 else 0)

mdeval dict (IsNull e) = do
  v <- mdeval dict e
  return (if v == 0 then 1 else 0)

-- Q2 (6 marks) 
-- Consider the following four recursive pattern definitions:
len :: Int -> [Int] -> Int
len z []     = z
len z (x:xs) = len (z `incfst` x) xs
sumup :: Int -> [Int] -> Int
sumup sbase []     = sbase
sumup sbase (n:ns) = sumup (sbase + n) ns
prod :: Int -> [Int] -> Int
prod mbase []     = mbase
prod mbase (n:ns) = prod (mbase * n) ns
cat :: [Thing] -> [[Thing]] -> [Thing]
cat pfx []     = pfx
cat pfx (xs:xss) = cat (pfx ++ xs) xss

-- They all have the same abstract pattern,
-- as captured by the following Higher Order Function (HOF):
foldL z _ [] = z
foldL z op (x:xs) = foldL (z `op` x) op xs

-- We can gather the `z` and `opr` arguments into a tuple: (z,op)
-- which allows us to construct a call to foldL as:
dofold (z,op) = foldL z op

-- Your task is to complete the tuples below,
-- so that `dofold` can be used to implement the fns. above.

-- dofold lenTuple = len
lenTuple :: (Int,Int -> Int -> Int)
lenTuple = (0, \x y -> y + 1) 

-- dofold sumupTuple = sumup
sumupTuple :: (Int,Int -> Int -> Int)
sumupTuple = (0, (+)) 

-- dofold prodTuple = prod
prodTuple :: (Int,Int -> Int -> Int)
prodTuple = (1, (*)) 

-- dofold catTuple = cat
catTuple :: ([Thing],[Thing] -> [Thing] -> [Thing])
catTuple = ([], (++)) 

-- Q3 (11 marks)
sub = subtract -- shorter!
ops = [(+34),(*29),(33-),(sub 28),(*34),(+34),(+33),(30-),(*29),(*32),(*26)]

-- (!) This question requires modifying Main.hs
-- See, and/or compile and run Main.hs for further details

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...
-- Helper function to print results for each test
-- Define the function printTestResult to compare and print the results
--test1 = "test1" ~: mdeval [] (Number 5) ~?= Right 5.0
--test2 = "test2" ~: mdeval [("x", 10), ("y", 20)] (Ident "x") ~?= Right 10.0
--test3 = "test3" ~: mdeval [("x", 10), ("y", 20)] (Ident "z") ~?= Left "Identifier not found: z"
--test4 = "test4" ~: mdeval [("x", 10), ("y", 5)] (DvdBy (Ident "x") (Ident "y")) ~?= Right 2.0
--test5 = "test5" ~: mdeval [("x", 10), ("y", 0)] (DvdBy (Ident "x") (Ident "y")) ~?= Left "Division by zero"
--test6 = "test6" ~: mdeval [("x", 3), ("y", 4)] (Prod (Ident "x") (Ident "y")) ~?= Right 12.0
--test7 = "test7" ~: mdeval [("x", 5)] (Negate (Ident "x")) ~?= Right (-5.0)
--test8 = "test8" ~: mdeval [("x", 0)] (Not (Ident "x")) ~?= Right 1.0
--test9 = "test9" ~: mdeval [("x", 10)] (Not (Ident "x")) ~?= Right 0.0
--test10 = "test10" ~: mdeval [("x", 5), ("y", 10)] (SameOrLess (Ident "x") (Ident "y")) ~?= Right 1.0
--test11 = "test11" ~: mdeval [("x", 15), ("y", 10)] (SameOrLess (Ident "x") (Ident "y")) ~?= Right 0.0
--test12 = "test12" ~: mdeval [("x", 0)] (IsNull (Ident "x")) ~?= Right 1.0
--test13 = "test13" ~: mdeval [("x", 10)] (IsNull (Ident "x")) ~?= Right 0.0
--test14 = "test14" ~: mdeval [("x", 20), ("y", 5)] (DvdBy (Prod (Ident "x") (Ident "y")) (Ident "y")) ~?= Right 4.0
--test15 = "test15" ~: mdeval [("x", 5)] (Negate (Prod (Ident "x") (Ident "x"))) ~?= Right (-25.0)
--test16 = "test16" ~: mdeval [] (Ident "undefined_var") ~?= Left "Identifier not found: undefined_var"
--test17 = "test17" ~: mdeval [("a", 1), ("b", 0)] (DvdBy (Ident "a") (Ident "b")) ~?= Left "Division by zero"

-- Run all tests
--tests = TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16, test17]

--main :: IO Counts
--main = runTestTT tests