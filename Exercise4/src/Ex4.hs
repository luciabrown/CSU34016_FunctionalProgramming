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

-- Tests for Q1:
testMdevalA = mdeval [] (Number 5.0) == Just 5.0
testMdevalB = mdeval [] (Prod (Number 3.0) (Number 4.0)) == Just 12.0
testMdevalC = mdeval [] (DvdBy (Number 10.0) (Number 5.0)) == Just 2.0
testMdevalD = mdeval [] (Negate (Number 7.0)) == Just (-7.0)
testMdevalE = mdeval [("x", 10.0)] (Ident "x") == Just 10.0
testMdevalF = mdeval [] (Not (Number 0.0)) == Just 1.0
testMdevalG = mdeval [] (Not (Number 5.0)) == Just 0.0
testMdevalH = mdeval [] (SameOrLess (Number 3.0) (Number 5.0)) == Just 1.0
testMdevalI = mdeval [] (SameOrLess (Number 6.0) (Number 5.0)) == Just 0.0
testMdevalJ = mdeval [] (IsNull (Number 0.0)) == Just 1.0
testMdevalK = mdeval [] (IsNull (Number 5.0)) == Just 0.0
testMdevalL = mdeval [] (DvdBy (Number 10.0) (Number 0.0)) == Nothing -- Division by zero
testMdevalM = mdeval [] (Ident "y") == Nothing -- Undefined variable

-- Test cases for Q2:
testLenA = dofold lenTuple [1, 2, 3, 4] == 4 -- Length of the list [1,2,3,4] is 4
testLenB = dofold lenTuple [] == 0 -- Empty list has length 0

testSumupA = dofold sumupTuple [1, 2, 3, 4] == 10 -- Sum of [1,2,3,4] is 10
testSumupB = dofold sumupTuple [] == 0 -- Sum of an empty list is 0

testProdA = dofold prodTuple [1, 2, 3, 4] == 24 -- Product of [1,2,3,4] is 24
testProdB = dofold prodTuple [] == 1 -- Product of an empty list is 1 (identity)

-- Sample Things for testing `cat`
testThings1 = [(1.0, [1]), (2.0, [2])]
testThings2 = [[(3.0, [3])], [(4.0, [4])]]

testCatA = dofold catTuple (testThings1 : testThings2) ==
           [(1.0, [1]), (2.0, [2]), (3.0, [3]), (4.0, [4])] -- Concatenation works as expected

testCatB = dofold catTuple [] == [] -- Concatenation of empty list is empty

