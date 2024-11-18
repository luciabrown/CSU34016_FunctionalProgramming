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
mdeval _ _ = error "Ex4Q1: mdeval not yet defined"

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
lenTuple = (undefined,undefined)

-- dofold sumupTuple = sumup
sumupTuple :: (Int,Int -> Int -> Int)
sumupTuple = (undefined,undefined)

-- dofold prodTuple = prod
prodTuple :: (Int,Int -> Int -> Int)
prodTuple = (undefined,undefined)

-- dofold catTuple = cat
catTuple :: ([Thing],[Thing] -> [Thing] -> [Thing])
catTuple = (undefined,undefined)

-- Q3 (11 marks)
sub = subtract -- shorter!
ops = [(+34),(*29),(33-),(sub 28),(*34),(+34),(+33),(30-),(*29),(*32),(*26)]

-- (!) This question requires modifying Main.hs
-- See, and/or compile and run Main.hs for further details

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

