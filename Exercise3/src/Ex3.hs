module Ex3 where

--required for all Qs:
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
find :: String -> Dict -> Maybe Float
find s [] = Nothing
find s ((t,f):d)
  | s==t       =  Just f
  | otherwise  =  find s d

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (7 marks)
-- Implement the following function (which may have runtime errors)
-- This will only be graded using dictionaries and values that do
-- not result in runtime errors in a correct implementation.
eval :: Dict -> MathExpr -> Float
eval _ _ = error "Ex3Q1: eval not yet defined"

-- Q2 (7 marks)
-- Implement the following function (which always returns a value)
-- Grading of this will put emphasis on cases that would cause a
-- runtime error for Q1.
meval :: Dict -> MathExpr -> Maybe Float
meval _ _ = error "Ex3Q2: meval not yet defined"

-- Q3 (6 marks)
-- Laws of Arithmetic for this question:
--    0 = x * 0
--    x = 1 * x
-- The following function should implement simplifications
-- using ONLY the above two laws, wherever they apply.
simp :: MathExpr -> MathExpr
simp _ = error "Ex3Q3: simp not yet defined"

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

