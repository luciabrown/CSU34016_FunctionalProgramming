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
-- Number
eval _ (Number n) = n

-- Ident
eval dict (Ident s) = 
  case find s dict of
    Just v->v
    Nothing -> error("Variable "++s++" not found.")

-- Dvdby
eval dict (DvdBy n1 n2) = 
  let numerator = eval dict n1
      denominator = eval dict n2
  in if denominator == 0 
    then error "Divide by zero error."
    else numerator/denominator

-- Prod
eval dict (Prod n1 n2) = eval dict n1 * eval dict n2

-- Negate
eval dict (Negate n) = - eval dict n

-- Not
eval dict (Not n) =
  if eval dict n == 0
    then 1.0
    else 0.0

-- Same or Less
eval dict (SameOrLess n1 n2) =
  if eval dict n1 > eval dict n2
    then 0.0
    else 1.0

-- Is Null
eval dict (IsNull n) =
  if eval dict n == 0
    then 1.0
    else 0.0

-- Q2 (7 marks)
-- Implement the following function (which always returns a value)
-- Grading of this will put emphasis on cases that would cause a
-- runtime error for Q1.
meval :: Dict -> MathExpr -> Maybe Float
-- Number
meval _ (Number n) = Just n

-- Ident
meval dict (Ident n) =
  case find n dict of
    Just v-> Just v
    Nothing->Nothing -- if not found, return nothing

-- DvdBy
meval dict (DvdBy n1 n2)=
  case(meval dict n1, meval dict n2) of
    (Just n1, Just n2) ->
      if n2 == 0.0 then Nothing
      else Just(n1/n2)
    _ -> Nothing
 
-- Prod
meval dict (Prod n1 n2)=
  case (meval dict n1, meval dict n2) of
    (Just v1, Just v2) -> Just (v1 * v2)
    _ -> Nothing  -- If either subexpression is Nothing, return Nothing

-- Negate
meval dict (Negate n)=
    case meval dict n of
    Just v -> Just (-v)
    Nothing -> Nothing  -- If subexpression is Nothing, return Nothing

-- Not
meval dict (Not n)=
  case meval dict n of
    Just v -> Just (if v == 0.0 then 1.0 else 0.0)
    Nothing -> Nothing

-- Same or Less
meval dict (SameOrLess n1 n2)=
  case (meval dict n1, meval dict n2) of
    (Just v1, Just v2) -> Just (if v1 <= v2 then 1.0 else 0.0)
    _ -> Nothing

-- Is Null
meval dict (IsNull n)=
  case meval dict n of
    Just v -> Just (if v == 0.0 then 1.0 else 0.0)
    Nothing -> Nothing


-- Q3 (6 marks)
-- Laws of Arithmetic for this question:
--    0 = x * 0
--    x = 1 * x
-- The following function should implement simplifications
-- using ONLY the above two laws, wherever they apply.
simp :: MathExpr -> MathExpr
-- Number
simp (Number n) = Number n  -- Numbers remain unchanged

-- Ident
simp (Ident n) = Ident n  -- Identifiers remain unchanged

-- DvdBy
simp (DvdBy n1 n2) = DvdBy (simp n1) (simp n2)

-- Prod
simp (Prod n1 n2) = 
  case (simp n1, simp n2) of
    (Number 0.0, _) -> Number 0.0  -- x * 0 = 0
    (_, Number 0.0) -> Number 0.0  -- 0 * x = 0
    (Number 1.0, e) -> e             -- 1 * x = x
    (e, Number 1.0) -> e             -- x * 1 = x
    (se1, se2) -> Prod se1 se2  -- Otherwise, simplify subexpressions

-- Negate
simp (Negate n) = Negate (simp n)

-- Not
simp (Not n) = Not (simp n) 

-- Same or Less
simp (SameOrLess n1 n2) = 
  SameOrLess (simp n1) (simp n2)  -- No changes in comparisons

-- Is Null
simp (IsNull n) = IsNull (simp n)  -- No changes in null checks

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...
testEvalA = eval [] (Number 5.0) == 5.0
testEvalB = eval [] (Prod (Number 3.0) (Number 4.0)) == 12.0
testEvalC = eval [] (DvdBy (Number 10.0) (Number 5.0)) == 2.0
testEvalD = eval [] (Negate (Number 7.0)) == -7.0
testEvalE = eval [("x", 10.0)] (Ident "x") == 10.0
testEvalF = eval [] (Not (Number 0.0)) == 1.0
testEvalG = eval [] (Not (Number 5.0)) == 0.0
testEvalH = eval [] (SameOrLess (Number 3.0) (Number 5.0)) == 1.0
testEvalI = eval [] (SameOrLess (Number 6.0) (Number 5.0)) == 0.0
testEvalJ = eval [] (IsNull (Number 0.0)) == 1.0
testEvalK = eval [] (IsNull (Number 5.0)) == 0.0

errorEvalA = eval [] (Ident "x") -- error - variable not found
errorEvalB = eval [] (DvdBy (Number 10.0) (Number 0.0)) -- error - division by zero

testMevalA = meval [] (Number 5.0) == (Just 5.0)
testMevalB = meval [] (Prod (Number 3.0) (Number 4.0)) == (Just 12.0)
testMevalC = meval [] (DvdBy (Number 10.0) (Number 5.0)) == (Just 2.0)
testMevalD = meval [] (Negate (Number 7.0)) == Just (-7.0)
testMevalE = meval [("x", 10.0)] (Ident "x") == (Just 10.0)
testMevalF = meval [] (Not (Number 0.0)) == (Just 1.0)
testMevalG = meval [] (Not (Number 5.0)) == (Just 0.0)
testMevalH = meval [] (SameOrLess (Number 3.0) (Number 5.0)) == (Just 1.0)
testMevalI = meval [] (SameOrLess (Number 6.0) (Number 5.0)) == (Just 0.0)
testMevalJ = meval [] (IsNull (Number 0.0)) == (Just 1.0)
testMevalK = meval [] (IsNull (Number 5.0)) == (Just 0.0)

errorMevalA= meval [] (Ident "x") == Nothing -- error - variable not found
errorMevalB= meval [] (DvdBy (Number 10.0) (Number 0.0)) == Nothing-- error - division by zero
errorMevalC= meval [] (SameOrLess (Ident "x") (Number 5.0)) == Nothing
errorMevalD= meval [] (Prod (Ident "x") (Number 2.0)) == Nothing
errorMevalE=meval [] (Not (Ident "x")) == Nothing
errorMevalF= meval [] (DvdBy (Ident "x") (Number 5.0)) == Nothing
errorMevalG= meval [] (Not (Ident "x")) == Nothing

testSimpA = simp (Prod (Number 0.0) (Ident "x")) == Number 0.0
testSimpB = simp (Prod (Ident "x") (Number 0.0)) == Number 0.0
testSimpC = simp (Prod (Number 1.0) (Ident "x")) == Ident "x"
testSimpD = simp (Prod (Ident "x") (Number 1.0)) == Ident "x"
testSimpE = simp (Prod (Number 3.0) (Number 0.0)) == Number 0.0

-- Non-zero Test Cases for simplification
testNonZeroSimpA = 
    let expr = Prod (Number 2.0) (Prod (Ident "x") (Number 1.0))
        expected = Prod (Number 2.0) (Ident "x")  -- Expected result after simplification
    in simp expr == expected

testNonZeroSimpB = 
    let expr = Prod (Prod (Ident "y") (Number 1.0)) (Ident "z")
        expected = Prod (Ident "y") (Ident "z")  -- Expected result after simplification
    in simp expr == expected