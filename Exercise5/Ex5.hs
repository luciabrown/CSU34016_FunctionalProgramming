module Ex5 where
-- no code for Q1


-- for Q2:
frec x  =  if x <= 4 then 4 else x * frec (x `div` 5)



-- for Q3:
bonus []      =  12
bonus (x:xs)  =  x + 5 + bonus xs




--for Q4:
casef x
  | x < 4   =  2*x
  | x >= 4  = 2*x-1


