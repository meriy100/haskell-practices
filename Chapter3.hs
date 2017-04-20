module Chapter3 where

-- 3-4
myAnd :: Bool -> Bool -> Bool
myAnd x y
  | x == y = y
  | x      = myAnd y y
  | otherwise = False


