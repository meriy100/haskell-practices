{-# LANGUAGE GADTs, TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Test.QuickCheck

-- 13-2
numEqual n (x:xs)
  | n == x = 1 + numEqual n xs
  | otherwise = numEqual n xs

numEqual n [] = 0
-- :t numEqual
-- #=> numEqual :: (Eq t1, Num t) => t1 -> [t1] -> t

-- 13-8
class Eq a => Ord' a where
  (<), (>), (<=), (>=) :: a -> a -> Bool


-- 13-9
-- instance (Ord a, Ord b) => Ord (a, b) where
--   (x, y) < (m, n)  =  x > m && y > n
--   (x, y) > (m, n)  =  x < m && y < n
--   (x, y) >= (m, n)  =  x >= m && y >= n
--   (x, y) <= (m, n)  =  x <= m && y <= n
--


-- 13-18
-- f :: Int -> b
-- g :: a -> Bool
-- # a is Int, b is Bool

-- h :: (Int, a, a)
-- l :: (a, a, [Bool])
-- cant unify because
--   if a == [Bool]
--     h :: (Int, [Bool], [Bool])
--     g :: ([Bool], [Bool], [Bool])
--     but is not  same
--   if a == Int
--     h :: (Int, Int, Int)
--     g :: (Int, Int, [Bool])
--     but is not  same

-- 13-20
-- type True = 1
-- type Foo = True 2
f :: (a, [a]) -> Bool
f (x, y:ys) = True

main :: IO ()
main = hspec $ do
  describe "13-2" $ do
    context "numEqual" $ do
      it "1 [1..10]" $ do
        (numEqual 1 [1..10]) `shouldBe` (1::Int)
      it "1 [2,1,4,1,3,4,2]" $ do
        (numEqual 1 [2,1,4,1,3,4,2]) `shouldBe` (2::Int)
