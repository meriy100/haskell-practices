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

main :: IO ()
main = hspec $ do
  describe "13-2" $ do
    context "numEqual" $ do
      it "1 [1..10]" $ do
        (numEqual 1 [1..10]) `shouldBe` (1::Int)
      it "1 [2,1,4,1,3,4,2]" $ do
        (numEqual 1 [2,1,4,1,3,4,2]) `shouldBe` (2::Int)
