import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

-- module Chapter3 where

-- 3-4
myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd x y = False

myOr :: Bool -> Bool -> Bool
myOr False False = False
myOr x y = True

-- 3-5
nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd x y = True

-- 3-9
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z
  | x == y = False
  | x == z = False
  | otherwise = y /= z

main :: IO ()
main = hspec $ do
  describe "3-4" $ do
    context "myAnd" $ do
      it "return Bool" $ do
        (myAnd True True) `shouldBe` (True :: Bool)
        (myAnd True False) `shouldBe` (False :: Bool)
        (myAnd False True) `shouldBe` (False :: Bool)
        (myAnd False False) `shouldBe` (False :: Bool)
    context "myOr" $ do
      it "return Bool" $ do
        (myOr True True) `shouldBe` (True :: Bool)
        (myOr True False) `shouldBe` (True :: Bool)
        (myOr False True) `shouldBe` (True :: Bool)
        (myOr False False) `shouldBe` (False :: Bool)
  describe "3-5" $ do
    context "nAnd" $ do
      it "return Bool" $ do
        (nAnd True True) `shouldBe` (False :: Bool)
        (nAnd True False) `shouldBe` (True :: Bool)
        (nAnd False True) `shouldBe` (True :: Bool)
        (nAnd False False) `shouldBe` (True :: Bool)

  describe "3-9" $ do
    context "threeDifferent" $ do
      it "return Bool" $ do
        (threeDifferent 1 2 3) `shouldBe` (True :: Bool)
        (threeDifferent 1 1 3) `shouldBe` (False :: Bool)
        (threeDifferent 1 2 1) `shouldBe` (False :: Bool)
        (threeDifferent 1 2 2) `shouldBe` (False :: Bool)
