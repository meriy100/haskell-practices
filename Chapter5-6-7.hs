import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

exists :: Eq a => [a] -> a -> Bool
exists xs x
  | head xs == x = True
  | tail xs == [] = False
  | otherwise = exists (tail xs) x
find :: Eq t => [t] -> (t -> Bool) -> Maybe t
find xs f
  | f(head xs) = Just(head xs)
  | tail xs == [] = Nothing
  | otherwise = find (tail xs) f

-- 5-18

doubleAll :: [Integer] -> [Integer]
doubleAll [] = []
doubleAll xs = (head xs * 2):(doubleAll (tail xs))

-- 5-20
divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors x = filter (\i -> x `mod` i == 0) [1..x]

isPrime :: Integer -> Bool
isPrime x =  divisors x == [1, x]

-- 7-2
func72 (x1:x2:_) = x1 + x2
func72 (_) = 0

-- 7-3
func73 xs = sum (take 2 xs)


-- 7-5
procduct (x:xs) = x * procduct xs
procduct _ = 1

-- 7-6
mAnd (x:xs) = x && mAnd xs
mAnd _ = True
mOr (x:xs) = x || mOr xs
mOr _ = False

-- 7-8

elemNum :: Integer -> [Integer]  -> Integer
elemNum x (x1:xs)
  | x == x1 = 1 + elemNum x xs
  | otherwise = elemNum x xs
elemNum x _ = 0

-- 7-9
unique :: [Integer] -> [Integer]
unique (x1:xs)
  | [x | x <- xs, x == x1] =

main :: IO ()
main = hspec $ do
  describe "5-18" $ do
    context "doubleAll" $ do
      it "[1..10]" $ do
        (doubleAll [1..10]) `shouldBe` ([2,4..20])
  describe "5-20" $ do
    context "divisors" $ do
      it "0" $ do
        (divisors 0) `shouldBe` ([])
      it "1" $ do
        (divisors 1) `shouldBe` ([1])
      it "2" $ do
        (divisors 2) `shouldBe` ([1, 2])
      it "12" $ do
        (divisors 12) `shouldBe` ([1, 2, 3, 4, 6, 12])
    context "isPrime" $ do
      it "1" $ do
        (isPrime 1) `shouldBe` False
      it "2" $ do
        (isPrime 2) `shouldBe` True
      it "10" $ do
        (isPrime 10) `shouldBe` False
      it "23" $ do
        (isPrime 23) `shouldBe` True
  describe "7-5" $ do
    context "product" $ do
      it "1..10" $ do
        (procduct [1..10]) `shouldBe` 3628800
      it "2, 3, 5" $ do
        (procduct [2, 3, 5]) `shouldBe` 30
      it "empty" $ do
        (procduct []) `shouldBe` 1
  describe "7-8" $ do
    context "elemNum" $ do
      it "should count x in xs" $ do
        (elemNum 2 [1..5]) `shouldBe` 1
      it "should count x in xs" $ do
        (elemNum 2 (concat [[1..5], [0,2..10]])) `shouldBe` 2
  describe "7-9" $ do
    context "unique" $ do
      it "" $ do
        (unique [1,2,3,4,5,4,3,2]) `shouldBe` [1,5]
      it "" $ do
        (unique [2,3,4,4,5,4,3,2]) `shouldBe` [5]
