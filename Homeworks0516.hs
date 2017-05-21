module Homeworks0516 where

import Test.Hspec

-- memo
putStrLn = putStr . (++ "\n")
inc = (+1)


square x = x * x

-- 10-2
length' :: Num a => [a1] -> a
length' xs = sum $ map (\_ -> 1) xs

-- 10-6
squares :: [Integer] -> [Integer]
squares = map (\n -> n * n)
sumSquares :: [Integer] -> Integer
sumSquares = sum . squares
isPositives :: [Integer] -> Bool
isPositives (n:ns)
  | n > 0 = True && isPositives ns
  | otherwise = False
isPositives [] = True

-- 10-13
sumOfSquares :: [Integer] -> Integer
sumOfSquares (x:xs) = foldr (\n -> \m -> n * n + m) (x * x) xs

-- 10-14

positiveOrZero x
  | x > 0 = x
  | otherwise = 0
sumOfSquaresOnlyPositive :: [Integer] -> Integer
sumOfSquaresOnlyPositive ns = foldr (+) 0 $ map (square . positiveOrZero) ns

-- 11-4
-- ($) :: (a -> b) -> a -> b

-- 11-5
-- zipWith ($) [sum, product] [[1, 2], [3, 4]]

main :: IO ()
main = hspec $ do
  describe "10-2" $ context "length'" $ do
    it "abcd" $ length' "abcd" `shouldBe` (4::Integer)
    it "[1..10]" $ length' [1..10] `shouldBe` (10::Integer)
  describe "10-6" $ do
    context "squares" $ do
      it "[1..5]" $ squares [1..5] `shouldBe` ([1, 4, 9, 16, 25]::[Integer])
      it "[5..9]" $ squares [5..9] `shouldBe` ([25, 36, 49, 64, 81]::[Integer])
    context "sumSquares" $ do
      it "[1..5]" $ sumSquares [1..5] `shouldBe` (55::Integer)
      it "[5..9]" $ sumSquares [5..9] `shouldBe` (255::Integer)
    context "isPositives" $ do
      it "[1..5]" $ isPositives [1..5] `shouldBe` (True::Bool)
      it "[-1, 2..5]" $ isPositives (-1:[2..5]) `shouldBe` (False::Bool)
  describe "10-13" $ context "sumOfSquares" $ do
    it "[1..5]" $ sumOfSquares [1..5] `shouldBe` sumSquares [1..5]
    it "[n + 1 | [1..10]]" $ do
      let ns = [n * 2 | n <- [1..10]]
      sumOfSquares ns `shouldBe` sumSquares ns
    it "[1..5]" $ sumOfSquares [5..9] `shouldBe` sumSquares [(-9)..(-5)]
    it "[1..5]" $ sumOfSquares [(-9)..(-5)] `shouldBe` sumSquares [5..9]
  describe "10-14" $ context "sumOfSquaresOnlyPositive" $ do
    it "[1..5]" $ sumOfSquaresOnlyPositive [1..5] `shouldBe` sumSquares [1..5]
    it "[n + 1 | [1..10]]" $ do
      let ns = [n * 2 | n <- [1..10]]
      sumOfSquaresOnlyPositive ns `shouldBe` sumSquares ns
    it "[5..9]" $ sumOfSquaresOnlyPositive [5..9] `shouldBe` sumSquares [(-9)..(-5)]
    it "[-9..-5]" $ sumOfSquaresOnlyPositive [(-9)..(-5)] `shouldBe` sumSquares []
    it "[1..2, -3, 3..5)" $ sumOfSquaresOnlyPositive (1:2:(-3):[3..5]) `shouldBe` sumSquares [1..5]
  describe "11-5" $ context "zipWith" $ it "" $ zipWith ($) [sum, product] [[1, 2], [3, 4]] `shouldBe` ([3, 12]::[Integer])
