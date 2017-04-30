import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

--- 4-2
weakAscendingOrder :: Integer -> Integer -> Integer -> Bool
weakAscendingOrder x y z
 | (x < y) && (y < z) = True
 | otherwise = False

between :: Integer -> Integer -> Integer -> Bool
between x y z
 | weakAscendingOrder x y z = True
 | weakAscendingOrder z y x = True
 | otherwise  = False

middleNumber :: Integer -> Integer -> Integer -> Integer
middleNumber x y z
  | between y x z = x
  | between x y z = y
  | otherwise = z

-- 4-11 && 4-12


data Move = Rock |
            Paper |
            Scissors
            deriving Eq

instance Show Move where
      show Rock = "r"
      show Paper = "p"
      show Scissors = "s"

data MyResult = Win |
              Lose |
              Draw
              deriving Eq

instance Show MyResult where
  show Win = "win"
  show Lose = "lose"
  show Draw = "draw"

win :: Move -> Move
win Rock = Paper
win Scissors = Rock
win Paper = Scissors


lose :: Move -> Move
lose Rock = Scissors
lose Scissors = Paper
lose Paper = Rock

draw :: Move -> Move
draw Rock = Rock
draw Scissors =  Scissors
draw Paper = Paper

outcome :: Move -> Move -> MyResult
outcome x y
  | win x == y = Win
  | lose x == y = Lose
  | draw x == y = Draw

-- 4-17
rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
  | (n < m) || m < 0 = 0
  | m == n = m
  | n - m == 1 = m*n
  | otherwise = m * rangeProduct(m + 1) (n - 1) * n

-- 4-18
fac :: Integer -> Integer
fac n
  | n == 0 = 1
  | n > 0 = fac(n - 1) * n
  | otherwise = 0

fac' :: Integer -> Integer
fac' n = rangeProduct 1 n


--4-19
multiplication :: Integer -> Integer -> Integer
multiplication x y
  | x < 0 || y < 0 = 0 -- none nutural number
  | y > 0 = x + multiplication x (y - 1)
  | otherwise = y


--4.22

isIncludeZero :: (Integer -> Integer) -> Integer -> Bool
isIncludeZero f n
 | (f n == 0) = True
 | (n == 0) = False
 | otherwise = isIncludeZero f (n - 1)


test1 n = (n + 1) `mod` 5
test2 n = 1

--5.1
maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs x y
  | (x == y) = (x, 2)
  | otherwise = (max x y, 1)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs x y z
  | (x == y && y == z) = (x, 3)
  | (x == y || x == z) = (x, 2)
  | (y == z) = (y, 2)
  | otherwise = (maximum [x, y, z], 1)


main :: IO ()
main = hspec $ do
  describe "4-2" $ do
    context "middleNumber" $ do
      it "x" $ do
        (middleNumber 2 1 3) `shouldBe` (2)
      it "y" $ do
        (middleNumber 2 4 9) `shouldBe` (4)
      it "z" $ do
        (middleNumber 10 38 (-11)) `shouldBe` (10)
  describe "4-11 and 4-12" $ do
    context "Rock is " $ do
      it "draw to Rock" $ do
        (outcome Rock Rock) `shouldBe` (Draw)
      it "win to Scissors" $ do
        (outcome Scissors Rock) `shouldBe` (Win)
      it "lose to Paper" $ do
        (outcome Paper Rock) `shouldBe` (Lose)
    context "Scissors is " $ do
      it "lose to Rock" $ do
        (outcome Rock Scissors) `shouldBe` (Lose)
      it "draw to Scissors" $ do
        (outcome Scissors Scissors) `shouldBe` (Draw)
      it "Win to Paper" $ do
        (outcome Paper Scissors) `shouldBe` (Win)
    context "Paper is " $ do
      it "win to Rock" $ do
        (outcome Rock Paper) `shouldBe` (Win)
      it "lose to Scissors" $ do
        (outcome Scissors Paper) `shouldBe` (Lose)
      it "draw to Paper" $ do
        (outcome Paper Paper) `shouldBe` (Draw)
  describe "4-17" $ do
    context "m < n" $ do
      it "should calc" $ do
        (rangeProduct 2 5) `shouldBe` 120
        (rangeProduct 3 5) `shouldBe` 60
    context "m == n" $ do
      it "should n" $ do
        (rangeProduct 2 2) `shouldBe` 2
    context "m > n" $ do
      it "should 0" $ do
        (rangeProduct 3 2) `shouldBe` 0
    context "m or n is negative" $ do
      it "should 0" $ do
        (rangeProduct (-1) (-1)) `shouldBe` 0
  describe "4-18" $ do
    context "m < n" $ do
      it "should calc" $ do
        (multiplication 2 5) `shouldBe` (2*5)
    context "m > n" $ do
      it "should calc" $ do
        (multiplication 10 9) `shouldBe` (9*10)
    context "m or n = 0" $ do
      it "should calc" $ do
        (multiplication 2 0) `shouldBe` (0)
        (multiplication 0 9) `shouldBe` (0)
  describe "4-22" $ do
    context "test1 = (n + 1) `mod` 5" $ do
      it "n = 10 should True" $ do
        (isIncludeZero test1 10) `shouldBe` True
      it "n = 3 should False" $ do
        (isIncludeZero test1 3) `shouldBe` False
    context "test2 = 1" $ do
      it "should False" $ do
        (isIncludeZero test2 10) `shouldBe` False
