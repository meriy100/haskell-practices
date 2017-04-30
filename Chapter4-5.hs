import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

-- module Chapter4 where
--- 4-2
weakAscendingOrder :: Integer -> Integer -> Integer -> Bool
weakAscendingOrder x y z
 | (x < y) && (y < z) = True
 | otherwise = False

between :: Integer -> Integer -> Integer -> Bool
between x y z
 | weakAscendingOrder x y z == True = True
 | weakAscendingOrder z y x == True = True
 | otherwise  = False


--4-11

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

--4-19
multiplication :: Integer -> Integer -> Integer
multiplication x y
 | y == 0 = 0
 | y > 0 = x + multiplication x (y - 1)


--4.22

----------for debug function----------
fac :: Integer -> Integer
fac n
 | n == 0 = 1
 | n > 0 = fac(n - 1) * n
 | otherwise = 0
----------for debug function----------


-- zeroFunction :: (Integer -> Integer) -> Integer -> Bool
-- zeroFunction f n
--  | (f n == 0 || f n == False) = True
--  | (n == 0) = False
--  | otherwise = zeroFunction f (n - 1)


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
