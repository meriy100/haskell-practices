import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Test.QuickCheck
import System.Random

--- 7-28
type Line = [String]
joinLine :: Line -> String
joinLine (word:line)
  | (length line) > 0 = joinLine ( ( word ++ " " ++ (head line) ):(tail line) )
  | otherwise = word
joinLine _ =  ""

--- 7-29
joinLines :: Line -> String
joinLines (word:line)
  | (length line) > 0 = joinLines ( ( word ++ "\n" ++ (head line) ):(tail line) )
  | otherwise = word
joinLines _ =  ""

--- 9-5

prop_sum_check xs ys = sum(xs ++ ys) == sum xs + sum ys

generateArray len
  | len > 0 = ((1):(generateArray (len - 1)))
  | otherwise = []

main :: IO ()
main = hspec $ do
  describe "7-28" $ do
    context "joinLine" $ do
      it "cat dog and bird" $ do
        (joinLine ["cat", "dog", "bird"]) `shouldBe` "cat dog bird"
      it "empty" $ do
        (joinLine []) `shouldBe` ""
  describe "7-29" $ do
    context "joinLines" $ do
      it "cat dog and bird" $ do
        (joinLines ["cat", "dog", "bird"]) `shouldBe` "cat\ndog\nbird"
      it "empty" $ do
        (joinLine []) `shouldBe` ""
  describe "9-5" $ do
    context "xs.length and ys.length == 1" $ do
      it "should be true" $ do
        (prop_sum_check (generateArray 1) (generateArray 1)) `shouldBe` True
    context "xs.length == k " $ do
      it "should be true but limit 100" $ do
        k <- randomRIO(2, 100) :: IO Int
        h <- randomRIO(2, 100) :: IO Int
        (prop_sum_check (generateArray k) (generateArray h)) `shouldBe` True
        (prop_sum_check (generateArray (k+1)) (generateArray h)) `shouldBe` True
        (prop_sum_check (generateArray k) (generateArray h)) `shouldBe` True
        (prop_sum_check (generateArray k) (generateArray (h+1))) `shouldBe` True
