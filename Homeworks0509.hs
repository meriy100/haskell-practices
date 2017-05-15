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

main :: IO ()
main = hspec $ do
  describe "7-28" $ do
    context "joinLine" $ do
      it "cat dog and bird" $ do
        (joinLine ["cat", "dog", "bird"]) `shouldBe` "cat dog bird"
      it "empty" $ do
        (joinLine []) `shouldBe` ""
