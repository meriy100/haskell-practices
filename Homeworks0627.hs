import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Test.QuickCheck

factorialNumber 0 = 1
factorialNumber n = n * factorialNumber (n - 1)
factorial = [factorialNumber x | x <- [0..]]

main :: IO ()
main = hspec $ do
  describe "17-23" $ do
    context "factorialNumber" $ do
      it "3" $ do
        factorialNumber 3 `shouldBe` 6
