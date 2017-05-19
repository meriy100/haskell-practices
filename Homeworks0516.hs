import Test.Hspec
-- import Control.Exception (evaluate)
-- import Test.QuickCheck
-- import System.Random

-- memo
putStrLn = putStr . (++ "\n")
inc = (+1)


-- 10-2
length' :: Num a => [a1] -> a
length' xs = sum $ map (\_ -> 1) xs


main :: IO ()
main = hspec $ describe "" $ do
    context "length'" $ do
      it "abcd" $ length' "abcd" `shouldBe` (4::Integer)
      it "[1..10]" $ length' [1..10] `shouldBe` (10::Integer)
