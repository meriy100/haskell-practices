module Homeworks0516 where

import Test.Hspec

-- memo
putStrLn :: String -> IO ()
putStrLn = putStr . (++ "\n")

inc :: Integer -> Integer
inc = (+1)
dec :: Integer -> Integer
dec = (1-)



main :: IO ()
main = hspec $ do
  describe "" $ context "" $ it "abcd" $ length "abcd" `shouldBe` 4
