import Test.Hspec
-- import Control.Exception (evaluate)
-- import Test.QuickCheck
-- import System.Random

putStrLn = putStr . (++ "\n")
inc = (+1)

absolute :: Num a => Ord a => a -> a
absolute x
  | x < 0 = -x
  | otherwise = x

elem' :: a -> [a] -> Bool
elem' a [] = False


-- test = absolute "a"


main :: IO ()
main = hspec $ do
  describe "" $ do
    context "" $ do
      it "" $ do
        print "ok"
