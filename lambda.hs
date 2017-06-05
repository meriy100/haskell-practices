{-# LANGUAGE FlexibleInstances #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Test.QuickCheck
import System.Random


elemGem :: (a -> a -> Bool) -> a -> [a] -> Bool
elemGem f x [] = False
elemGem f x (y:ys) = (x `f` y) || elemGem f x ys

allEqual :: (Eq a) => a -> a -> a -> Bool
allEqual n m p = (n == m) && (n == p)

sec = (+1)

prodFun :: (Int -> Bool) -> (Int -> Char) -> Int -> (Bool, Char)
prodFun f g = \x -> (f x, g x)


foo :: a -> (a, [a])
foo x = (x, [x])
bar :: ([b], c) -> c
bar ([y], z) = z
foobar = bar . foo




class Blankable a where
  blank :: a -> Bool
  present :: a -> Bool
  blank a = not (present a)
  present a = not (blank a)

-- data Hoo = BLANK | PRESENT deriving (Blankable)

instance Blankable Char where
  blank ' ' = True
  blank _ = False

instance Blankable String where
  blank [] = True
  blank str = all blank str
instance Blankable Int where
  blank _ = False

main :: IO ()
main = hspec $ do
  describe "" $ do
    context "" $ do
      it "" $ do
        print "ok"

