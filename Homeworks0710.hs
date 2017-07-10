import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Test.QuickCheck
import Control.Applicative
import System.IO


--- 18.17
hGetInt :: Handle -> IO Integer
hGetInt handle = hGetLine handle >>= \line -> return (read line)


hSumInts :: Handle -> Integer -> IO Integer
hSumInts handle s = hGetInt handle >>= \n ->
  if n == 0
    then return s
    else hSumInts handle (s+n)


--- 18.18
data Error a = OK a | Error String

instance Functor Error where
  fmap f (OK a) = do x <- OK a
                     return (f a)
  fmap f (Error a) = Error a

instance Applicative Error where
  pure = OK
  OK f <*> OK a = OK (f a)

instance Monad Error where
  (OK x) >>= k = k x
  (Error x) >>= k = Error x
  return = OK

main = do handle <- openFile "input.txt" ReadMode
          num <- hSumInts handle 0
          print num
          hClose handle
