import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Test.QuickCheck
import System.IO


--- 18.17
hGetInt :: Handle -> IO Integer
hGetInt handle = hGetLine handle >>= \line -> return (read line)


hSumInts :: Handle -> Integer -> IO Integer
hSumInts handle s = hGetInt handle >>= \n ->
  if n == 0
    then return s
    else hSumInts handle (s+n)



main = do handle <- openFile "input.txt" ReadMode
          num <- hSumInts handle 0
          print num
          hClose handle
