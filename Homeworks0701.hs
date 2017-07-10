import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import System.IO

getInt :: IO Integer
getInt = do line <- getLine
            return (read line)

sumInts :: Integer -> IO Integer
sumInts s
  = do n <- getInt
       if n == 0
         then return s
         else sumInts (s+n)

hGetInt :: Handle -> IO Integer
hGetInt handle = do line <- hGetLine handle
                    return (read line)


hSumInts :: Handle -> Integer -> IO Integer
hSumInts handle s
  = do n <- hGetInt handle
       if n == 0
         then return s
         else hSumInts handle (s+n)


--  handle   <- openFile filename ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

main
  = do putStrLn "File name"
