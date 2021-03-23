import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import System.IO
import Data.Bits
import Data.Char
import Data.Maybe
import Prelude as P
import Data.List
import Data.Time
import Data.Map as M
import Data.Maybe(fromMaybe)
import qualified Data.Vector.Mutable as VM
---------------------------------------------------------------------------------------------
----------------------------------knut_morris_pratt------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
type Table = V.Vector Int

makeprefix :: B.ByteString -> Table
makeprefix str = 
  let n = B.length str
  in V.create $ do
    p <- VM.new n
    VM.set p 0
    let loop i k | i == n = return ()
                 | (k > 0 && (B.index str k /= B.index str i)) = do
                    help <- VM.read p (k - 1)
                    loop i help
                 | (B.index str k == B.index str i) = do
                    VM.write p i (k + 1)
                    loop (i + 1) (k + 1)
                 | otherwise = do
                    VM.write p i k
                    loop (i + 1) k
    loop 1 0
    return p

findSubstring :: B.ByteString -> B.ByteString -> Table -> Int
findSubstring what wher table =
  let loop i k | (k == B.length what) = i - (B.length what)
               | (i == B.length wher) = -1
               | (k > 0 && B.index wher i /= B.index what k) = loop i $ table V.! (k - 1)
               | otherwise = 
                  let smth_ne = if (B.index wher i == B.index what k)
                                then (k + 1)
                                else k
                  in loop (i + 1) smth_ne
  in loop 0 0

knut_morris_pratt :: B.ByteString -> B.ByteString -> Int
knut_morris_pratt what wher = findSubstring what wher (makeprefix what)
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------boyerMur----------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
makeDict :: B.ByteString -> Map Int Int
makeDict what = M.fromList [if (B.elem (chr i) what) then (fromEnum (B.index what (fromMaybe 0 $ (B.elemIndexEnd (chr i) what))), B.length what - (fromMaybe 0 $ (B.elemIndexEnd (chr i) what)))
   else (i, B.length what) | i <- [0..255]]

help :: B.ByteString -> B.ByteString -> Map Int Int -> Int -> Int
help what wher dict len = 
  let loop i j k | (j > 0 && i <= B.length wher) = if (B.index what (j - 1) == B.index wher (k - 1))
                                                   then loop i (j - 1) (k - 1)
                                                   else loop (i + dict!(fromEnum (B.index wher (k - 1)))) (B.length what) i
                 | otherwise = if (j <= 0)
                                     then (i - 2*len)
                                     else -1
  in loop len len len


boyerMur :: B.ByteString -> B.ByteString -> Int
boyerMur what wher = help what wher (makeDict what) (B.length what)
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
------------------------------------------Main-----------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
main = do
    scan <- readFile "test_3.txt"
    begT <- getCurrentTime
    print(boyerMur (B.pack "abv") (B.pack scan))
    endT <- getCurrentTime
    putStrLn " " 
    putStrLn "Time is (boyerMur):" 
    putStrLn $ init $ show $ diffUTCTime endT begT
    putStrLn " "

    begT <- getCurrentTime
    print(knut_morris_pratt (B.pack "abv") (B.pack scan))
    endT <- getCurrentTime
    putStrLn " " 
    putStrLn "Time is (knut_morris_pratt):" 
    putStrLn $ init $ show $ diffUTCTime endT begT
    putStrLn " "