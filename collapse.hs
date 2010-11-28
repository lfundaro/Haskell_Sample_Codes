import Data.List(foldl')
import Control.Parallel
import System.Random
import Data.Word (Word32)
import Test.QuickCheck.Gen
import Test.QuickCheck
import System.Environment
import Control.Monad
import Data.Sequence(Seq,fromList,empty,index)
import Control.Parallel.Strategies
import Control.Concurrent
import System.IO.Unsafe


{-| 
  Collapse a list of Word32. It takes advantage of inmediate 
  evaluation of foldl'. It uses the go function to collapse the 
  integer list.
-}
collapse :: [Word32] -> Word32
collapse [] = 0
collapse xs = collapseInt 0 $ foldl' (go) 0 xs

{-| 
  Takes a number, it takes its collapse and sums it up with
  the following element in the list.
 -}
go :: Word32 -> Word32 -> Word32
go x y = x + p 
    where !p = collapseInt 0 y

{-|
    Takes a number and collapses it until it becomes 
    a single digit.
  -}
collapseInt :: Word32 -> Word32 -> Word32
collapseInt res x
    | x > 0 = collapseInt h f
    | res > 9 = collapseInt g i
    | otherwise = res 
    where
      !f = (x `div` 10)
      !g = (res `div`10)
      !h = (res + x `mod` 10)
      !i = (0 + res`mod`10)
       
{-| 
  Applies parallelism dividing the list in chunks of 
  100000 elements. The used constant (100000) tells us 
  that parellelism will be used when list are sufficiently 
  bing.
 -}
divideNconquer :: [Word32]  -> Word32
divideNconquer [] = 0
divideNconquer x  = q `par` p `pseq` p + q
    where
      (r,s) = splitAt 100000 x
      p     = foldl' (go) 0 r
      q     = divideNconquer s
              
{-| 
  Makes a collapse of a list in parallel. During the 
  implementation of this function, it was concluded 
  that using list could be not so good in terms of performance 
  if we want to apply parallelism. Dividing the list on
  several pieces generates new copies of the list, so the 
  garbage collector starts working more frequently and that 
  makes productivity go down. It is recommended to use unboxed arrays 
  (UArrays) for its low access cost. 
 -}
pcollapse :: [Word32] -> Word32
pcollapse k = collapseInt 0 (divideNconquer k)  

{-| 
  The homework is run by putting 
  the seed followed by the size of the list.
 -}
main = do
       args  <- map read `liftM` getArgs
       k     <- aBigList (head args) (last args)
       print $ pcollapse k

{-| 
  Functions that generate the instances.
 -}
instance Random Word32 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

integralRandomR (a,b) g = case randomR (c,d) g of
                            (x,h) -> (fromIntegral x, h)
    where (c,d) = (fromIntegral a :: Integer, fromIntegral b :: Integer)

instance Arbitrary Word32 where
    arbitrary   = choose (minBound, maxBound)

aBigList :: Int -> Int -> IO [Word32]
aBigList s n = do
  let g     = mkStdGen s
      gs r0 = r1 : gs r2 where (r1,r2) = split r0
      m     = unGen arbitrary
  return [(m r n) | (r,n) <- gs g `zip` [1..n] ]
