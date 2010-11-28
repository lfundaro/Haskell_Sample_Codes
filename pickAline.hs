import Control.Monad.State
import Control.Monad(liftM)
import Time
import Random
import System(getArgs)


{-| 
Data type that models a state. A state carries 
the actual lines read (linesRead), the selected 
line with 1/k probability (selection), the list
that collects all the read strings from the file 
(info), and the list of random numbers that are used 
to select a random line (randomList).
|-}
data Pack  = Pack { linesRead  :: Int,
                    selection  :: String,
                    info       :: [String],
                    randomList :: [Float] }
             deriving (Show)
                  
{-| 
Combinator used to modify all the attributes that
makes up the state of the State Monad.
|-}
changeCount :: String -> Pack -> Pack
changeCount newLine n = 
  let lineplus = linesRead n + 1
      ranNums  = randomList n
      index    = fromIntegral (truncate (fromIntegral (lineplus) * (head ranNums)))
      record   = info n
      info'    = newLine : record
  in
   n { linesRead = lineplus,
       selection = info' !! index, 
       info      = info',
       randomList = tail ranNums } 
    
{-|    
Converts the file Strings to a State Pack ().
|-}
apply :: String -> State Pack ()
apply str = get >>= put . changeCount str

{-|
Auxiliary function for the noop function. 
The use of noop is useful in the base case 
of the myseq function.
|-}

myseq :: [State Pack ()] -> State Pack ()
myseq [] = noop
myseq l  = foldl1 (>>) l

{-|
Operation that builds the State Pack () monad list 
and threads it with all the present monads 
in this list.
|-}
dum :: [String] -> State Pack ()
dum l = myseq (map (apply) l)

{-| 
Generation of random numbers.
|-}
randomNum :: (Random a) => Int -> [a]
randomNum seed = randoms (mkStdGen seed)
  
{-|
The noop function is used in the base case 
of myseq.
|-}
noop :: State Pack ()
noop = return ()

{-|
Creates a Pack element with an execState to execute 
all the states the State Monad goes through, from the 
beginning to the end.
|-}
initial :: [Float] -> Pack
initial ran = Pack { linesRead = 0,
                     selection = "",
                     info = [],
                     randomList = ran}
           
{-|
Function that generates a seed and returns a list of 
Floats. This list is used with the State Monad to
select a random line. 
|-}
generarSeed :: IO [Float]
generarSeed = do
  c <- getClockTime
  cal <- toCalendarTime c
  let seed = (ctDay cal) + (ctHour cal) + (ctMin cal) + (ctSec cal)
  let result = randomNum seed :: [Float]
  return result
  
{-|
Main function that reads the contents of a file 
given as an argument and returns a selected line 
with choosing probability of 1/k.
|-}
main :: IO ()
main = do 
  x <- getArgs
  y <- liftM lines . readFile $ head x
  b <- generarSeed 
  let a = dum y
  let k = execState a (initial $ b)
  print $ selection k
  