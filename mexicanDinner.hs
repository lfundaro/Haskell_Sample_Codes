import Control.Concurrent.STM
import Random
import Control.Concurrent

{-| 
  Aliases for transactional data.
 -}
type Ingrediente = TVar Bool
type Comiendo    = TVar Bool    
type Mensaje a   = TVar [a]
    
{-|
    Functions to create new TVar Bool within
    the IO Monad.
 -}
newIngr disp = newTVarIO disp
               
newCom  disp = newTVarIO disp
               
newMsg = newTVarIO []
               
{-| 
    Data type that models a mariachi. Within a 
    mariachi data type we have an ID,ingredient, 
    name of ingredient, and a TVar Bool that lets 
    the fourth mariachi know whether the other mariachi 
    is eating or not.
 -}
data Mariachi = Mariachi { nombre :: Int,
                           ingr   :: Ingrediente,
                           ningr  :: String,
                           come :: Comiendo}
{-| 
    When the mariachi has the two ingredients that needs to 
    make his "flauta" he blocks the ingredients. If they're 
    not available then he retries the operation.
-}
agarrar :: Ingrediente -> STM ()
agarrar ingr = do 
  x <- readTVar ingr
  if x then writeTVar ingr False
       else retry
      
{-| 
  It indicates that the mariachi is eating. This is useful 
  so the fourth mariachi doesn't choose and release the ingredient 
  belonging to another mariachi eating.
 -}
yoComo :: TVar Bool -> STM ()
yoComo come = do
  writeTVar come True
  
{-| 
  Indicates that the mariachi is not eating. Whenever that 
  happens, that mariachi becomes a candidate to release his 
  ingredient.
 -}
yoNoComo :: TVar Bool -> STM ()
yoNoComo come = do
  writeTVar come False
            
{-| 
    Writes a messange on the information channel.
 -}
anuncio :: Mensaje a -> a -> STM ()
anuncio msg str = do
  info <- readTVar msg
  writeTVar msg (info ++ [str])

{-| 
  Makes a delay.
-}

randomDelay :: IO ()
randomDelay = do
  r <- randomRIO (100000,500000)
  threadDelay r

{-| 
  Takes an announcement from the information channel 
  and returns it so @imprimir@ can show it on the 
  standard input.
 -}
sacarAnuncio :: Mensaje a -> STM a
sacarAnuncio msg = do
  info <- readTVar msg
  case info of
    [] -> retry
    (x:xs) -> do writeTVar msg xs
                 return x
  
{-| 
    Prints the messages on the information channel to 
    the standard input.
 -}
imprimir msg = do
  info <- atomically $ sacarAnuncio msg
  putStrLn info
  imprimir msg

{-| 
    Chooses two random numbers between 0 and 2. 
    The returned tuple has different components.
 -}
candidatos :: (Int,Int) -> IO (Int,Int)
candidatos (t1,t2) = do
  x <- randomRIO (0,2)
  if (t1 /= x) then return (t1,x)
               else candidatos (t1,t2)
                     
{-| 
  This is the fourth mariachi. Receives a list of mariachis 
  and the information channel in order to make announcements. 
  Chooses two mariachis non deterministically and releases his
  ingredients. The function calls itself recursivly to choose 
  two new mariachis. 
-}
sobrio :: [Mariachi] -> Mensaje [Char] -> IO ()
sobrio listaM msg = do
  t1 <- randomRIO (0,2)
  (n1,n2) <- candidatos(t1,t1)
  let m1 = listaM !! n1
      m2 = listaM !! n2
  atomically $ liberar m1 >> liberar m2 >> 
                 anuncio msg ("Mariachi " ++ show (nombre m1) ++ 
                              " releases " ++ show (ningr m1) ++ " and mariachi " 
                              ++ show (nombre m2) ++ " releases " ++ show (ningr m2)) 
                             >> anuncio msg ("Mariachi " ++ 
                             show (3-n1-n2) ++ " making flauta")
  randomDelay
  sobrio listaM msg
    
{-| 
    Realeases an ingredient belonging to a given mariachi.
    Checks if the mariachi is eating.
 -}
liberar :: Mariachi -> STM ()
liberar m = do
  let status = come m
      ingrediente = ingr m
  stat <- readTVar status
  if stat then retry
          else do 
            writeTVar ingrediente True
       
{-| 
    Models concurrency between mariachis. Receives the mariachi
    that's trying to eat. When ingr1 (ingredient 1) and ingr2 are 
    available then this mariachi notifies that he's eating and he 
    proceeds to eat. Finally he notifies that he finished eating and the 
    function calls itself recursively.
 -}
comer :: Mariachi -> Ingrediente -> Ingrediente -> Mensaje [Char] -> IO ()
comer m ingr1 ingr2 msg = do
  atomically $ agarrar ingr1 >> agarrar ingr2 >> yoComo (come m)
  atomically $ anuncio msg ("Mariachi " ++ show (nombre m) ++ " is eating")
  randomDelay
  atomically $ yoNoComo (come m)
  comer m ingr1 ingr2 msg       
        
main = do 
  guacamole <- newIngr False
  tortilla  <- newIngr False
  rCarne    <- newIngr False
  come1     <- newCom False
  come2     <- newCom False
  come3     <- newCom False
  let m1 = Mariachi { nombre = 0, ingr = guacamole,
                    ningr = "guacamole", come = come1}
      m2 = Mariachi { nombre = 1, ingr = tortilla,
                      ningr = "tortilla", come = come2}
      m3  = Mariachi { nombre = 2, ingr = rCarne,
                    ningr = "meat", 
                       come = come3}
  mensajes <- newMsg
  forkIO $ comer m1 rCarne tortilla mensajes
  forkIO $ comer m2 guacamole rCarne mensajes
  forkIO $ comer m3 guacamole tortilla mensajes
  forkIO $ sobrio [m1,m2,m3] mensajes
  imprimir mensajes
