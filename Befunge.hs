module Main 
    where

--- Copyright Brandon Simmons, 2010
--- http://coder.bsimmons.name/blog/ 

-- TODO:
    -- performance improvements
    -- consider switch over to IO Array or Vector?
    -- catch errors in exec block
    -- constrain grid to max proportions of array,
    --    - expanding up to 80x25 when we have a 'p'
    --   command outside the grid.
    --    - Warn when we try to expand past 80x25, but
    --   do expand.


-- helpers:
import System.Environment
import Data.Char
import Control.Arrow (first,second,(&&&),(|||))

-- we need to be able to modify our array efficiently
-- on 'p' calls. Using an IO Array would be easy here. For
-- now we use basic unboxed immutable array. 
import Data.Array.Unboxed

-- we do a lot of work with IO in the State monad using StateT:
   --- using 'mtl' library
import Control.Monad.State.Strict
   --- using 'transformers' library:  SLOWER THAN 'MTL'
--import Control.Monad.Trans.State.Strict
--import Control.Monad
--import Control.Monad.IO.Class

-- we need to be able to do randomness if we see '?':
import System.Random

-- error catching:
import System.IO
import System.IO.Error

--------------------------------------------------------------------------------

-- position of the "program counter":
type Position = (Int,Int)

-- directions as contintuations:
type Direction = Position -> Position

-- the source code is mapped onto a 2d array as Int ASCII values:
type Code = UArray Position Int

-- The stack stores integers, per the Funge-93 spec. When in "stringmode"
-- we push the ASCII value of the characters we read: 
type Stack = [ Int ]

-- the state of our befunge program's execution. This is passed around
-- and modified as IO actions are performed:
data ProgramState = 
    ES { -- state of the code and stack:
         code :: Code,    
         stack :: Stack,
         -- state of program flow:
         position :: Position, 
         direction :: Direction,
         haltBit :: Bool,
         -- random generator:
         randGen :: StdGen, 
         -- should we announce messages and warnings?:
         verbose :: Bool
        } 

-- the default initial state of a befunge computation:
defaultState =  ES  { 
         -- these are defaults:
         haltBit    = False,
         position   = (0,0), 
         direction  = right,
         stack      = [],
         randGen    = mkStdGen 31337, 
         verbose    = True,  
         code       = array ((0,0),(0,0)) []  }



-- we are using the State monad for our plumbing, but we need to be in 
-- the IO monad, so we use the State monad transformer! This is our
-- read-evaluate-print loop:
type REPL a = StateT ProgramState IO a

-- our command line paramaters:
data Opts = Opts { quiet :: Bool,
                   allow_oversize :: Bool,
                   seed :: Maybe Int,
                   srcFile :: FilePath  }

-- by default we assume input is a correct funge-93 program:
defaultOpts = Opts False False Nothing ""
                           
usage = "\n\tusage: " ++
    "befunge-93 [--quiet] [--allow-oversize] [--seed INT] source.bf\n"


--------------------------------------------------------------------------------
                       --PROGRAM INITIALIZATION--
--------------------------------------------------------------------------------

main = do 
    -- parse command line options, then build our initial state for our befunge
    -- computation, catching any IO errors in the process:
    --state_i <- try $ processOpts . parseArgs =<< getArgs
    args <- getArgs
    opts <- either parsingErrors return (parseArgs args)
    state_i <- try $ processOpts opts
    
    -- it seems most programs expect input to be line-buffered: 
    hSetBuffering stdin LineBuffering

     -- if there are errors, then print them and exit...
    either processingErrors
           -- else supply our initial state to the State/IO magic monadic
           -- machinery and begin executing our befunge program:
           (evalStateT evalLoop)  state_i


--------------------------------------------------------------------------------
                           --EVALUATION LOOPS--
--------------------------------------------------------------------------------

evalLoop :: REPL ()
evalLoop = do
     -- extract the command at our position & execute it:
    getCmd >>= execute
     -- halt if @ command issued, else move and recurse:
    halting <- gets haltBit
    unless halting (move >> evalLoop) 
       

-- we push characters' ASCII values onto the stack until we hit " again:
stringModeLoop :: REPL ()
stringModeLoop = do 
    c <- getCmd
    unless (c == '"') $ do
           push (ord c)
           move
           stringModeLoop
    
-- this is the code that does all the work: a function which takes a program
-- state and does IO operations in the State/IO monads:
execute :: Char -> REPL ()
execute c = 
    case c of
    -- stack operations: --
         '+' -> do (a,b) <- pop2
                   push (a+b)

         '-' -> do (a,b) <- pop2
                   push (b-a)
         
         '*' -> do (a,b) <- pop2
                   push (a*b)
         
         -- according to the befunge-98 spec, befunge-93 should ask the user
         -- what the result of the division by zero should be. so elegant:
         '/' -> do (a,b) <-pop2
                   safeZero  b quot a  >>= push 
         
         '%' -> do (a,b) <- pop2
                   safeZero  b rem a  >>= push  
                
         '!' -> do i <- pop
                   if i == 0 
                      then push 1
                      else push 0

         '`' -> do (a,b) <- pop2 
                   if b>a
                      then push 1
                      else push 0

         '$' -> do pop	
                   return ()
         
         -- Pop value and output as an integer. funge-98 spec calls for
         -- integer to be followed by a space, so we'll do that too:
         '.' -> do i <- pop
                   liftIO $ putStr $ show i ++" "
         
         -- Pop value and output as ASCII character
         ',' -> do i <- pop
                   liftIO $ putChar $ chr i

         '\\' -> do (a,b) <- pop2
                    push a
                    push b
         
         ':' -> do a <- pop
                   replicateM_ 2 (push a)
         
    -- program flow commands: --
         ' ' -> return ()    

         '>' -> setDirection right
        
         '<' -> setDirection left	
       
         '^' -> setDirection up
      
         'v' -> setDirection down
         
         '?' -> getRandomDirection >>= setDirection 
     
         -- hop over the next cell:
         '#' -> move
    
         '@' -> halt
         
    -- stack and program flow: --
         '"' -> move  >>  stringModeLoop
                   
         
         '_' -> do a <- pop
                   if a == 0
                      then setDirection right
                      else setDirection left
         
         '|' -> do a <- pop 	
                   if a == 0
                      then setDirection down
                      else setDirection up
         
    -- IO operations: --
         'p' -> do (y,x) <- pop2
                   pop  >>= putCell (x,y) 
                   
         'g' -> do (y,x) <- pop2 	
                   getCell (x,y) >>= push

         '&' -> liftIO askInt >>= push
         
         '~' -> liftIO askChar >>= push

         x -> if isDigit x 
               then push (digitToInt x)
                -- halt and complain if invalid command:
               else do loc <- gets position
                       halt
                       maybeSay$ "unknown command '" ++ show x ++ 
                                 "' in code path at " ++ show loc ++
                                 ". Maybe you want a funge-98 interpreter?"
    

--------------------------------------------------------------------------------
                             -- LOOP HELPERS --  
--------------------------------------------------------------------------------

    -- -- -- SPECIAL ARITHMETIC -- -- --

safeZero :: Int -> (Int -> Int -> Int) -> Int -> REPL Int
safeZero b func a =
       if a /= 0
          then return (b `func` a)
          else do liftIO $ putStr 
                     "\ndiv by zero; enter desired Int result: "
                  liftIO askInt
                  

    -- -- -- STACK HELPERS -- -- --

-- when empty stack, we are supposed to pop 0:
pop :: REPL Int
pop = do 
    st <- gets stack                                    
    if null st                                      
       then return 0                                
       else do modify $ \s-> s{stack = tail st}     
               return (head st)                     

pop2 :: REPL (Int,Int)
pop2 = do a <- pop
          b <- pop
          return (a,b)

push :: Int -> REPL ()
push c = modify $ \s-> s{stack = c : stack s}
    


    -- -- -- MISC STATE HELPERS -- -- --

-- the command character at our position:
getCmd :: REPL Char    
getCmd = do p <- gets position 
            gets (chr . (! p) . code)    


getCell :: Position -> REPL Int
getCell xy = do
    xy' <- wrap xy
    unless (xy == xy') (maybeSay "warning: getCell out of bounds. wrapping.")
    gets ((! xy') . code)


-- modify our array by changing its value at position xy:
putCell :: Position -> Int -> REPL ()
putCell xy c = do
    xy' <- wrap xy
    unless (xy == xy') (maybeSay "warning: putCell out of bounds. wrapping.")
    a <- gets code
    modify $ \s-> s{code = a//[(xy',c)] }


-- update state with new seed, returning a random direction:
getRandomDirection :: REPL Direction
getRandomDirection = do
    (i,g) <- gets (randomR (0,3) . randGen)
    modify $ \s-> s{randGen = g}
    return ([up,down,left,right] !! i)


    -- -- -- PROGRAM FLOW HELPERS -- -- --

halt :: REPL ()
halt = modify $ \s-> s{ haltBit = True }


setDirection :: Direction -> REPL ()
setDirection d = modify $ \s-> s{direction = d}
       

move :: REPL ()
move = do
    -- we need to know when we're out of bounds, so we can wrap:
    (pos,mv) <- gets (position &&& direction)
    pos' <- wrapSimple (mv pos)
    modify $ \s-> s{position = pos'}


-- if we exceed bounds, we wrap like a torus:
wrap :: Position -> REPL Position
wrap (pX,pY) = do
    (bX,bY) <- gets (snd . bounds . code)
    let x = pX `mod` (bX+1)
        y = pY `mod` (bY+1)
    return (x,y)

--   DIFFICULT TO TELL IF THIS IS SIGNIFICANTLY FASTER THAN `wrap`:
-- a more efficient wrapping function, for the program counter. Because
-- the PC moves only one space at a time, we can use this in the 'move'
-- function:
wrapSimple :: Position -> REPL Position
wrapSimple (pX,pY) = do
    (bX,bY) <- gets (snd . bounds . code)
    let x | pX < 0  = bX
          | pX > bX = 0
          | otherwise = pX
        y | pY < 0  = bY
          | pY > bY = 0
          | otherwise = pY
    return (x,y)



-- we use continuations for the direction, because we never need to inspect
-- our direction, thus a function is appropriate:
down, up, left, right :: Direction
down  = second (+1)
right = first  (+1)
left  = first  (subtract 1)
up    = second (subtract 1)



    -- -- -- IO HELPERS -- -- --

askInt :: IO Int
askInt = do n <- getLine 
            if isInt n 
               then return (read n)
               else putStr "NOT AN INT. Try again: " >> askInt
                    

askChar :: IO Int
askChar = getChar >>= return . ord    


isInt :: String -> Bool
isInt = all isDigit

    -- -- -- ERROR AND DEBUGGING HELPERS -- -- --

maybeSay :: String -> REPL ()
maybeSay m = do
    v <- gets verbose
    when v (liftIO $ putStrLn m )



--------------------------------------------------------------------------------
                          -- MAIN IO HELPERS --
--------------------------------------------------------------------------------


    -- -- -- CLI ARGUMENTS PROCESSING -- -- -- 

-- convert user-supplied options in our Opt container into
-- an initial state, in the IO monad:
processOpts :: Opts -> IO ProgramState
processOpts os = do
   let loud  = not $ quiet os        -- print warnings
       bigOk = allow_oversize os     -- okay if grid is oversize
   
    -- build code grid from source file; we return the code array
    -- and a Bool set to True if the input file was larger than the
    -- standard 80x25:
   hdl <- openFile (srcFile os) ReadMode
    -- This should be compatible with UTF-8, and any symbols outside
    -- the allowed Funge-93 charset will raise an error in `execute`:
   hSetEncoding hdl latin1 
   src <- hGetContents hdl
   let (wasBig,arr) = buildGrid bigOk src
    
    -- initialize the RNG either from user-supplied seed, or
    -- with the system generator:
   rng <- maybe getStdGen (return . mkStdGen) (seed os)

   when (loud && wasBig) $ putStrLn $
        if bigOk
           then "WARNING: using code area larger than 80x25."
           else "WARNING: source was truncated to 80x25; " ++ 
                "use --allow-oversize for source code of " ++   
                "arbitrary dimensions."                         
   
   return$ defaultState{ code    = arr,
                         randGen = rng,
                         verbose = loud }


-- we catch only errors of parsing the command line arguments here.
-- btw this frankly seems easier than System.Console.GetOpts:
parseArgs :: [String] -> Either IOError Opts
parseArgs [] = Left$ userError$ "SOURCE FILE REQUIRED:"++ usage
parseArgs as = parse as defaultOpts
    where  -- the source code file:
          parse [src] opts = Right$ opts{srcFile = src}
           
           -- simple optional flags:
          parse ("--quiet":as) os = parse as os{quiet = True}
          parse ("--allow-oversize":as) os = parse as os{allow_oversize = True}
           
           -- options with args:
          parse ("--seed":s:as) os = 
                if isInt s
                   then parse as os{seed = Just$ read s}
                   else Left$ userError$ "SEED MUST BE AN INTEGER:"++usage
           
           -- parse errors:
          parse _ _ = Left$ userError $ "ERROR PARSING OPTIONS"++usage



    -- -- -- INPUT PROCESSING -- -- --


-- returns the array of source code, along with a Bool denoting whether
-- the input source file exceeded 80x25 in any of it's dimensions:
buildGrid :: Bool -> String -> (Bool,Code)
buildGrid allowing_oversize str =
    let ls = lines $ fixLineBreaks str
         -- our array must be at least 80x25, but we will expand it if
         -- the source seems to exceed those dimensions anywhere:
        sxN = maximum $ 79 : map (subtract 1 . length) ls
        syN = max 24 (length ls - 1)
        (xN,yN) = if allowing_oversize
                     then (sxN, syN)
                     else (79,24)
         -- we pad the array with spaces:
        pad = repeat ' '
        ls' = take (yN+1) (ls ++ repeat pad)
         -- array is indexed from (0,0) in the upper left corner:
        coords = [ (x,y) | y <- [0..yN], x <- [0..xN] ]
        cells  = map ord $ concatMap (take (xN+1) . (++pad)) ls'
         -- was the source input bigger than 80x25?:
        big = sxN > 79 || syN > 24
        arr = array ((0,0),(xN,yN)) (zip coords cells)
     in (big, arr)



--------------------------------------------------------------------------------
                       -- ERROR AND FILE HANDLING --  
--------------------------------------------------------------------------------

-- convert all three line break types to newlines. This isn't required by
-- the spec, but is useful:
fixLineBreaks :: String -> String
fixLineBreaks [] = []
fixLineBreaks ('\r':'\n':xs) = '\n' : fixLineBreaks xs --win
fixLineBreaks ('\r':xs)      = '\n' : fixLineBreaks xs --old mac
fixLineBreaks (x:xs)         = x : fixLineBreaks xs

-- minimal error handling. An error here seems to mean File Encoding
-- issues:
processingErrors :: IOError -> IO ()
processingErrors e = do
    print e
    putStrLn "* Please report this bug to me so I can fix it *"

-- raise the error retuned by the parsing function: parseArgs
parsingErrors :: IOError -> IO a
parsingErrors = ioError
