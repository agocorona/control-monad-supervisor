{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}

module Control.Monad.Supervisor.Trace where

import Control.Monad.Supervisor
import Control.Monad.Loc
import Control.Monad.State
import Control.Monad.CatchIO as CMC
import Control.Exception (SomeException)
import Data.List(intersperse)


type Trace= [String]

instance (MonadLoc m, Supervise Trace m, MonadCatchIO m)=> MonadLoc (Sup m) where
    withLoc loc (Sup f) =  Sup $ do
       withLoc loc $ do
             r <- f `CMC.catch` handler1  loc
             trace <- get 
             case trace of
                  []     ->  return r                  -- all ok
                  trace  ->  put (loc:trace) >> return r   -- is going back with a trace, we add one more line
             return r

       where
       -- detected failure, added the first line of trace with the error, init execution back
       handler1 loc  (e :: SomeException)=    put ["exception: " ++show e]  >> return Backward

type WState  m = StateT [String] m

runTrace :: Sup (WState IO) () -> IO (Control ())
runTrace  f=  evalStateT (runSup f1) []
  where
  f1= printBackTrace >> f
  printBackTrace= do
     s <- get
     case  s of
       [] -> breturn()
       tr -> error (disp tr)
     where
     disp tr= "TRACE (error in the last line):\n\n" ++(concat $ intersperse "\n" tr)



