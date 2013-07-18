{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}

-- | add a MonadLoc instance to the Supervisor monad that generate a trace in case of uncaugh exception
-- . See the MonadLoc package: <http://hackage.haskell.org/package/monadloc>
--
-- the package control-monad-loc produces call stacks using monadloc, but the Supervisor monad
-- produces execution traces thanks to the backtracking mechanism.
--
-- The trace is produced after the error is produced not at normal execution time, so it does not generate
-- overhead in normal execution.
--
-- Execute the example at Demos.TraceExample.hs it has an error
--
-- > {-# OPTIONS -F -pgmF MonadLoc #-}
-- > module Demos.TraceExample (
-- >
-- > ) where
-- >
-- > import Control.Monad.Loc
-- > import Control.Monad.Supervisor.Trace
-- > import Control.Monad.Trans
-- >
-- > main= runTrace $ do
-- >    liftIO $ print "hello"
-- >
-- >    example
-- >
-- > example=
-- >    if True
-- >       then  do
-- >               liftIO $ print "world"
-- >               liftIO $ undefined
-- >
-- >       else liftIO $ print "not there"

-- produce this trace:

-- > "hello"
-- > "world"
-- > TraceExample.hs: TRACE (error in the last line):
-- >
-- > main, Demos.TraceExample(Demos\TraceExample.hs): (23, 18)
-- > main, Demos.TraceExample(Demos\TraceExample.hs): (26, 4)
-- > example, Demos.TraceExample(Demos\TraceExample.hs): (30, 13)
-- > example, Demos.TraceExample(Demos\TraceExample.hs): (32, 15)
-- > exception: Prelude.undefined

-- TO DO:  extend it for forward traces and test coverages

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



