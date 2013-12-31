{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances,
    MultiParamTypeClasses, FlexibleInstances
    ,FlexibleContexts, UndecidableInstances #-}

-- | This module add a 'MonadLoc' instance to the 'Supervisor' monad. This instance generates a trace when a
-- uncaugh exception is raised.
--
-- . See the MonadLoc package: <http://hackage.haskell.org/package/monadloc>
--
-- The package control-monad-exception produces call stacks using @monadloc@, but the @Supervisor@ monad
-- produces execution traces thanks to the backtracking mechanism.
--
-- The trace is produced after the exception is raised. So it does not generate
-- overhead in normal execution.
--
-- For more finer control of exceptions, ej. for retrowing exceptions managed outside the Supervisor monad
-- , create your own instance
--
-- Execute the example at @Demos/TraceExample.hs@
--
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
--
-- Produce this trace:
--
--  @
--  \"hello\"
--  \"world\"
--  TraceExample.hs: TRACE (error in the last line):
--  .
--  main, Demos.TraceExample(Demos\TraceExample.hs): (23, 18)
--  main, Demos.TraceExample(Demos\TraceExample.hs): (26, 4)
--  example, Demos.TraceExample(Demos\TraceExample.hs): (30, 13)
--  example, Demos.TraceExample(Demos\TraceExample.hs): (32, 15)
--  exception: Prelude.undefined
--  @

-- TO DO:  extend it for forward traces and test coverages

module Control.Monad.Supervisor.Trace(runTrace) where
import Control.Monad.State
import Control.Monad.Supervisor
import Control.Monad.Loc
import Control.Monad.Catch as CMC
import Control.Exception (SomeException)
import Data.List(intersperse)


type Trace= [String]



instance (MonadLoc m, Supervise Trace m, MonadCatch m)=> MonadLoc (Sup Trace m) where
    withLoc loc (Sup f) =  Sup $ do
       withLoc loc $ do
             r <- f `CMC.catch` handler1
             trace <- get 
             case trace of
                  []     ->  return r                      -- all ok
                  trace  ->  put (loc:trace) >> return r   -- is going back with a trace, we add one more line
             return r

       where
       -- detected failure, add the first line of trace with the error, init execution back
       handler1 (e :: SomeException)=    put ["exception: " ++show e]  >> return Backward



-- | Execute an Supervisor computation and raise an error with a trace when an uncaugh exception
-- is raised. It is necessary to preprocess the file with the monadloc-pp preprocessor.
--
-- Otherwise, it produces the same error with no trace.
runTrace :: Supervise [String] m => Sup [String] m a -> m (Control a)
runTrace  f=  runSup f1
  where
  f1= printBackTrace >> f
  printBackTrace= do
     s <- get
     case  s of
       [] -> breturn()
       tr -> error (disp tr)
     where
     disp tr= "TRACE (error in the last line):\n\n" ++(concat $ intersperse "\n" tr)

---- A less polimorphic version of runTrace. It assumes a state monad for the sole purpose
---- of capturing traces
--runTraceState  :: Monad m => Sup (StateT [String] m) a -> m (Control a)
--runTraceState f= evalStateT (runTrace f) []

