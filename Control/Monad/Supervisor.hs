-----------------------------------------------------------------------------
--
-- Module      :  Control.Monad.Supervisor
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | A supervisor monad that explore the execution tree of an internal monad and define extra behaviours thanks to flexible instance definitions for each particular purpose.
-- It can inject new behaviours for backtracking, trace generation, testing, transaction rollbacks etc
-- The supervisor monad is used in the package MFlow to control the routing, tracing, state management, back button management and navigation in general

--
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             UndecidableInstances, DeriveDataTypeable,
             FunctionalDependencies #-}

module Control.Monad.Supervisor where

import Control.Monad.Trans
import Control.Monad.State
import Data.Typeable

import Debug.Trace
(!>)= flip trace

--class  Monad m => MonadState1 s m where
--   get :: m s
--   put :: s -> m ()

-- | The internal computation can be reexecuted, proceed forward or backward
data Control a = Control a | Forward a | Backward deriving (Typeable, Read, Show)

-- | The supervisor add a Control wrapper that is interpreted by the monad instance
newtype Sup s m a = Sup { runSup :: m (Control a ) }

-- | The supervise class add two general modifiers that can be applied:
class  MonadState s m => Supervise s m  where
   supBack :: s -> m ()          -- ^ Called before initiating backtracking in a control point
                                -- When the computation goes back, by default
                                -- the  state is kepth. This procedure can change
                                -- that behaviour. The state passed is the one before the
                                -- computation was executed.
   supBack = const $ return ()
   
   supervise :: s ->  m (Control a) -> m (Control a)  -- ^ When the conputation has been executed
                                                    -- this method is an opportunity for modifying the result
                                                    -- By default: supervise _= id
   supervise= const $ id


-- | Flag the computation that executes @breturn@ as a control point.
--
-- When the computation is going back, it will be re-executed (see the monad definition)
breturn :: Monad m => a -> Sup s m a
breturn = Sup . return . Control 

--instance MonadState () IO where
--  get= return()
--  put= const $ return ()
  
--instance MonadState s m => Supervise s m



-- | The Supervisor Monad is in essence an Identity monad transformer when executing Forward.
instance  Supervise s m => Monad (Sup s m) where
    fail   _ = Sup . return $ Backward
    return x = Sup . return $ Forward x
    x >>= f    = Sup $ loop 
     where
     loop = do
        s <- get
        -- execution as usual if supervise== id
        v <-  supervise s $ runSup x                        
        case v of
            --  a normal execution if supervise== id
            Forward y  -> supervise s $ runSup (f y)

            --   Backward was returned, stop the branch of execution and propagate it back
            Backward  ->  return  Backward

            -- the computaton x was a control point. if the branch of execution goes Backward
            -- then x will be reexecuted. supBack will control the state backtracking, how much of
            -- the current state we want to keep and how much we want to backtrack  
            Control y  -> do
                 z <- supervise s $ runSup (f y)            
                 case z of
                  Backward  -> supBack s >> loop           -- re-execute x   
                  other   -> return other


instance MonadTrans (Sup s) where
  lift f = Sup $  f >>= return . Forward

instance (MonadIO m,Supervise s m)=> MonadIO (Sup s m) where
  liftIO iof= Sup $ liftIO iof  >>= return . Forward

instance Supervise s m => MonadState s (Sup s m) where
   get= lift get
   put = lift . put



