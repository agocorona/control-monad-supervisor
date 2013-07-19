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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Control.Monad.Supervisor where

import Control.Monad.State

-- | The internal computation can be reexecuted, proceed forward or backward
data Control a = Control a | Forward a | Backward

-- | The supervisor add a Control wrapper that is interpreted by the monad instance
newtype Sup m a = Sup { runSup :: m (Control a ) }

-- | The supervise class add two general modifiers that can be applied:
class MonadState s m => Supervise s m where
   supBack     :: s -> m ()          -- ^ When the computation goes back, by default
                                    -- the previous state is restored This procedure can change
                                    -- that behaviour.
   supBack = const $ return ()
   
   supervise ::    m (Control a) -> m (Control a)  -- ^ When the conputation has been executed
                                                  -- this method is an opportunity for modifying the result
                                                  -- By default: supervise= id
   supervise= id

-- | Flag the computation that executes @breturn@ as a control point.
--
-- When the computation is going back, it will be re-executed (see the monad definition)
breturn :: Monad m => a -> Sup m a
breturn = Sup . return . Control 

instance MonadState s m =>Supervise s m

-- | The Supervisor Monad is in essence an Identity monad transformer when executing Forward.
instance  Supervise s m => Monad (Sup  m) where
    fail   _ = Sup . return $ Backward
    return x = Sup . return $ Forward x
    x >>= f  = Sup $ loop 
     where
     loop = do
        s <- get
        -- execution as usual if supervise== id
        v <-  supervise $ runSup x                        
        case v of
            --  a normal execution if supervise== id
            Forward y  -> supervise $ runSup (f y)

            --   a back was returned, propagate it back
            Backward  ->  return  Backward

            -- the computaton x was a control point. if the branch of execution goes Backward
            -- then x will be reexecuted. supBack will control the state backtracking, how much of
            -- the current state we want to keep and how much we want to backtrack  
            Control y  -> do
                 z <- supervise $ runSup (f y)            
                 case z of
                  Backward  -> supBack s >> loop           
                  other   -> return other


instance MonadTrans Sup  where
  lift f = Sup $  f >>= return . Forward

instance (MonadState s m, MonadIO m) => MonadIO (Sup m) where
  liftIO iof= Sup $ liftIO iof  >>= return . Forward

instance Supervise s m => MonadState s (Sup m) where
   get= lift get                               
   put= lift . put



