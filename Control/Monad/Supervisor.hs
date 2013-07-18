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
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Control.Monad.Supervisor where

import Control.Monad.State

data Control a = Control a | Forward a | Backward

newtype Sup m a = Sup { runSup :: m (Control a ) }


class MonadState s m => Supervise s m where
   supBack     :: s -> m ()
   supBack = const $ return ()
   
   supervise ::    m (Control a) -> m (Control a)
   supervise= id

breturn :: Monad m => a -> Sup m a
breturn = Sup . return . Control 

instance MonadState s m =>Supervise s m

instance  Supervise s m => Monad (Sup  m) where
    fail   _ = Sup . return $ Backward
    return x = Sup . return $ Forward x
    x >>= f  = Sup $ loop
     where
     loop = do
        s <- get
        v <-  supervise $ runSup x                        
        case v of
            Forward y  -> supervise $ runSup (f y)         
            Control y  -> do
                 z <- supervise $ runSup (f y)            
                 case z of
                  Backward  -> supBack s >> loop           
                  other   -> return other
            Backward  ->  return  Backward

instance MonadTrans Sup  where
  lift f = Sup $  f >>= return . Forward

instance (MonadState s m, MonadIO m) => MonadIO (Sup m) where
  liftIO iof= Sup $ liftIO iof  >>= return . Forward

instance Supervise s m => MonadState s (Sup m) where
   get= lift get                               
   put= lift . put



