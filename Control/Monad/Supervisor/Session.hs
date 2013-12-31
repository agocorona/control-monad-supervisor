-----------------------------------------------------------------------------
--
-- Module      :  Control.Monad.Supervisor.Session
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
{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances,
             MultiParamTypeClasses,
             FlexibleContexts,
             UndecidableInstances #-}
module Control.Monad.Supervisor.Session where
import Control.Monad.State
--import Control.Monad.Supervisor
import Data.Map as M
import Data.Typeable
import Data.Dynamic


onNothing m exc= do
   r <- m
   case r of
     Nothing -> exc
     Just r  -> return r


-- | Set user-defined data in the context of the session.
--
-- The data is indexed by  type in a map. So the user can insert-retrieve different kinds of data
-- in the session context to add different behaviours to the supervisor monad.
--




type SessionData= M.Map TypeRep Dynamic

emptySessionData= M.empty

setSessionData :: ( MonadState SessionData m, Typeable a) => a -> m ()  
setSessionData  x=
  modify $ \session ->  M.insert  (typeOf x ) (toDyn x)  session

delSessionData :: ( MonadState SessionData m, Typeable a) => a -> m ()
delSessionData x=
 modify $ \session ->  M.delete  (typeOf x )  (session :: M.Map TypeRep Dynamic)
  
-- | Get the session data of the desired type if there is any.
getSessionData ::  (Typeable a, MonadState SessionData m) =>  m (Maybe a)
getSessionData =  resp where
 resp= get  >>= \list  ->
    case M.lookup ( typeOf $ typeResp resp) list :: Maybe Dynamic of
      Just x  -> return $ cast x
      Nothing -> return Nothing
 typeResp :: m (Maybe x) -> x
 typeResp= undefined



runs= flip evalStateT

main= do
   r <- runs emptySessionData  $ do
            setSessionData  True
            getSessionData
   print (r :: Maybe Bool)
