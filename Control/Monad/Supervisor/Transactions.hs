-----------------------------------------------------------------------------
--
-- Module      :  Control.Monad.Supervisor.Transactions
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
{-#LANGUAGE FlexibleInstances, MultiParamTypeClasses,
            GeneralizedNewtypeDeriving, FlexibleContexts
            ,UndecidableInstances, DeriveDataTypeable #-}
module Control.Monad.Supervisor.Transactions (

) where
import Control.Monad.State
import Control.Monad.Supervisor hiding (breturn)
import qualified Control.Monad.Supervisor as Sup(breturn)
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Supervisor.Session hiding (runs)
import Data.Typeable




breturn x= setSessionData (BackTracking False) >> Sup.breturn x

instance MonadState SessionData m => Supervise  SessionData m where
     supBack s= setSessionData (BackTracking True) !> "supBack"

--     supervise _ mx= setSessionData (BackTracking False) >> mx !> "supervise"

--instance (MonadState a m ,MonadState b m)=> MonadState (a,b) m where
--  get= do
--    x <- get
--    y <- get
--    return (x,y)
--
--  put (x,y)= put x >> put y

class  Monad m => BackTrackingStatus m where
   isGoingBack :: m Bool


class Monoid t => HasInverse t where
  cancel :: t -> t
  -- property: forall w, w => v <> t <> w <> cancel t === v <> w

newtype BackTracking= BackTracking Bool deriving Typeable

processReversibleEvent :: (Supervise SessionData m, HasInverse a)=> a -> a -> (a -> Sup SessionData m()) -> Sup SessionData m ()
processReversibleEvent resource event update =  do
   now <- isGoingBack
   if  now  !> ("back="++ show now) then  update (resource <> cancel event) >> fail ""
           else  update (resource <> event) >> breturn ()

--processIrreversibleEvent :: (BackTrackingStatus m, Monoid a)=> a -> a -> Sup m a -> Sup m a
--processIrreversibleEvent resource event failAction= do
--   now <- lift $ isGoingBack
--   if not now
--    then breturn $ resource <> event
--    else do
--         breturn()  -- will not go back beyond this
--         failAction

instance Monoid Int where mempty=0; mappend =(+)
newtype Potatoes= Potatoes Int deriving (Num, Monoid, Typeable)

instance HasInverse Potatoes where cancel = negate



instance Supervise SessionData m => BackTrackingStatus  m where
   isGoingBack= do
     BackTracking b <- getSessionData `onNothing` (return (BackTracking False) !> "backtracking not set" ) !> "isgoingback"
     return b

runs= flip evalStateT


main= do
  print "hi"
  runs emptySessionData $ runSup $ do
      setSessionData (BackTracking True)
      getSessionData `onNothing` error "not set" >>= \(BackTracking b) -> liftIO (print b)

      Potatoes potatoes <- getSessionData `onNothing` return (Potatoes 0)
      liftIO . print $ " Now you have"++ show potatoes ++ " in your shopping cart"
      liftIO . print $ "press enter for more potatoes"
      liftIO $ getLine
      liftIO . print $ " you want to add potatoes to the shopping cart"
      processReversibleEvent (Potatoes potatoes) (Potatoes 5) setSessionData


      liftIO . print $  "but wait!. you do not need potatoes, you need oranges!. "
      liftIO . print $  "No problem. press enter to drop the potatoes from the cart"
      liftIO $ getLine
      fail ""





