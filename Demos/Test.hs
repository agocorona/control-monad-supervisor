-----------------------------------------------------------------------------
--
-- Module      :
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :

-----------------------------------------------------------------------------
{-# OPTIONS -F -pgmF MonadLoc #-}
module Demos.Test(

) where


import Control.Monad.Loc
import Control.Monad.Supervisor
import Control.Monad.Trans
import Control.Monad.State

import Debug.Trace

(!>)= flip trace

stateTEval :: Monad m => s -> StateT s m a -> m a
stateTEval= flip evalStateT



main= do
 print "hello IO monad"
 stateTEval 0 $ runSup $ do
    breturn()
    n <- get
    put (3 :: Int) !> (show n)
    change


change= do
   put 4
   fail ""


