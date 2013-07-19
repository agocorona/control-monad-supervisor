-----------------------------------------------------------------------------
--
-- Module      :  Demos.TraceExample
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | This is an example of trace generated by Control.Monad.Supervisor.Trace
--
-- This program has an error and the execution will show the executio trace
--
-- It is necessary to install the packages monadloc and monadloc-pp
-----------------------------------------------------------------------------
{-# OPTIONS -F -pgmF MonadLoc #-}
module Demos.TraceExample (

) where

import Control.Monad.Loc
import Control.Monad.Supervisor.Trace
import Control.Monad.Trans

main= runTrace $ do
   liftIO $ print "hello"

   example

example=
   if True
      then  do
              liftIO $ print "world"
              liftIO $ undefined

      else liftIO $ print "not there"

