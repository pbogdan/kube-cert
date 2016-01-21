{-# LANGUAGE FlexibleContexts #-}

module Lib.Env
    (withEnv) where

import Control.Exception.Lifted (bracket)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Environment (setEnv)


withEnv
    :: (Traversable t, MonadBaseControl IO m, MonadIO m)
    => t (String, String) -> m c -> m c
withEnv envvars action =
    bracket
    (forM envvars $ \ (name, val) -> liftIO $ setEnv name val)
    (\ _ -> forM envvars $ \ (name, _) -> liftIO $ setEnv name "")
    (const action)
