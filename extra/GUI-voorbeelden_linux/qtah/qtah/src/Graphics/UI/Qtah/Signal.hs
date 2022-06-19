-- This file is part of Qtah.
--
-- Copyright 2015-2021 The Qtah Authors.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

-- | General routines for managing Qt signals.
module Graphics.UI.Qtah.Signal (
  Signal (..),
  Connection,
  connect,
  connect_,
  disconnect,
  connectionIsValid,
  -- * Internal
  internalMakeConnection,
  ) where

import Control.Concurrent (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad (unless)
import Data.Maybe (isJust)
import Foreign.Hoppy.Runtime (Deletable, delete)

data SomeDeletable = forall a. Deletable a => SomeDeletable a

-- | A signal that can be connected to an instance of the @object@ (C++) class,
-- and when invoked will call a function of the given @handler@ type.
data Signal object handler = Signal
  { internalConnectSignal :: object -> handler -> IO (Maybe Connection)
  , internalName :: String
  }

instance Show (Signal object handler) where
  show signal = concat ["<Signal ", internalName signal, ">"]

-- | A handle representing a callback function that has been connected to a Qt
-- signal on a specific object.  A connection is created by 'connect' or
-- 'connect_'.  The callback function may be unregistered by passing this object
-- to the 'disconnect' function, after which 'connectionIsValid' will return
-- false.
newtype Connection = Connection (MVar (Maybe SomeDeletable))

-- | Internal function.  Constructs a 'Connection' from a listener object.
internalMakeConnection :: Deletable a => a -> IO Connection
internalMakeConnection listener = do
  listenerVar <- newMVar $ Just $ SomeDeletable listener
  return $ Connection listenerVar

-- | Registers a handler function to listen to a signal an object emits.  If the
-- connection is made successfully, a handle representing the connection is
-- returned, otherwise @Nothing@ is returned.
connect :: object -> Signal object handler -> handler -> IO (Maybe Connection)
connect = flip internalConnectSignal

-- | Registers a handler function to listen to a signal an object emits, via
-- 'connect'.  If the connection fails, then the program aborts.
connect_ :: object -> Signal object handler -> handler -> IO ()
connect_ object signal handler = do
  maybeConnection <- connect object signal handler
  unless (isJust maybeConnection) $
    fail $ "connect_: Failed to connect signal " ++ show signal ++ "."

-- | Disconnects a connection previously created through 'connect', returning
-- true on success.
--
-- Passing a connection that has already been disconnected does nothing and
-- returns false.
disconnect :: Connection -> IO Bool
disconnect (Connection listenerVar) =
  modifyMVar listenerVar $ \maybeListener -> case maybeListener of
    Just (SomeDeletable listener) -> do
      delete listener
      return (Nothing, True)
    Nothing ->
      return (Nothing, False)

-- | Returns true if the connection is still valid, i.e. its callback function
-- is still connected to its signal.
connectionIsValid :: Connection -> IO Bool
connectionIsValid (Connection listenerVar) = isJust <$> readMVar listenerVar
