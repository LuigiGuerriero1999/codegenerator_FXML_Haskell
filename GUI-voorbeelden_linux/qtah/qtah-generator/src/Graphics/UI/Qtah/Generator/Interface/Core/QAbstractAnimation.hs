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

module Graphics.UI.Qtah.Generator.Interface.Core.QAbstractAnimation (
  aModule,
  c_QAbstractAnimation,
  e_DeletionPolicy,
  e_Direction,
  e_State,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod',
  mkMethod,
  np,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerDirection,
  listenerInt,
  listenerStateState,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QAbstractAnimation"] [4, 6] $
  QtExportClassAndSignals c_QAbstractAnimation signals :
  collect
  [ just $ qtExport e_DeletionPolicy
  , just $ qtExport e_Direction
  , just $ qtExport e_State
  ]

(c_QAbstractAnimation, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [ includeStd "QAbstractAnimation" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractAnimation") Nothing [c_QObject] $
  collect
  [ just $ mkConstMethod "currentLoopTime" np intT
  , just $ mkConstMethod "duration" np intT
  --, just $ mkConstMethod "group" np $ ptrT $ objT c_QAnimationGroup
  , just $ mkMethod "pause" np voidT
  , just $ mkMethod "resume" np voidT
  , just $ mkMethod "setPaused" [boolT] voidT
  , just $ mkMethod' "start" "start" np voidT
  , just $ mkMethod' "start" "startWithDeletionPolicy" [enumT e_DeletionPolicy] voidT
  , just $ mkMethod "stop" np voidT
  , just $ mkConstMethod "totalDuration" np intT

    -- TODO
  ]

signalGens :: [SignalGen]
signalGens =
  collect
  [ just $ makeSignal "currentLoopChanged" listenerInt
  , just $ makeSignal "directionChanged" listenerDirection
  , just $ makeSignal "finished" listener
  , just $ makeSignal "stateChanged" listenerStateState
  ]

e_DeletionPolicy =
  makeQtEnum (ident1 "QAbstractAnimation" "DeletionPolicy") [includeStd "QAbstractAnimation"]
  [ "KeepWhenStopped"
  , "DeleteWhenStopped"
  ]

e_Direction =
  makeQtEnum (ident1 "QAbstractAnimation" "Direction") [includeStd "QAbstractAnimation"]
  [ "Forward"
  , "Backward"
  ]

e_State =
  makeQtEnum (ident1 "QAbstractAnimation" "State") [includeStd "QAbstractAnimation"]
  [ "Stopped"
  , "Paused"
  , "Running"
  ]
