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

module Graphics.UI.Qtah.Generator.Interface.Core.QThread (
  aModule,
  c_QThread,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkStaticMethod,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, uintT, ulongT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listener)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QThread"]
  [ QtExportClassAndSignals c_QThread signals
  , qtExport e_Priority
  ]

(c_QThread, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QThread"] $
  classSetEntityPrefix "" $
  makeClass (ident "QThread") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
    -- TODO eventDispatcher property
  , just $ mkMethod' "exit" "exit" np voidT
  , just $ mkMethod' "exit" "exitWithCode" [intT] voidT
  , just $ mkConstMethod "isFinished" np boolT
  , test (qtVersion >= [5, 2]) $ mkConstMethod "isInterruptionRequested" np boolT
  , just $ mkConstMethod "isRunning" np boolT
  , test (qtVersion >= [5, 5]) $ mkConstMethod "loopLevel" np intT
  , just $ mkProp "priority" $ enumT e_Priority
  , just $ mkMethod "quit" np voidT
  , test (qtVersion >= [5, 2]) $ mkMethod "requestInterruption" np voidT
  , just $ mkProp "stackSize" uintT
  , just $ mkMethod' "start" "start" np voidT
  , just $ mkMethod' "start" "startWithPriority" [enumT e_Priority] voidT
  , just $ mkMethod "terminate" np voidT
  , just $ mkMethod' "wait" "wait" np voidT
  , just $ mkMethod' "wait" "waitWithMillis" [ulongT] voidT

    -- Static methods.
  , just $ mkStaticMethod "currentThread" np $ ptrT $ objT c_QThread
    -- TODO currentThreadId
  , just $ mkStaticMethod "idealThreadCount" np intT
  , just $ mkStaticMethod "msleep" [ulongT] voidT
  , just $ mkStaticMethod "sleep" [ulongT] voidT
  , just $ mkStaticMethod "usleep" [ulongT] voidT
  , just $ mkStaticMethod "yieldCurrentThread" np voidT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignalPrivate "finished" listener
  , makeSignalPrivate "started" listener
  ]

e_Priority =
  makeQtEnum (ident1 "QThread" "Priority") [includeStd "QThread"]
  [ "IdlePriority"
  , "LowestPriority"
  , "LowPriority"
  , "NormalPriority"
  , "HighPriority"
  , "HighestPriority"
  , "TimeCriticalPriority"
  , "InheritPriority"
  ]
