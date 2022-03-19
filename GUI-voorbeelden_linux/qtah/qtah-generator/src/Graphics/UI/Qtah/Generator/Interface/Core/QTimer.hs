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

module Graphics.UI.Qtah.Generator.Interface.Core.QTimer (
  aModule,
  c_QTimer
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkMethod,
  mkConstMethod,
  mkCtor,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listener)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QTimer"]
  [ QtExportClassAndSignals c_QTimer signals
  ]

(c_QTimer, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QTimer"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTimer") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkConstMethod "interval" np intT
  -- , just $ mkConstMethod "intervalAsDuration" np $ objT c_std::chrono::milliseconds
  , just $ mkConstMethod "isActive" np boolT
  , just $ mkConstMethod "isSingleShot" np boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "remainingTime" np intT
  -- , just $ mkConstMethod "remainingTimeAsDuration" np $ objT c_std::chrono::milliseconds
  , just $ mkMethod "setInterval" [intT] voidT
  -- , just $ mkMethod' "setInterval" "setInterval" [objT c_std::chrono::milliseconds] voidT
  , just $ mkMethod "setSingleShot" [boolT] voidT
  -- , just $ mkMethod "setTimerType" [objT c_Qt::TimerType] voidT
  , just $ mkMethod "start" [intT] voidT
  , just $ mkConstMethod "timerId" np intT
  -- , just $ mkConstMethod "timerType" np $ objT c_Qt::TimerType
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignalPrivate "timeout" listener
  ]
