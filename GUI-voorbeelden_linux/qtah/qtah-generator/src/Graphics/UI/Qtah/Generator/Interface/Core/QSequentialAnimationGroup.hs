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

module Graphics.UI.Qtah.Generator.Interface.Core.QSequentialAnimationGroup (
  aModule,
  c_QSequentialAnimationGroup,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  np,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QAnimationGroup (c_QAnimationGroup)
import Graphics.UI.Qtah.Generator.Interface.Core.QPauseAnimation (c_QPauseAnimation)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractAnimation (c_QAbstractAnimation)
import Foreign.Hoppy.Generator.Types (intT, objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listenerQAbstractAnimation)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QSequentialAnimationGroup"] [4, 6]
  [ QtExportClassAndSignals c_QSequentialAnimationGroup signals ]

(c_QSequentialAnimationGroup, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [ includeStd "QSequentialAnimationGroup" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QSequentialAnimationGroup") Nothing [c_QAnimationGroup] $
  collect
  [ just $ mkMethod "addPause" [intT] $ ptrT $ objT c_QPauseAnimation
  , just $ mkMethod "insertPause" [intT, intT] $ ptrT $ objT c_QPauseAnimation
  , just $ mkConstMethod "currentAnimation" np $ ptrT $ objT c_QAbstractAnimation
  ]

signalGens :: [SignalGen]
signalGens =
  collect
  [ just $ makeSignal "currentAnimationChanged" listenerQAbstractAnimation
  ]
