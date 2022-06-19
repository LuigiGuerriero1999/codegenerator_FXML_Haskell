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

module Graphics.UI.Qtah.Generator.Interface.Core.QAnimationGroup (
  aModule,
  c_QAnimationGroup,
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
import Foreign.Hoppy.Generator.Types (intT, voidT, objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractAnimation (c_QAbstractAnimation)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QAnimationGroup"] [4, 6] $
  [qtExport c_QAnimationGroup]

c_QAnimationGroup =
  addReqIncludes [ includeStd "QAnimationGroup" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QAnimationGroup") Nothing [c_QAbstractAnimation] $
  collect
  [ just $ mkMethod "addAnimation" [ptrT $ objT c_QAbstractAnimation] voidT
  , just $ mkConstMethod "animationAt" [intT] $ ptrT $ objT c_QAbstractAnimation
  , just $ mkConstMethod "animationCount" np intT
  , just $ mkMethod "clear" np voidT
  , just $ mkConstMethod "duration" np intT
  , just $ mkConstMethod "indexOfAnimation" [ptrT $ objT c_QAbstractAnimation] intT
  , just $ mkMethod "insertAnimation" [intT, ptrT $ objT c_QAbstractAnimation] voidT
  , just $ mkMethod "removeAnimation" [ptrT $ objT c_QAbstractAnimation] voidT
  , just $ mkMethod "takeAnimation" [intT] $ ptrT $ objT c_QAbstractAnimation
  ]
