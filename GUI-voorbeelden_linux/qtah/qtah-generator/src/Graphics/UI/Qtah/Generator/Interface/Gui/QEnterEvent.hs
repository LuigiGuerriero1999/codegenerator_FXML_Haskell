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

module Graphics.UI.Qtah.Generator.Interface.Gui.QEnterEvent (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  np,
  )
import Foreign.Hoppy.Generator.Types (intT, objT)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [5, 0]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Gui", "QEnterEvent"] minVersion
  [ QtExportEvent c_QEnterEvent ]

c_QEnterEvent =
  addReqIncludes [includeStd "QEnterEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QEnterEvent") Nothing [c_QEvent]
  [ mkCtor "new" [objT c_QPointF, objT c_QPointF, objT c_QPointF]
  , mkConstMethod "globalPos" np $ objT c_QPoint
  , mkConstMethod "globalX" np intT
  , mkConstMethod "globalY" np intT
  , mkConstMethod "localPos" np $ objT c_QPointF
  , mkConstMethod "pos" np $ objT c_QPoint
  , mkConstMethod "screenPos" np $ objT c_QPointF
  , mkConstMethod "windowPos" np $ objT c_QPointF
  , mkConstMethod "x" np intT
  , mkConstMethod "y" np intT
  ]
