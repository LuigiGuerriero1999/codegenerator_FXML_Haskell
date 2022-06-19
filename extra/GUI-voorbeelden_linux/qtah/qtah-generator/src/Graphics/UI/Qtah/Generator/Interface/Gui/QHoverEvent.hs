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

module Graphics.UI.Qtah.Generator.Interface.Gui.QHoverEvent (
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
import Foreign.Hoppy.Generator.Types (enumT, objT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent, e_Type)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_KeyboardModifiers)
import Graphics.UI.Qtah.Generator.Interface.Gui.QInputEvent (c_QInputEvent)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QHoverEvent"]
  [ QtExportEvent c_QHoverEvent
  ]

c_QHoverEvent =
  addReqIncludes [includeStd "QHoverEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QHoverEvent") Nothing
  [if qtVersion >= [5, 0] then c_QInputEvent else c_QEvent] $
  collect
  [ test (qtVersion < [5, 0]) $ mkCtor "new" [enumT e_Type, objT c_QPoint, objT c_QPoint]
  , test (qtVersion >= [5, 0]) $ mkCtor "new" [enumT e_Type, objT c_QPointF, objT c_QPointF]
  , test (qtVersion >= [5, 0]) $ mkCtor "newWithModifiers"
    [enumT e_Type, objT c_QPointF, objT c_QPointF, flagsT fl_KeyboardModifiers]
  , just $ mkConstMethod "oldPos" np $ objT c_QPoint
  , test (qtVersion >= [5, 0]) $ mkConstMethod "oldPosF" np $ objT c_QPointF
  , just $ mkConstMethod "pos" np $ objT c_QPoint
  , test (qtVersion >= [5, 0]) $ mkConstMethod "posF" np $ objT c_QPointF
  ]
