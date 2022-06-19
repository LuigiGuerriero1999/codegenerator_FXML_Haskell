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

module Graphics.UI.Qtah.Generator.Interface.Gui.QWheelEvent (
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
import Foreign.Hoppy.Generator.Types (enumT, intT, objT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  fl_KeyboardModifiers,
  fl_MouseButtons,
  e_Orientation,
  e_ScrollPhase,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QInputEvent (c_QInputEvent)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QWheelEvent"]
  [ QtExportEvent c_QWheelEvent
  ]

c_QWheelEvent =
  addReqIncludes [includeStd "QWheelEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QWheelEvent") Nothing [c_QInputEvent] $
  collect
  [ test (qtVersion < [5, 0]) $ mkCtor "new"
    [objT c_QPoint, intT, flagsT fl_MouseButtons, flagsT fl_KeyboardModifiers,
     enumT e_Orientation]
  , test (qtVersion < [5, 0]) $ mkCtor "newWithGlobalPosition"
    [objT c_QPoint, objT c_QPoint, intT, flagsT fl_MouseButtons, flagsT fl_KeyboardModifiers,
     enumT e_Orientation]

  , test (qtVersion >= [5, 0]) $ mkCtor "new"
    [objT c_QPointF, objT c_QPointF, objT c_QPoint, objT c_QPoint, intT, enumT e_Orientation,
     flagsT fl_MouseButtons, flagsT fl_KeyboardModifiers]
  , test (qtVersion >= [5, 2]) $ mkCtor "newWithPhase"
    [objT c_QPointF, objT c_QPointF, objT c_QPoint, objT c_QPoint, intT, enumT e_Orientation,
     flagsT fl_MouseButtons, flagsT fl_KeyboardModifiers, enumT e_ScrollPhase]
  , test (qtVersion >= [5, 0]) $ mkConstMethod "angleDelta" np $ objT c_QPoint
  , just $ mkConstMethod "buttons" np $ flagsT fl_MouseButtons
  , test (qtVersion < [5, 0]) $ mkConstMethod "delta" np intT
  , just $ mkConstMethod "globalPos" np $ objT c_QPoint
  , test (qtVersion >= [5, 0]) $ mkConstMethod "globalPosF" np $ objT c_QPointF
  , just $ mkConstMethod "globalX" np intT
  , just $ mkConstMethod "globalY" np intT
  , test (qtVersion >= [5, 2]) $ mkConstMethod "phase" np $ enumT e_ScrollPhase
  , test (qtVersion >= [5, 0]) $ mkConstMethod "pixelDelta" np $ objT c_QPoint
  , just $ mkConstMethod "pos" np $ objT c_QPoint
  , test (qtVersion >= [5, 0]) $ mkConstMethod "posF" np $ objT c_QPointF
  , just $ mkConstMethod "x" np intT
  , just $ mkConstMethod "y" np intT
  ]
