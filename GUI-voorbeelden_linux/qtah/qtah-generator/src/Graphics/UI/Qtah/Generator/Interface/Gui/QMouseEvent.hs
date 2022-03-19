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

module Graphics.UI.Qtah.Generator.Interface.Gui.QMouseEvent (
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
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (e_Type)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  fl_KeyboardModifiers,
  e_MouseButton,
  fl_MouseButtons,
  fl_MouseEventFlags,
  e_MouseEventSource,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QInputEvent (c_QInputEvent)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QMouseEvent"]
  [ QtExportEvent c_QMouseEvent
  ]

c_QMouseEvent =
  addReqIncludes [includeStd "QMouseEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QMouseEvent") Nothing [c_QInputEvent] $
  collect
  [ test (qtVersion < [5, 0]) $ mkCtor "new"
    [enumT e_Type, objT c_QPoint, enumT e_MouseButton, flagsT fl_MouseButtons,
     flagsT fl_KeyboardModifiers]
  , test (qtVersion < [5, 0]) $ mkCtor "newWithGlobalPosition"
    [enumT e_Type, objT c_QPoint, objT c_QPoint, enumT e_MouseButton, flagsT fl_MouseButtons,
     flagsT fl_KeyboardModifiers]

  , test (qtVersion >= [5, 0]) $ mkCtor "new"
    [enumT e_Type, objT c_QPointF, enumT e_MouseButton, flagsT fl_MouseButtons,
     flagsT fl_KeyboardModifiers]
  , test (qtVersion >= [5, 0]) $ mkCtor "newWithScreenPosition"
    [enumT e_Type, objT c_QPointF, objT c_QPointF, enumT e_MouseButton, flagsT fl_MouseButtons,
     flagsT fl_KeyboardModifiers]
  , test (qtVersion >= [5, 0]) $ mkCtor "newWithWindowAndScreenPosition"
    [enumT e_Type, objT c_QPointF, objT c_QPointF, objT c_QPointF, enumT e_MouseButton,
     flagsT fl_MouseButtons, flagsT fl_KeyboardModifiers]
  , just $ mkConstMethod "button" np $ enumT e_MouseButton
  , just $ mkConstMethod "buttons" np $ flagsT fl_MouseButtons
  , test (qtVersion >= [5, 3]) $ mkConstMethod "flags" np $ flagsT fl_MouseEventFlags
  , just $ mkConstMethod "globalPos" np $ objT c_QPoint
  , just $ mkConstMethod "globalX" np intT
  , just $ mkConstMethod "globalY" np intT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "localPos" np $ objT c_QPointF
  , just $ mkConstMethod "pos" np $ objT c_QPoint
  , test (qtVersion < [5, 0]) $ mkConstMethod "posF" np $ objT c_QPointF
  , test (qtVersion >= [5, 0]) $ mkConstMethod "screenPos" np $ objT c_QPointF
  , test (qtVersion >= [5, 3]) $ mkConstMethod "source" np $ enumT e_MouseEventSource
  , test (qtVersion >= [5, 0]) $ mkConstMethod "windowPos" np $ objT c_QPointF
  , just $ mkConstMethod "x" np intT
  , just $ mkConstMethod "y" np intT
  ]
