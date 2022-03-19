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

module Graphics.UI.Qtah.Generator.Interface.Gui.QContextMenuEvent (
  aModule,
  c_QContextMenuEvent,
  e_Reason,
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
  np,
  )
import Foreign.Hoppy.Generator.Types (enumT, intT, objT)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Gui.QInputEvent (c_QInputEvent)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_KeyboardModifiers)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QContextMenuEvent"]
  [ QtExportEvent c_QContextMenuEvent
  , qtExport e_Reason
  ]

c_QContextMenuEvent =
  addReqIncludes [includeStd "QContextMenuEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QContextMenuEvent") Nothing [c_QInputEvent]
  [ mkCtor "new" [enumT e_Reason, objT c_QPoint]
  , mkCtor "newWithGlobalPos" [enumT e_Reason, objT c_QPoint, objT c_QPoint]
  , mkCtor "newWithGlobalPosAndModifiers" [enumT e_Reason, objT c_QPoint, objT c_QPoint, flagsT fl_KeyboardModifiers]
  , mkConstMethod "globalPos" np $ objT c_QPoint
  , mkConstMethod "globalX" np intT
  , mkConstMethod "globalY" np intT
  , mkConstMethod "pos" np $ objT c_QPoint
  , mkConstMethod "reason" np $ enumT e_Reason
  , mkConstMethod "x" np intT
  , mkConstMethod "y" np intT
  ]

e_Reason =
  makeQtEnum (ident1 "QContextMenuEvent" "Reason") [includeStd "QContextMenuEvent"]
  [ "Mouse"
  , "Keyboard"
  , "Other"
  ]
