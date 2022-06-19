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

module Graphics.UI.Qtah.Generator.Interface.Gui.QDropEvent (
  aModule,
  c_QDropEvent,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  mkProp,
  mkCtor,
  np,
  )
import Foreign.Hoppy.Generator.Types (constT, objT, enumT, voidT, ptrT)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent, e_Type)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QMimeData (c_QMimeData)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_KeyboardModifiers, fl_DropActions, fl_MouseButtons, e_DropAction)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QDropEvent"]
  [ QtExportEvent c_QDropEvent ]

c_QDropEvent =
  addReqIncludes [includeStd "QDropEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QDropEvent") Nothing [c_QEvent]
  [ mkCtor "new" [ objT c_QPointF, flagsT fl_DropActions, ptrT $ constT $ objT c_QMimeData, flagsT fl_MouseButtons, flagsT fl_KeyboardModifiers ]
  , mkCtor "newWithType" [ objT c_QPointF, flagsT fl_DropActions, ptrT $ constT $ objT c_QMimeData, flagsT fl_MouseButtons, flagsT fl_KeyboardModifiers, enumT e_Type]
  , mkMethod "acceptProposedAction" np voidT
  , mkProp "dropAction" $ enumT e_DropAction
  , mkConstMethod "keyboardModifiers" np $ flagsT fl_KeyboardModifiers
  , mkConstMethod "mimeData" np $ ptrT $ constT $ objT c_QMimeData
  , mkConstMethod "mouseButtons" np $ flagsT fl_MouseButtons
  , mkConstMethod "pos" np $ objT c_QPoint
  , mkConstMethod "posF" np $ objT c_QPointF
  , mkConstMethod "possibleActions" np $ flagsT fl_DropActions
  , mkConstMethod "proposedAction" np $ enumT e_DropAction
  , mkConstMethod "source" np $ ptrT $ objT c_QObject
  ]
