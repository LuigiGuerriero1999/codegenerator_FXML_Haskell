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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QMenuBar (
  aModule,
  c_QMenuBar,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (enumT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (wsWince)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_Corner)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listenerPtrQAction)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QMenuBar"]
  [ QtExportClassAndSignals c_QMenuBar signals ]

(c_QMenuBar, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QMenuBar"] $
  classSetEntityPrefix "" $
  makeClass (ident "QMenuBar") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkConstMethod "actionAt" [objT c_QPoint] $ ptrT $ objT c_QAction
  , just $ mkConstMethod "actionGeometry" [ptrT $ objT c_QAction] $ objT c_QRect
  , just $ mkProp "activeAction" $ ptrT $ objT c_QAction
  , just $ mkMethod' "addAction" "addAction" [ptrT $ objT c_QAction] voidT
  , just $ mkMethod' "addAction" "addNewAction" [objT c_QString] $ ptrT $ objT c_QAction
  , just $ mkMethod' "addMenu" "addMenu" [ptrT $ objT c_QMenu] $ ptrT $ objT c_QAction
  , just $ mkMethod' "addMenu" "addNewMenu" [objT c_QString] $ ptrT $ objT c_QMenu
  , just $ mkMethod' "addMenu" "addNewMenuWithIcon" [objT c_QIcon, objT c_QString] $
    ptrT $ objT c_QMenu
  , just $ mkMethod "addSeparator" np $ ptrT $ objT c_QAction
  , just $ mkMethod "clear" np voidT
  , just $ mkConstMethod "cornerWidget" [enumT e_Corner] $ ptrT $ objT c_QWidget
  , test wsWince $ mkProp "defaultAction" $ ptrT $ objT c_QAction
  , just $ mkBoolIsProp "defaultUp"
  , just $
    mkMethod "insertMenu" [ptrT $ objT c_QAction, ptrT $ objT c_QMenu] $ ptrT $ objT c_QAction
  , just $ mkMethod "insertSeparator" [ptrT $ objT c_QAction] $ ptrT $ objT c_QAction
  , just $ mkBoolIsProp "nativeMenuBar"
  , just $ mkMethod "setCornerWidget" [ptrT $ objT c_QWidget, enumT e_Corner] voidT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "hovered" listenerPtrQAction
  , makeSignal "triggered" listenerPtrQAction
  ]
