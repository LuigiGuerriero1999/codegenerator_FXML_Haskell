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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (
  aModule,
  c_QMenu,
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
import Foreign.Hoppy.Generator.Types (boolT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listener, listenerPtrQAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QMenu"]
  [ QtExportClassAndSignals c_QMenu signals ]

(c_QMenu, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QMenu"] $
  classSetEntityPrefix "" $
  makeClass (ident "QMenu") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithTitle" [objT c_QString]
  , just $ mkCtor "newWithTitleAndParent" [objT c_QString, ptrT $ objT c_QWidget]
  , just $ mkConstMethod "actionAt" [objT c_QPoint] $ ptrT $ objT c_QAction
  , just $ mkConstMethod "actionGeometry" [ptrT $ objT c_QAction] $ objT c_QRect
  , just $ mkProp "activeAction" $ ptrT $ objT c_QAction
  , just $ mkMethod' "addAction" "addAction" [ptrT $ objT c_QAction] voidT
  , just $ mkMethod' "addAction" "addNewAction" [objT c_QString] $ ptrT $ objT c_QAction
  , just $ mkMethod' "addAction" "addNewActionWithIcon" [objT c_QIcon, objT c_QString] $
    ptrT $ objT c_QAction
    -- TODO There are a bunch of addAction overloads that take functors (>=5.6); add them?
  , just $ mkMethod' "addMenu" "addMenu" [ptrT $ objT c_QMenu] $ ptrT $ objT c_QAction
  , just $ mkMethod' "addMenu" "addNewMenu" [objT c_QString] $ ptrT $ objT c_QMenu
  , just $ mkMethod' "addMenu" "addNewMenuWithIcon" [objT c_QIcon, objT c_QString] $
    ptrT $ objT c_QMenu
  , test (qtVersion >= [5, 1]) $
    mkMethod' "addSection" "addSection" [objT c_QString] $ ptrT $ objT c_QAction
  , test (qtVersion >= [5, 1]) $
    mkMethod' "addSection" "addSectionWithIcon" [objT c_QIcon, objT c_QString] $
    ptrT $ objT c_QAction
  , just $ mkMethod "addSeparator" np $ ptrT $ objT c_QAction
  , just $ mkMethod "clear" np voidT
  , just $ mkProp "defaultAction" $ ptrT $ objT c_QAction
  , just $ mkMethod' "exec" "exec" np $ ptrT $ objT c_QAction
  , just $ mkMethod' "exec" "execAt" [objT c_QPoint, ptrT $ objT c_QAction] $ ptrT $ objT c_QAction
    -- TODO Static exec
  , just $ mkMethod "hideTearOffMenu" np voidT
  , just $ mkProp "icon" $ objT c_QIcon
  , just $ mkMethod "insertMenu" [ptrT $ objT c_QAction, ptrT $ objT c_QMenu] $
    ptrT $ objT c_QAction
  , test (qtVersion >= [5, 1]) $
    mkMethod' "insertSection" "insertSection"
    [ptrT $ objT c_QAction, objT c_QString] $ ptrT $ objT c_QAction
  , test (qtVersion >= [5, 1]) $
    mkMethod' "insertSection" "insertSectionWithIcon"
    [ptrT $ objT c_QAction, objT c_QIcon, objT c_QString] $ ptrT $ objT c_QAction
  , just $ mkMethod "insertSeparator" [ptrT $ objT c_QAction] $ ptrT $ objT c_QAction
  , just $ mkConstMethod "isEmpty" np boolT
  , just $ mkConstMethod "isTearOffMenuVisible" np boolT
  , just $ mkConstMethod "menuAction" np $ ptrT $ objT c_QAction
  , just $ mkMethod' "popup" "popup" [objT c_QPoint] voidT
  , just $ mkMethod' "popup" "popupAction" [objT c_QPoint, ptrT $ objT c_QAction] voidT
  , just $ mkProp "separatorsCollapsible" boolT
  , just $ mkBoolIsProp "tearOffEnabled"
  , just $ mkProp "title" $ objT c_QString
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "aboutToHide" listener
  , makeSignal "aboutToShow" listener
  , makeSignal "hovered" listenerPtrQAction
  , makeSignal "triggered" listenerPtrQAction
  ]
