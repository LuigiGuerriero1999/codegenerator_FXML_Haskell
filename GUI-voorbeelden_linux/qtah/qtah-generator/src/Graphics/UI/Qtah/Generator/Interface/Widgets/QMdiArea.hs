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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QMdiArea (
  aModule,
  c_QMdiArea,
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
  mkMethod,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_WindowFlags)
import Graphics.UI.Qtah.Generator.Interface.Gui.QBrush (c_QBrush)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listenerPtrQMdiSubWindow)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QMdiSubWindow (c_QMdiSubWindow)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QTabWidget (e_TabPosition, e_TabShape)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QMdiArea"]
  [ QtExportClassAndSignals c_QMdiArea signals
  , qtExport e_AreaOption
  , qtExport fl_AreaOptions
  , qtExport e_ViewMode
  , qtExport e_WindowOrder
  ]

(c_QMdiArea, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QMdiArea"] $
  classSetEntityPrefix "" $
  makeClass (ident "QMdiArea") Nothing [c_QWidget] $
  collect
  [ -- Ctors:
    just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
    -- Properties:
  , just $ mkProp "activationOrder" $ enumT e_WindowOrder
  , just $ mkProp "background" $ objT c_QBrush
  , just $ mkProp "documentMode" boolT
  , just $ mkProp "tabPosition" $ enumT e_TabPosition
  , just $ mkProp "tabShape" $ enumT e_TabShape
  , just $ mkProp "tabsClosable" boolT
  , just $ mkProp "tabsMovable" boolT
  , just $ mkProp "viewMode" $ enumT e_ViewMode
    -- Methods:
  , just $ mkConstMethod "activeSubWindow" np $ ptrT $ objT c_QMdiSubWindow
  , just $ mkMethod' "addSubWindow" "addSubWindow"
    [ptrT $ objT c_QWidget] $ ptrT $ objT c_QMdiSubWindow
  , just $ mkMethod' "addSubWindow" "addSubWindowWithFlags"
    [ptrT $ objT c_QWidget, flagsT fl_WindowFlags] $ ptrT $ objT c_QMdiSubWindow
  , just $ mkConstMethod "currentSubWindow" np $ ptrT $ objT c_QMdiSubWindow
  , just $ mkMethod "removeSubWindow" [ptrT $ objT c_QWidget] voidT
  , just $ mkMethod "setOption" [enumT e_AreaOption, boolT] voidT
    -- TODO subWindowList
  , just $ mkConstMethod "testOption" [enumT e_AreaOption] boolT
    -- Slots:
  , just $ mkMethod "activateNextSubWindow" np voidT
  , just $ mkMethod "activatePreviousSubWindow" np voidT
  , just $ mkMethod "cascadeSubWindows" np voidT
  , just $ mkMethod "closeActiveSubWindow" np voidT
  , just $ mkMethod "closeAllSubWindows" np voidT
  , just $ mkMethod "setActiveSubWindow" [ptrT $ objT c_QMdiSubWindow] voidT
  , just $ mkMethod "tileSubWindows" np voidT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "subWindowActivated" listenerPtrQMdiSubWindow
  ]

(e_AreaOption, fl_AreaOptions) =
  makeQtEnumAndFlags (ident1 "QMdiArea" "AreaOption") "AreaOptions" [includeStd "QMdiArea"]
  [ "DontMaximizeSubWindowOnActivation"
  ]

e_ViewMode =
  makeQtEnum (ident1 "QMdiArea" "ViewMode") [includeStd "QMdiArea"]
  [ "SubWindowView"
  , "TabbedView"
  ]

e_WindowOrder =
  makeQtEnum (ident1 "QMdiArea" "WindowOrder") [includeStd "QMdiArea"]
  [ "CreationOrder"
  , "StackingOrder"
  , "ActivationHistoryOrder"
  ]
