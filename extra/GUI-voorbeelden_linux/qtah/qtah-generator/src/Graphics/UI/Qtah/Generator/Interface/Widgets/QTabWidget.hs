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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QTabWidget (
  aModule,
  e_TabPosition,
  e_TabShape,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
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
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_Corner, e_TextElideMode)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listenerInt)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTabWidget"]
  [ QtExportClassAndSignals c_QTabWidget signals
  , qtExport e_TabPosition
  , qtExport e_TabShape
  ]

(c_QTabWidget, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QTabWidget"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTabWidget") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkMethod' "addTab" "addTab" [ptrT $ objT c_QWidget, objT c_QString] intT
  , just $ mkMethod' "addTab" "addTabWithIcon" [ptrT $ objT c_QWidget, objT c_QIcon, objT c_QString]
    intT
  , just $ mkMethod "clear" np voidT
  , just $ mkConstMethod "cornerWidget" [enumT e_Corner] $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "count" np intT
  , just $ mkProp "currentIndex" intT
  , just $ mkProp "currentWidget" $ ptrT $ objT c_QWidget
  , test (qtVersion >= [4, 5]) $ mkProp "documentMode" boolT
  , test (qtVersion >= [4, 2]) $ mkProp "elideMode" $ enumT e_TextElideMode
  , test (qtVersion >= [4, 2]) $ mkProp "iconSize" $ objT c_QSize
  , just $ mkConstMethod "indexOf" [ptrT $ objT c_QWidget] intT
  , just $ mkMethod' "insertTab" "insertTab" [intT, ptrT $ objT c_QWidget, objT c_QString] intT
  , just $ mkMethod' "insertTab" "insertTabWithIcon"
    [intT, ptrT $ objT c_QWidget, objT c_QIcon, objT c_QString] intT
  , just $ mkConstMethod "isTabEnabled" [intT] boolT
  , test (qtVersion >= [4, 5]) $ mkBoolIsProp "movable"
  , just $ mkMethod "removeTab" [intT] voidT
  , just $ mkMethod "setCornerWidget" [ptrT $ objT c_QWidget, enumT e_Corner] voidT
  , just $ mkMethod "setTabEnabled" [intT, boolT] voidT
  , just $ mkMethod "setTabIcon" [intT, objT c_QIcon] voidT
  , just $ mkMethod "setTabText" [intT, objT c_QString] voidT
  , just $ mkMethod "setTabToolTip" [intT, objT c_QString] voidT
  , test (qtVersion >= [4, 1]) $ mkMethod "setTabWhatsThis" [intT, objT c_QString] voidT
    -- TODO tabBar
  , test (qtVersion >= [5, 4]) $ mkProp "tabBarAutoHide" boolT
  , just $ mkConstMethod "tabIcon" [intT] $ objT c_QIcon
  , just $ mkProp "tabPosition" $ enumT e_TabPosition
  , just $ mkProp "tabShape" $ enumT e_TabShape
  , just $ mkConstMethod "tabText" [intT] $ objT c_QString
  , just $ mkConstMethod "tabToolTip" [intT] $ objT c_QString
  , just $ mkConstMethod "tabWhatsThis" [intT] $ objT c_QString
  , test (qtVersion >= [4, 5]) $ mkProp "tabsClosable" boolT
  , test (qtVersion >= [4, 2]) $ mkProp "usesScrollButtons" boolT
  , just $ mkConstMethod "widget" [intT] $ ptrT $ objT c_QWidget
  ]

e_TabPosition =
  makeQtEnum (ident1 "QTabWidget" "TabPosition") [includeStd "QTabWidget"]
  [ "North"
  , "South"
  , "West"
  , "East"
  ]

e_TabShape =
  makeQtEnum (ident1 "QTabWidget" "TabShape") [includeStd "QTabWidget"]
  [ "Rounded"
  , "Triangular"
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "currentChanged" listenerInt
  , makeSignal "tabBarClicked" listenerInt
  , makeSignal "tabBarDoubleClicked" listenerInt
  , makeSignal "tabCloseRequested" listenerInt
  ]
