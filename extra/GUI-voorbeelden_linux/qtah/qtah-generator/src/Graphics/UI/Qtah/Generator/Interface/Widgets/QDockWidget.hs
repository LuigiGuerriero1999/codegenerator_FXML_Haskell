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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QDockWidget (
  aModule,
  c_QDockWidget,
  e_DockWidgetFeature,
  fl_DockWidgetFeatures,
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
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  e_DockWidgetArea,
  fl_DockWidgetAreas,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listenerBool,
  listenerDockWidgetArea,
  listenerDockWidgetAreas,
  listenerQDockWidgetFeatures,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QDockWidget"]
  [ QtExportClassAndSignals c_QDockWidget signals
  , qtExport e_DockWidgetFeature
  , qtExport fl_DockWidgetFeatures
  ]

(c_QDockWidget, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QDockWidget"] $
  classSetEntityPrefix "" $
  makeClass (ident "QDockWidget") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithText" [objT c_QString]
  , just $ mkCtor "newWithTextAndParent" [objT c_QString, ptrT $ objT c_QWidget]
    -- TODO Ctor with Qt::WindowFlags.
  , just $ mkProp "allowedAreas" $ flagsT fl_DockWidgetAreas
  , just $ mkConstMethod "isAreaAllowed" [enumT e_DockWidgetArea] boolT
  , just $ mkProp "features" $ flagsT fl_DockWidgetFeatures
  , just $ mkBoolIsProp "floating"
  , test (qtVersion >= [4, 3]) $ mkProp "titleBarWidget" $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "toggleViewAction" np $ ptrT $ objT c_QAction
  , just $ mkProp "widget" $ ptrT $ objT c_QWidget
  ]

signalGens :: [SignalGen]
signalGens =
  collect
  [ just $ makeSignal "allowedAreasChanged" listenerDockWidgetAreas
  , test (qtVersion >= [4, 3]) $ makeSignal "dockLocationChanged" listenerDockWidgetArea
  , just $ makeSignal "featuresChanged" listenerQDockWidgetFeatures
  , just $ makeSignal "topLevelChanged" listenerBool
  , just $ makeSignal "visibilityChanged" listenerBool
  ]

(e_DockWidgetFeature, fl_DockWidgetFeatures) =
  makeQtEnumAndFlags (ident1 "QDockWidget" "DockWidgetFeature") "DockWidgetFeatures"
  [includeStd "QDockWidget"]
  [ "NoDockWidgetFeatures"
  , "DockWidgetClosable"
  , "DockWidgetMovable"
  , "DockWidgetFloatable"
  , "AllDockWidgetFeatures"  -- Deprecated
  , "DockWidgetVerticalTitleBar"
  ]
