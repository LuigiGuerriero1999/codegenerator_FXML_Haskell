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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QStackedLayout (
  aModule,
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
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (enumT, intT, objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listenerInt)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QStackedLayout"] $
  collect
  [ just $ QtExportClassAndSignals c_QStackedLayout signals
  , test (qtVersion >= [4, 4]) $ qtExport e_StackingMode
  ]

(c_QStackedLayout, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QStackedLayout"] $
  classSetEntityPrefix "" $
  makeClass (ident "QStackedLayout") Nothing [c_QLayout] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithLayout" [ptrT $ objT c_QLayout]
  , just $ mkMethod "addWidget" [ptrT $ objT c_QWidget] intT
  , just $ mkConstMethod "count" np intT
  , just $ mkProp "currentIndex" intT
  , just $ mkProp "currentWidget" $ ptrT $ objT c_QWidget
  , just $ mkMethod "insertWidget" [intT, ptrT $ objT c_QWidget] intT
  , test (qtVersion >= [4, 4]) $ mkProp "stackingMode" $ enumT e_StackingMode
  , just $ mkConstMethod "widget" [intT] $ ptrT $ objT c_QWidget
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "currentChanged" listenerInt
  , makeSignal "widgetRemoved" listenerInt
  ]

e_StackingMode =
  makeQtEnum (ident1 "QStackedLayout" "StackingMode") [includeStd "QStackedLayout"]
  [ "StackOne"
  , "StackAll"
  ]
