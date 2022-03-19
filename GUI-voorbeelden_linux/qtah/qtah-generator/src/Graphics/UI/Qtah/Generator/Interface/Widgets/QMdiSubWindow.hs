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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QMdiSubWindow (
  aModule,
  c_QMdiSubWindow,
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
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_WindowFlags)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerWindowStatesWindowStates,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMdiArea (c_QMdiArea)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QMdiSubWindow"]
  [ QtExportClassAndSignals c_QMdiSubWindow signals
  , qtExport e_SubWindowOption
  , qtExport fl_SubWindowOptions
  ]

(c_QMdiSubWindow, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QMdiSubWindow"] $
  classSetEntityPrefix "" $
  makeClass (ident "QMdiSubWindow") Nothing [c_QWidget] $
  collect
  [ -- Ctors:
    just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithParentAndFlags" [ptrT $ objT c_QWidget, flagsT fl_WindowFlags]
    -- Properties:
  , just $ mkProp "keyboardPageStep" intT
  , just $ mkProp "keyboardSingleStep" intT
  , just $ mkProp "systemMenu" $ ptrT $ objT c_QMenu
  , just $ mkProp "widget" $ ptrT $ objT c_QWidget
    -- Methods:
  , just $ mkConstMethod "isShaded" np boolT
  , just $ mkConstMethod "mdiArea" np $ ptrT $ objT c_QMdiArea
  , just $ mkMethod "setOption" [enumT e_SubWindowOption, boolT] voidT
  , just $ mkConstMethod "testOption" [enumT e_SubWindowOption] boolT
    -- Slots:
  , just $ mkMethod "showShaded" np voidT
  , just $ mkMethod "showSystemMenu" np voidT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "aboutToActivate" listener
  , makeSignal "windowStateChanged" listenerWindowStatesWindowStates
  ]

(e_SubWindowOption, fl_SubWindowOptions) =
  makeQtEnumAndFlags (ident1 "QMdiSubWindow" "SubWindowOption") "SubWindowOptions"
  [includeStd "QMdiSubWindow"]
  [ "RubberBandResize"
  , "RubberBandMove"
  ]
