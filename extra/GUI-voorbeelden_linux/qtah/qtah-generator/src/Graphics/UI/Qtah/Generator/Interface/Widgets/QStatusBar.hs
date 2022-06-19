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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QStatusBar (
  aModule,
  c_QStatusBar,
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
  np,
  )
import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listenerQString)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QStatusBar"]
  [ QtExportClassAndSignals c_QStatusBar signals ]

(c_QStatusBar, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QStatusBar"] $
  classSetEntityPrefix "" $
  makeClass (ident "QStatusBar") Nothing [c_QWidget]
  [ mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkMethod' "addPermanentWidget" "addPermanentWidget" [ptrT $ objT c_QWidget] voidT
  , mkMethod' "addPermanentWidget" "addPermanentWidgetWithStretch"
    [ptrT $ objT c_QWidget, intT] voidT
  , mkMethod' "addWidget" "addWidget" [ptrT $ objT c_QWidget] voidT
  , mkMethod' "addWidget" "addWidgetWithStretch" [ptrT $ objT c_QWidget, intT] voidT
  , mkMethod "clearMessage" np voidT
  , mkConstMethod "currentMessage" np $ objT c_QString
  , mkMethod' "insertPermanentWidget" "insertPermanentWidget" [intT, ptrT $ objT c_QWidget] voidT
  , mkMethod' "insertPermanentWidget" "insertPermanentWidgetWithStretch"
    [intT, ptrT $ objT c_QWidget, intT] voidT
  , mkMethod' "insertWidget" "insertWidget" [intT, ptrT $ objT c_QWidget] voidT
  , mkMethod' "insertWidget" "insertWidgetWithStretch" [intT, ptrT $ objT c_QWidget, intT] voidT
  , mkMethod "removeWidget" [ptrT $ objT c_QWidget] voidT
  , mkMethod' "showMessage" "showMessage" [objT c_QString] voidT
  , mkMethod' "showMessage" "showMessageWithTimeout" [objT c_QString, intT] voidT
  , mkBoolIsProp "sizeGripEnabled"
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "messageChanged" listenerQString
  ]
