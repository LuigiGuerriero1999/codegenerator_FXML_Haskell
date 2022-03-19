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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QDoubleSpinBox (
  aModule,
  c_QDoubleSpinBox,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (doubleT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listenerDouble,
  listenerQString,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSpinBox (c_QAbstractSpinBox)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QDoubleSpinBox"]
  [ QtExportClassAndSignals c_QDoubleSpinBox signals ]

(c_QDoubleSpinBox, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QDoubleSpinBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QDoubleSpinBox") Nothing [c_QAbstractSpinBox]
  [ mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkConstMethod "cleanText" np $ objT c_QString
  , mkProp "decimals" intT
  , mkProp "maximum" doubleT
  , mkProp "minimum" doubleT
  , mkProp "prefix" $ objT c_QString
  , mkMethod "setRange" [doubleT, doubleT] voidT
  , mkProp "singleStep" doubleT
  , mkProp "suffix" $ objT c_QString
  , mkConstMethod "textFromValue" [doubleT] $ objT c_QString
  , mkProp "value" doubleT
  , mkConstMethod "valueFromText" [objT c_QString] doubleT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal' "valueChanged" "valueChangedDouble" listenerDouble
  , makeSignal' "valueChanged" "valueChangedString" listenerQString
  ]
