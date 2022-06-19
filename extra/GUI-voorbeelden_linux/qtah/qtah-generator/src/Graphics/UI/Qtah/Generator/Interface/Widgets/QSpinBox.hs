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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QSpinBox (
  aModule,
  c_QSpinBox,
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
import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listenerInt,
  listenerQString,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSpinBox (c_QAbstractSpinBox)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QSpinBox"]
  [ QtExportClassAndSignals c_QSpinBox signals ]

(c_QSpinBox, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QSpinBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QSpinBox") Nothing [c_QAbstractSpinBox] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkConstMethod "cleanText" np $ objT c_QString
  , test (qtVersion >= [5, 2]) $ mkProp "displayIntegerBase" intT
  , just $ mkProp "maximum" intT
  , just $ mkProp "minimum" intT
  , just $ mkProp "prefix" $ objT c_QString
  , just $ mkMethod "setRange" [intT, intT] voidT
  , just $ mkProp "singleStep" intT
  , just $ mkProp "suffix" $ objT c_QString
  , just $ mkProp "value" intT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal' "valueChanged" "valueChangedInt" listenerInt
  , makeSignal' "valueChanged" "valueChangedString" listenerQString
  ]
