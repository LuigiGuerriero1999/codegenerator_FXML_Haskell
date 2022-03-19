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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSlider (
  aModule,
  c_QAbstractSlider,
  e_SliderAction,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  enumSetValuePrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  makeEnum,
  mkBoolIsProp,
  mkBoolHasProp,
  mkCtor,
  mkMethod,
  mkProp,
  np,
  toExtName,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_Orientation)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerInt,
  listenerIntInt,
  listenerQAbstractSliderAction,
  )
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractSlider"] $
  [ QtExportClassAndSignals c_QAbstractSlider signals
  , qtExport e_SliderAction
  ]

(c_QAbstractSlider, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QAbstractSlider"] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractSlider") Nothing [c_QWidget]
  [ mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkProp "invertedAppearance" boolT
  , mkProp "invertedControls" boolT
  , mkProp "maximum" intT
  , mkProp "minimum" intT
  , mkProp "orientation" $ enumT e_Orientation
  , mkProp "pageStep" intT
  , mkProp "singleStep" intT
  , mkBoolIsProp "sliderDown"
  , mkProp "sliderPosition" intT
  , mkBoolHasProp "tracking"
  , mkMethod "triggerAction" [enumT e_SliderAction] voidT
  , mkProp "value" intT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "actionTriggered" listenerQAbstractSliderAction
  , makeSignal "rangeChanged" listenerIntInt
  , makeSignal "sliderMoved" listenerInt
  , makeSignal "sliderPressed" listener
  , makeSignal "sliderReleased" listener
  , makeSignal "valueChanged" listenerInt
  ]

-- This uses 'makeEnum' rather than 'makeQtEnum' in order to use the external
-- name of "QAbstractSliderAction" instead of "QAbstractSliderSliderAction".
e_SliderAction =
  addReqIncludes [includeStd "QAbstractSlider"] $
  enumSetValuePrefix "" $
  makeEnum (ident1 "QAbstractSlider" "SliderAction") (Just $ toExtName "QAbstractSliderAction")
  [ (0, ["slider", "no", "action"])
  , (1, ["slider", "single", "step", "add"])
  , (2, ["slider", "single", "step", "sub"])
  , (3, ["slider", "page", "step", "add"])
  , (4, ["slider", "page", "step", "sub"])
  , (5, ["slider", "to", "minimum"])
  , (6, ["slider", "to", "maximum"])
  , (7, ["slider", "move"])
  ]
