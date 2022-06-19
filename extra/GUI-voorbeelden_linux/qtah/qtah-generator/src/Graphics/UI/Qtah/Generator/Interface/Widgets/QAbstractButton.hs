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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton (
  aModule,
  c_QAbstractButton,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listener, listenerBool)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QButtonGroup (c_QButtonGroup)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractButton"]
  [ QtExportClassAndSignals c_QAbstractButton signals ]

(c_QAbstractButton, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QAbstractButton"] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractButton") Nothing [c_QWidget]
  -- Abstact.
  [ mkMethod "animateClick" [intT] voidT
  , mkProp "autoExclusive" boolT
  , mkProp "autoRepeat" boolT
  , mkProp "autoRepeatDelay" intT
  , mkProp "autoRepeatInterval" intT
  , mkBoolIsProp "checkable"
  , mkBoolIsProp "checked"
  , mkMethod "click" np voidT
  , mkBoolIsProp "down"
  , mkConstMethod "group" np $ ptrT $ objT c_QButtonGroup
  , mkProp "icon" $ objT c_QIcon
  , mkProp "iconSize" $ objT c_QSize
    -- TODO shortcut
  , mkProp "text" $ objT c_QString
  , mkMethod "toggle" np voidT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "clicked" listenerBool
  , makeSignal "pressed" listener
  , makeSignal "released" listener
  , makeSignal "toggled" listenerBool
  ]
