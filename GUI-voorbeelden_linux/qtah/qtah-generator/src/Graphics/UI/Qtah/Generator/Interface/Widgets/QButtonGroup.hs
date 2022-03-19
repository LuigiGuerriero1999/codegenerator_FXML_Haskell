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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QButtonGroup (
  aModule,
  c_QButtonGroup,
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
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listenerInt,
  listenerIntBool,
  listenerPtrQAbstractButton,
  listenerPtrQAbstractButtonBool,
  )
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton (
  c_QAbstractButton,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QButtonGroup"]
  [ QtExportClassAndSignals c_QButtonGroup signals ]

(c_QButtonGroup, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QButtonGroup"] $
  classSetEntityPrefix "" $
  makeClass (ident "QButtonGroup") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkMethod' "addButton" "addButton" [ptrT $ objT c_QAbstractButton] voidT
  , just $ mkMethod' "addButton" "addButtonWithId" [ptrT $ objT c_QAbstractButton, intT] voidT
  , test (qtVersion >= [4, 1]) $ mkConstMethod "button" [intT] $ ptrT $ objT c_QAbstractButton
  , just $ mkConstMethod "buttons" np $ objT c_QListQAbstractButton
  , just $ mkConstMethod "checkedButton" np $ ptrT $ objT c_QAbstractButton
  , test (qtVersion >= [4, 1]) $ mkConstMethod "checkedId" np intT
  , just $ mkProp "exclusive" boolT
  , test (qtVersion >= [4, 1]) $ mkConstMethod "id" [ptrT $ objT c_QAbstractButton] intT
  , just $ mkMethod "removeButton" [ptrT $ objT c_QAbstractButton] voidT
  , test (qtVersion >= [4, 1]) $ mkMethod "setId" [ptrT $ objT c_QAbstractButton, intT] voidT
  ]

signalGens :: [SignalGen]
signalGens =
  collect
  [ just $ makeSignal' "buttonClicked" "buttonClicked" listenerPtrQAbstractButton
  , just $ makeSignal' "buttonClicked" "buttonClickedId" listenerInt
  , test (qtVersion >= [4, 2]) $
    makeSignal' "buttonPressed" "buttonPressed" listenerPtrQAbstractButton
  , test (qtVersion >= [4, 2]) $
    makeSignal' "buttonPressed" "buttonPressedId" listenerInt
  , test (qtVersion >= [4, 2]) $
    makeSignal' "buttonReleased" "buttonReleased" listenerPtrQAbstractButton
  , test (qtVersion >= [4, 2]) $
    makeSignal' "buttonReleased" "buttonReleasedId" listenerInt
  , test (qtVersion >= [5, 2]) $
    makeSignal' "buttonToggled" "buttonToggled" listenerPtrQAbstractButtonBool
  , test (qtVersion >= [5, 2]) $
    makeSignal' "buttonToggled" "buttonToggledId" listenerIntBool
  ]
