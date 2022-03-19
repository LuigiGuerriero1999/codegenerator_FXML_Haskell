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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QComboBox (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listenerInt,
  listenerQString,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QComboBox"]
  [ QtExportClassAndSignals c_QComboBox signals ]

(c_QComboBox, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QComboBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QComboBox") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkMethod "addItem" [objT c_QString] voidT
  , just $ mkMethod' "addItem" "addItemWithData" [objT c_QString, objT c_QVariant] voidT
  , just $ mkMethod' "addItem" "addItemWithIcon" [objT c_QIcon, objT c_QString] voidT
  , just $ mkMethod' "addItem" "addItemWithIconAndData"
    [objT c_QIcon, objT c_QString, objT c_QVariant] voidT
  , just $ mkProp "currentIndex" intT
  , just $ mkProp "currentText" $ objT c_QString
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "activated" listenerInt
  , makeSignal' "activated" "activatedString" listenerQString
  , makeSignal "currentIndexChanged" listenerInt
  , makeSignal' "currentIndexChanged" "currentIndexChangedString" listenerQString
  ]

-- TODO The rest of QComboBox.
