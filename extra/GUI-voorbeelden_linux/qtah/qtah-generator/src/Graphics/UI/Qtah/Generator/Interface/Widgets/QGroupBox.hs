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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGroupBox (
  aModule,
  c_QGroupBox,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_Alignment)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listenerBool,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGroupBox"]
  [ QtExportClassAndSignals c_QGroupBox signals ]

(c_QGroupBox, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QGroupBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGroupBox") Nothing [c_QWidget]
  [ mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithTitle" [objT c_QString]
  , mkCtor "newWithTitleAndParent" [objT c_QString, ptrT $ objT c_QWidget]
  , mkProp "alignment" $ flagsT fl_Alignment
  , mkBoolIsProp "checkable"
  , mkBoolIsProp "checked"
  , mkBoolIsProp "flat"
  , mkProp "title" $ objT c_QString
  ]

signalGens :: [SignalGen]
signalGens =
  collect
  [ test (qtVersion >= [4, 2]) $ makeSignal "clicked" listenerBool
  , just $ makeSignal "toggled" listenerBool
  ]
