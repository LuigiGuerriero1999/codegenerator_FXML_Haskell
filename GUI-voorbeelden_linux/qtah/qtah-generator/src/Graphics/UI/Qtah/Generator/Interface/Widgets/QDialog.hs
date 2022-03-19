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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QDialog (
  aModule,
  c_QDialog,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_WindowFlags)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listener, listenerInt)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QDialog"] $
  [ QtExportClassAndSignals c_QDialog signals
  , qtExport e_DialogCode
  ]

(c_QDialog, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QDialog"] $
  classSetEntityPrefix "" $
  makeClass (ident "QDialog") Nothing [c_QWidget]
  [ mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithParentAndFlags" [ptrT $ objT c_QWidget, flagsT fl_WindowFlags]
  , mkMethod "accept" np voidT
  , mkMethod "done" [intT] voidT
  , mkMethod "exec" np intT
  , mkBoolIsProp "modal"
  , mkMethod "open" np voidT
  , mkMethod "reject" np voidT
  , mkProp "result" intT
  , mkBoolIsProp "sizeGripEnabled"
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "accepted" listener
  , makeSignal "finished" listenerInt
  , makeSignal "rejected" listener
  ]

e_DialogCode =
  makeQtEnum (ident1 "QDialog" "DialogCode") [includeStd "QDialog"]
  [ "Rejected"
  , "Accepted"
  ]
