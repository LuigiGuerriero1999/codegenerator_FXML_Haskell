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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QLCDNumber (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  (~:),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkProp,
  mkConstMethod,
  mkConstMethod',
  mkMethod,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, doubleT, enumT, intT, objT, ptrT, uintT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QFrame (c_QFrame)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listener)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QLCDNumber"]
  [ QtExportClassAndSignals c_QLCDNumber signals
  , qtExport e_Mode
  , qtExport e_SegmentStyle
  ]

(c_QLCDNumber, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QLCDNumber"] $
  classSetEntityPrefix "" $
  makeClass (ident "QLCDNumber") Nothing [c_QFrame] $
  collect
  [ -- Ctors:
    just $ mkCtor "new" np
  , just $ mkCtor "newWithDigits" ["numDigits" ~: uintT]
  , just $ mkCtor "newWithDigitsAndParent" ["numDigits" ~: uintT, "parent" ~: ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithParent" ["parent" ~: ptrT $ objT c_QWidget]
    -- Properties:
  , just $ mkProp "digitCount" intT
  , just $ mkProp "mode" $ enumT e_Mode
  , just $ mkProp "segmentStyle" $ enumT e_SegmentStyle
  , just $ mkProp "smallDecimalPoint" boolT
    -- Methods:
  , just $ mkConstMethod' "checkOverflow" "checkOverflowInt" ["num" ~: intT] boolT
  , just $ mkConstMethod' "checkOverflow" "checkOverflowDouble" ["num" ~: doubleT] boolT
  , just $ mkConstMethod "intValue" np intT
  , just $ mkConstMethod "value" np doubleT
    -- Slots:
  , just $ mkMethod' "display" "displayDouble" [doubleT] voidT
  , just $ mkMethod' "display" "displayInt" [intT] voidT
  , just $ mkMethod' "display" "displayString" [objT c_QString] voidT
  , just $ mkMethod "setBinMode" np voidT
  , just $ mkMethod "setDecMode" np voidT
  , just $ mkMethod "setHexMode" np voidT
  , just $ mkMethod "setOctMode" np voidT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "overflow" listener
  ]

e_Mode =
  makeQtEnum (ident1 "QLCDNumber" "Mode") [includeStd "QLCDNumber"]
  [ "Hex"
  , "Dec"
  , "Bin"
  , "Oct"
  ]

e_SegmentStyle =
  makeQtEnum (ident1 "QLCDNumber" "SegmentStyle") [includeStd "QLCDNumber"]
  [ "Outline"
  , "Filled"
  , "Flat"
  ]
