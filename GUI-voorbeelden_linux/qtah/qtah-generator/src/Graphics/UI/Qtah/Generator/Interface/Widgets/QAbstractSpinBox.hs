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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSpinBox (
  aModule,
  c_QAbstractSpinBox,
  e_ButtonSymbols,
  e_CorrectionMode,
  e_StepEnabledFlag,
  fl_StepEnabled,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkBoolHasProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_Alignment)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listener)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractSpinBox"] $
  collect
  [ just $ QtExportClassAndSignals c_QAbstractSpinBox signals
  , test (qtVersion >= [4, 2]) $ qtExport e_ButtonSymbols
  , just $ qtExport e_CorrectionMode
  , just $ qtExport e_StepEnabledFlag
  , just $ qtExport fl_StepEnabled
  ]

(c_QAbstractSpinBox, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QAbstractSpinBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractSpinBox") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , test (qtVersion >= [4, 2]) $ mkBoolIsProp "accelerated"
  , just $ mkProp "alignment" $ flagsT fl_Alignment
  , test (qtVersion >= [4, 2]) $ mkProp "buttonSymbols" $ enumT e_ButtonSymbols
  , just $ mkMethod "clear" np voidT
  , just $ mkProp "correctionMode" $ enumT e_CorrectionMode
  , test (qtVersion >= [4, 2]) $ mkConstMethod "hasAcceptableInput" np boolT
  , just $ mkConstMethod "fixup" [refT $ objT c_QString] voidT
  , test (qtVersion >= [4, 3]) $ mkBoolHasProp "frame"
  , test (qtVersion >= [5, 3]) $ mkBoolIsProp "groupSeparatorShown"
  , just $ mkMethod "interpretText" np voidT
  , just $ mkProp "keyboardTracking" boolT
  , just $ mkBoolIsProp "readOnly"
  , just $ mkMethod "selectAll" np voidT
  , just $ mkProp "specialValueText" $ objT c_QString
  , just $ mkMethod "stepBy" [intT] voidT
  , just $ mkMethod "stepDown" np voidT
  , just $ mkMethod "stepUp" np voidT
  , just $ mkConstMethod "text" np $ objT c_QString
    -- TODO validate
  , just $ mkProp "wrapping" boolT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "editingFinished" listener
  ]

e_ButtonSymbols =
  addReqIncludes [includeStd "QAbstractSpinBox"] $
  makeQtEnum (ident1 "QAbstractSpinBox" "ButtonSymbols") [includeStd "QAbstractSpinBox"]
  [ "UpDownArrows"
  , "PlusMinus"
  , "NoButtons"
  ]

e_CorrectionMode =
  makeQtEnum (ident1 "QAbstractSpinBox" "CorrectionMode") [includeStd "QAbstractSpinBox"]
  [ "CorrectToPreviousValue"
  , "CorrectToNearestValue"
  ]

(e_StepEnabledFlag, fl_StepEnabled) =
  makeQtEnumAndFlags (ident1 "QAbstractSpinBox" "StepEnabledFlag") "StepEnabled"
  [includeStd "QAbstractSpinBox"]
  [ "StepNone"
  , "StepUpEnabled"
  , "StepDownEnabled"
  ]
