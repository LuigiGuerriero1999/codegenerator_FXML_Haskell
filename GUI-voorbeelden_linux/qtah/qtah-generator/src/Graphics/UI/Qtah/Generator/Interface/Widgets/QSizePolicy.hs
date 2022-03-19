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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QSizePolicy (
  aModule,
  c_QSizePolicy,
  e_Policy,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolHasProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_Orientations)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QSizePolicy"] $
  collect
  [ just $ qtExport c_QSizePolicy
  , test (qtVersion >= [4, 3]) $ qtExport e_ControlType
  , test (qtVersion >= [4, 3]) $ qtExport fl_ControlTypes
  , just $ qtExport e_Policy
  , just $ qtExport e_PolicyFlag
  ]

c_QSizePolicy =
  addReqIncludes [includeStd "QSizePolicy"] $
  classSetConversionToGc $
  classAddFeatures [Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QSizePolicy") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , test (qtVersion >= [4, 3]) $ mkCtor "newWithOptions"
    [enumT e_Policy, enumT e_Policy, enumT e_ControlType]
  , test (qtVersion >= [4, 3]) $ mkProp "controlType" $ enumT e_ControlType
  , just $ mkConstMethod "expandingDirections" np $ flagsT fl_Orientations
  , just $ mkBoolHasProp "heightForWidth"
  , just $ mkBoolHasProp "widthForHeight"
  , just $ mkProp "horizontalPolicy" $ enumT e_Policy
  , just $ mkProp "horizontalStretch" intT
  , just $ mkProp "verticalPolicy" $ enumT e_Policy
  , just $ mkProp "verticalStretch" intT
  , test (qtVersion >= [5, 2]) $ mkProp "retainSizeWhenHidden" boolT
  , just $ mkMethod "transpose" np voidT
  ]

(e_ControlType, fl_ControlTypes) =
  makeQtEnumAndFlags (ident1 "QSizePolicy" "ControlType") "ControlTypes"
  [includeStd "QSizePolicy"]
  [ "DefaultType"
  , "ButtonBox"
  , "CheckBox"
  , "ComboBox"
  , "Frame"
  , "GroupBox"
  , "Label"
  , "Line"
  , "LineEdit"
  , "PushButton"
  , "RadioButton"
  , "Slider"
  , "SpinBox"
  , "TabWidget"
  , "ToolButton"
  ]

e_Policy =
  makeQtEnum (ident1 "QSizePolicy" "Policy") [includeStd "QSizePolicy"]
  [ "Fixed"
  , "Minimum"  -- GrowFlag
  , "Maximum"  -- ShrinkFlag
  , "Preferred"  -- GrowFlag | ShrinkFlag
  , "Expanding"  -- GrowFlag | ShrinkFlag | ExpandFlag
  , "MinimumExpanding"  -- GrowFlag | ExpandFlag
  , "Ignored"  -- ShrinkFlag | GrowFlag | IgnoreFlag
  ]

e_PolicyFlag =
  makeQtEnum (ident1 "QSizePolicy" "PolicyFlag") [includeStd "QSizePolicy"]
  [ "GrowFlag"
  , "ExpandFlag"
  , "ShrinkFlag"
  , "IgnoreFlag"
  ]
