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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QFormLayout (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  MethodApplicability (MConst),
  Purity (Nonpure),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (constT, enumT, intT, objT, ptrT, refT, voidT)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_Alignment)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QLayoutItem (c_QLayoutItem)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [4, 4]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Widgets", "QFormLayout"] minVersion
  [ qtExport c_QFormLayout
  , qtExport e_FieldGrowthPolicy
  , qtExport e_ItemRole
  , qtExport e_RowWrapPolicy
  ]

c_QFormLayout =
  addReqIncludes [includeStd "QFormLayout",
                  includeLocal "wrap_qformlayout.hpp"] $
  classSetEntityPrefix "" $
  makeClass (ident "QFormLayout") Nothing [c_QLayout]
  [ mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkMethod' "addRow" "addRowWidgetWidget" [ptrT $ objT c_QWidget, ptrT $ objT c_QWidget] voidT
  , mkMethod' "addRow" "addRowWidgetLayout" [ptrT $ objT c_QWidget, ptrT $ objT c_QLayout] voidT
  , mkMethod' "addRow" "addRowStringWidget" [objT c_QString, ptrT $ objT c_QWidget] voidT
  , mkMethod' "addRow" "addRowStringLayout" [objT c_QString, ptrT $ objT c_QLayout] voidT
  , mkMethod' "addRow" "addRowWidget" [ptrT $ objT c_QWidget] voidT
  , mkMethod' "addRow" "addRowLayout" [ptrT $ objT c_QLayout] voidT
  , mkProp "fieldGrowthPolicy" $ enumT e_FieldGrowthPolicy
  , mkProp "formAlignment" $ flagsT fl_Alignment
  , makeFnMethod (ident2 "qtah" "qformlayout" "getItemRow") "getItemRow" MConst Nonpure
    [refT $ constT $ objT c_QFormLayout, intT] intT
  , makeFnMethod (ident2 "qtah" "qformlayout" "getItemRole") "getItemRole" MConst Nonpure
    [refT $ constT $ objT c_QFormLayout, intT] $ enumT e_ItemRole
  , makeFnMethod (ident2 "qtah" "qformlayout" "getLayoutRow") "getLayoutRow" MConst Nonpure
    [refT $ constT $ objT c_QFormLayout, ptrT $ objT c_QLayout] intT
  , makeFnMethod (ident2 "qtah" "qformlayout" "getLayoutRole") "getLayoutRole" MConst Nonpure
    [refT $ constT $ objT c_QFormLayout, ptrT $ objT c_QLayout] $ enumT e_ItemRole
  , makeFnMethod (ident2 "qtah" "qformlayout" "getWidgetRow") "getWidgetRow" MConst Nonpure
    [refT $ constT $ objT c_QFormLayout, ptrT $ objT c_QWidget] intT
  , makeFnMethod (ident2 "qtah" "qformlayout" "getWidgetRole") "getWidgetRole" MConst Nonpure
    [refT $ constT $ objT c_QFormLayout, ptrT $ objT c_QWidget] $ enumT e_ItemRole
  , mkProp "horizontalSpacing" intT
  , mkMethod' "insertRow" "insertRowWidgetWidget"
    [intT, ptrT $ objT c_QWidget, ptrT $ objT c_QWidget] voidT
  , mkMethod' "insertRow" "insertRowWidgetLayout"
    [intT, ptrT $ objT c_QWidget, ptrT $ objT c_QLayout] voidT
  , mkMethod' "insertRow" "insertRowStringWidget"
    [intT, objT c_QString, ptrT $ objT c_QWidget] voidT
  , mkMethod' "insertRow" "insertRowStringLayout"
    [intT, objT c_QString, ptrT $ objT c_QLayout] voidT
  , mkMethod' "insertRow" "insertRowWidget" [intT, ptrT $ objT c_QWidget] voidT
  , mkMethod' "insertRow" "insertRowLayout" [intT, ptrT $ objT c_QLayout] voidT
  , mkConstMethod "itemAt" [intT, enumT e_ItemRole] $ ptrT $ objT c_QLayoutItem
  , mkProp "labelAlignment" $ flagsT fl_Alignment
  , mkConstMethod' "labelForField" "labelForFieldWidget"
    [ptrT $ objT c_QWidget] $ ptrT $ objT c_QWidget
  , mkConstMethod' "labelForField" "labelForFieldLayout"
    [ptrT $ objT c_QLayout] $ ptrT $ objT c_QWidget
  , mkConstMethod "rowCount" np intT
  , mkProp "rowWrapPolicy" $ enumT e_RowWrapPolicy
  , mkMethod "setItem" [intT, enumT e_ItemRole, ptrT $ objT c_QLayoutItem] voidT
  , mkMethod "setLayout" [intT, enumT e_ItemRole, ptrT $ objT c_QLayout] voidT
  , mkMethod "setWidget" [intT, enumT e_ItemRole, ptrT $ objT c_QWidget] voidT
  , mkProp "spacing" intT
  , mkProp "verticalSpacing" intT
  ]

e_FieldGrowthPolicy =
  makeQtEnum (ident1 "QFormLayout" "FieldGrowthPolicy") [includeStd "QFormLayout"]
  [ "FieldsStayAtSizeHint"
  , "ExpandingFieldsGrow"
  , "AllNonFixedFieldsGrow"
  ]

e_ItemRole =
  makeQtEnum (ident1 "QFormLayout" "ItemRole") [includeStd "QFormLayout"]
  [ "LabelRole"
  , "FieldRole"
  , "SpanningRole"
  ]

e_RowWrapPolicy =
  makeQtEnum (ident1 "QFormLayout" "RowWrapPolicy") [includeStd "QFormLayout"]
  [ "DontWrapRows"
  , "WrapLongRows"
  , "WrapAllRows"
  ]
