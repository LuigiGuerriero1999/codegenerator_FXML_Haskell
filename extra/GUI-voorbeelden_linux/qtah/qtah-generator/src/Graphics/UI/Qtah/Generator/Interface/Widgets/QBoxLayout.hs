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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QBoxLayout (
  aModule,
  c_QBoxLayout,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_Alignment)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QBoxLayout"]
  [ qtExport c_QBoxLayout
  , qtExport e_Direction
  ]

c_QBoxLayout =
  addReqIncludes [includeStd "QBoxLayout"] $
  classSetEntityPrefix "" $
  makeClass (ident "QBoxLayout") Nothing [c_QLayout]
  [ mkCtor "new" [enumT e_Direction]
  , mkCtor "newWithParent" [enumT e_Direction, ptrT $ objT c_QWidget]
  , mkMethod' "addLayout" "addLayout" [ptrT $ objT c_QLayout] voidT
  , mkMethod' "addLayout" "addLayoutWithStretch" [ptrT $ objT c_QLayout, intT] voidT
  , mkMethod "addSpacing" [intT] voidT
  , mkMethod' "addStretch" "addStretch" np voidT
  , mkMethod' "addStretch" "addStretchOf" [intT] voidT
  , mkMethod "addStrut" [intT] voidT
  , mkMethod' "addWidget" "addWidget" [ptrT $ objT c_QWidget] voidT
  , mkMethod' "addWidget" "addWidgetWithStretch" [ptrT $ objT c_QWidget, intT] voidT
  , mkMethod' "addWidget" "addWidgetWithStretchAndAlignment"
    [ptrT $ objT c_QWidget, intT, flagsT fl_Alignment] voidT
  , mkProp "direction" $ enumT e_Direction
  , mkMethod' "insertLayout" "insertLayout" [intT, ptrT $ objT c_QLayout] voidT
  , mkMethod' "insertLayout" "insertLayoutWithStretch" [intT, ptrT $ objT c_QLayout, intT] voidT
    -- TODO insertSpacerItem
  , mkMethod "insertSpacing" [intT, intT] voidT
  , mkMethod' "insertStretch" "insertStretch" [intT] voidT
  , mkMethod' "insertStretch" "insertStretchOf" [intT, intT] voidT
  , mkMethod' "insertWidget" "insertWidget" [intT, ptrT $ objT c_QWidget] voidT
  , mkMethod' "insertWidget" "insertWidgetWithStretch" [intT, ptrT $ objT c_QWidget, intT] voidT
  , mkMethod' "insertWidget" "insertWidgetWithStretchAndAlignment"
    [intT, ptrT $ objT c_QWidget, intT, flagsT fl_Alignment] voidT
  , mkMethod "setStretch" [intT, intT] voidT
  , mkMethod' "setStretchFactor" "setWidgetStretchFactor" [ptrT $ objT c_QWidget, intT] boolT
  , mkMethod' "setStretchFactor" "setLayoutStretchFactor" [ptrT $ objT c_QLayout, intT] boolT
  , mkProp "spacing" intT
  ]

e_Direction =
  makeQtEnum (ident1 "QBoxLayout" "Direction") [includeStd "QBoxLayout"]
  [ "LeftToRight"
  , "RightToLeft"
  , "TopToBottom"
  , "BottomToTop"
  ]
