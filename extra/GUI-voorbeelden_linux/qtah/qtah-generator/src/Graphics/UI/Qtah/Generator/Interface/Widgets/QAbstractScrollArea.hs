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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractScrollArea (
  aModule,
  c_QAbstractScrollArea,
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
  np,
  )
import Foreign.Hoppy.Generator.Types (enumT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_Alignment, e_ScrollBarPolicy)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QFrame (c_QFrame)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QScrollBar (c_QScrollBar)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractScrollArea"]
  [ qtExport c_QAbstractScrollArea ]

c_QAbstractScrollArea =
  addReqIncludes [includeStd "QAbstractScrollArea"] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractScrollArea") Nothing [c_QFrame]
  [ mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkMethod "addScrollBarWidget" [ptrT $ objT c_QWidget, flagsT fl_Alignment] voidT
  , mkConstMethod "cornerWidget" np $ ptrT $ objT c_QWidget
  , mkConstMethod "horizontalScrollBar" np $ ptrT $ objT c_QScrollBar
  , mkConstMethod "horizontalScrollBarPolicy" np $ enumT e_ScrollBarPolicy
  , mkConstMethod "maximumViewportSize" np $ objT c_QSize
  -- TODO mkMethod "scrollBarWidgets" [flagsT fl_Alignment] $ objT c_QWidgetList
  , mkMethod "setCornerWidget" [ptrT $ objT c_QWidget] voidT
  , mkMethod "setHorizontalScrollBar" [ptrT $ objT c_QScrollBar] voidT
  , mkMethod "setHorizontalScrollBarPolicy" [enumT e_ScrollBarPolicy] voidT
  , mkMethod "setVerticalScrollBar" [ptrT $ objT c_QScrollBar] voidT
  , mkMethod "setVerticalScrollBarPolicy" [enumT e_ScrollBarPolicy] voidT
  , mkMethod "setViewport" [ptrT $ objT c_QWidget] voidT
  , mkConstMethod "verticalScrollBar" np $ ptrT $ objT c_QScrollBar
  , mkConstMethod "verticalScrollBarPolicy" np $ enumT e_ScrollBarPolicy
  , mkConstMethod "viewport" np $ ptrT $ objT c_QWidget
  ]
