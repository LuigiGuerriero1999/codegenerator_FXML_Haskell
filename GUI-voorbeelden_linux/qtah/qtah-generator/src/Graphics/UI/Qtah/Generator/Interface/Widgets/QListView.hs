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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QListView (
  aModule,
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
  mkProp,
  mkConstMethod,
  mkMethod,
  mkMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, refT, voidT, enumT, constT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemView (c_QAbstractItemView, e_ScrollHint)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listenerRefConstQListQModelIndex)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_Alignment)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QListView"]
  [ QtExportClassAndSignals c_QListView signals
  , qtExport e_Flow
  , qtExport e_LayoutMode
  , qtExport e_Movement
  , qtExport e_ResizeMode
  , qtExport e_ViewMode
  ]

(c_QListView, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QListView"] $
  classSetEntityPrefix "" $
  makeClass (ident "QListView") Nothing [c_QAbstractItemView] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , test (qtVersion >= [4, 2]) $ mkProp "batchSize" intT
  , just $ mkMethod "clearPropertyFlags" np voidT
  , just $ mkProp "flow" $ enumT e_Flow
  , just $ mkProp "gridSize" $ objT c_QSize
  , just $ mkBoolIsProp "wrapping"
  , test (qtVersion >= [5, 12]) $ mkProp "itemAlignment" $ flagsT fl_Alignment
  , just $ mkProp "layoutMode" $ enumT e_LayoutMode
  , just $ mkProp "modelColumn" intT
  , just $ mkProp "movement" $ enumT e_Movement
  , just $ mkProp "resizeMode" $ enumT e_ResizeMode
  , test (qtVersion >= [4, 3]) $ mkBoolIsProp "selectionRectVisible"
  , just $ mkProp "spacing" intT
  , test (qtVersion >= [4, 1]) $ mkProp "uniformItemSizes" boolT
  , just $ mkProp "viewMode" $ enumT e_ViewMode
  , test (qtVersion >= [4, 2]) $ mkProp "wordWrap" boolT

  , just $ mkConstMethod "indexAt" [refT $ constT $ objT c_QPoint] $ objT c_QModelIndex
  , just $ mkConstMethod "isRowHidden" [intT] boolT
  , just $ mkMethod' "scrollTo" "scrollTo" [refT $ constT $ objT c_QModelIndex] voidT
  , just $ mkMethod' "scrollTo" "scrollToWithHint" [refT $ constT $ objT c_QModelIndex, enumT e_ScrollHint] voidT
  , just $ mkMethod "setRowHidden" [intT, boolT] voidT
  , just $ mkMethod "visualRect" [refT $ constT $ objT c_QModelIndex] $ objT c_QRect
  ]

signalGens :: [SignalGen]
signalGens =
  collect
  [ test (qtVersion >= [4, 2]) $ makeSignal "indexesMoved" listenerRefConstQListQModelIndex
  ]

e_Flow =
  makeQtEnum (ident1 "QListView" "Flow") [includeStd "QListView"]
  [ "LeftToRight"
  , "TopToBottom"
  ]

e_LayoutMode =
  makeQtEnum (ident1 "QListView" "LayoutMode") [includeStd "QListView"]
  [ "SinglePass"
  , "Batched"
  ]

e_Movement =
  makeQtEnum (ident1 "QListView" "Movement") [includeStd "QListView"]
  [ "Static"
  , "Free"
  , "Snap"
  ]

e_ResizeMode =
  makeQtEnum (ident1 "QListView" "ResizeMode") [includeStd "QListView"]
  [ "Fixed"
  , "Adjust"
  ]

e_ViewMode =
  makeQtEnum (ident1 "QListView" "ViewMode") [includeStd "QListView"]
  [ "ListMode"
  , "IconMode"
  ]
