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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeView (
  aModule,
  c_QTreeView,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Class,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkMethod,
  mkMethod',
  mkConstMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, voidT, constT, refT, enumT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorInt)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemView (c_QAbstractItemView)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_SortOrder)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listenerRefConstQModelIndex,
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTreeView"]
  [ QtExportClassAndSignals c_QTreeView signals ]

c_QTreeView :: Class
(c_QTreeView, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QTreeView"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTreeView") Nothing [c_QAbstractItemView] $
  collect
  [
  -- Properties
    test (qtVersion >= [4, 2]) $ mkProp "allColumnsShowFocus" boolT
  , test (qtVersion >= [4, 2]) $ mkBoolIsProp "animated"
  , test (qtVersion >= [4, 3]) $ mkProp "autoExpandDelay" intT
  , test (qtVersion >= [4, 4]) $ mkProp "expandsOnDoubleClick" boolT
  , test (qtVersion >= [4, 4]) $ mkBoolIsProp "headerHidden"
  , just $ mkProp "indentation" intT
  , just $ mkProp "itemsExpandable" boolT
  , just $ mkProp "rootIsDecorated" boolT
  , test (qtVersion >= [4, 2]) $ mkBoolIsProp "sortingEnabled"
  , just $ mkProp "uniformRowHeights" boolT
  , test (qtVersion >= [4, 3]) $ mkProp "wordWrap" boolT
  -- Public Functions
  , just $ mkCtor "new" np
  , just $ mkConstMethod "columnAt" [intT] intT
  , just $ mkConstMethod "columnViewportPosition" [intT] intT
  , just $ mkConstMethod "columnWidth" [intT] intT
  , just $ mkMethod' "dataChanged" "dataChanged" [refT $ constT $ objT c_QModelIndex, refT $ constT $ objT c_QModelIndex] voidT
  , just $ mkMethod' "dataChanged" "dataChangedWithRoles" [refT $ constT $ objT c_QModelIndex, refT $ constT $ objT c_QModelIndex, refT $ constT $ objT c_QVectorInt] voidT
  -- QHeaderView *QTreeView::header() const
  , just $ mkConstMethod "indexAbove" [refT $ constT $ objT c_QModelIndex] $ objT c_QModelIndex
  , just $ mkConstMethod "indexBelow" [refT $ constT $ objT c_QModelIndex] $ objT c_QModelIndex
  , just $ mkConstMethod "isColumnHidden" [intT] boolT
  , just $ mkConstMethod "isExpanded" [refT $ constT $ objT c_QModelIndex] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod "isFirstColumnSpanned" [intT, refT $ constT $ objT c_QModelIndex] boolT
  , just $ mkConstMethod "isRowHidden" [intT, refT $ constT $ objT c_QModelIndex] boolT
  , just $ mkMethod "setColumnHidden" [intT, boolT] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setColumnWidth" [intT, intT] voidT
  , just $ mkMethod "setExpanded" [refT $ constT $ objT c_QModelIndex, boolT] voidT
  , test (qtVersion >= [4, 3]) $ mkMethod "setFirstColumnSpanned" [intT, refT $ constT $ objT c_QModelIndex, boolT] voidT
  -- TODO void QTreeView::setHeader(QHeaderView *header)
  , just $ mkMethod "setRowHidden" [intT, refT $ constT $ objT c_QModelIndex, boolT] voidT
  , test (qtVersion >= [5, 2]) $ mkMethod "setTreePosition" [intT] voidT
  , just $ mkMethod "selectAll" np voidT
  -- Public Slots
  , just $ mkMethod "collapse" [objT c_QModelIndex] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "collapseAll" np voidT
  , just $ mkMethod "expand" [objT c_QModelIndex] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "expandAll" np voidT
  , test (qtVersion >= [5, 13]) $ mkMethod' "expandRecursively" "expandRecursively" [refT $ constT $ objT c_QModelIndex] voidT
  , test (qtVersion >= [5, 13]) $ mkMethod' "expandRecursively" "expandRecursivelyWithDepth" [refT $ constT $ objT c_QModelIndex, intT] voidT
  , test (qtVersion >= [4, 3]) $ mkMethod "expandToDepth" [intT] voidT
  , just $ mkMethod "hideColumn" [intT] voidT
  , just $ mkMethod "resizeColumnToContents" [intT] voidT
  , just $ mkMethod "showColumn" [intT] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "sortByColumn" [intT, enumT e_SortOrder] voidT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "collapsed" listenerRefConstQModelIndex
  , makeSignal "expanded" listenerRefConstQModelIndex
  ]
