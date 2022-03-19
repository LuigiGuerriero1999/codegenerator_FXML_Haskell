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

module Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionModel (
  aModule,
  c_QItemSelectionModel,
  fl_SelectionFlags,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  constT,
  intT,
  objT,
  ptrT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (c_QAbstractItemModel)
import Graphics.UI.Qtah.Generator.Interface.Core.QItemSelection (c_QItemSelection)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listenerPtrQAbstractItemModel,
  listenerRefConstQItemSelectionRefConstQItemSelection,
  listenerQModelIndexQModelIndex,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QItemSelectionModel"] $
  [ QtExportClassAndSignals c_QItemSelectionModel signals
  , qtExport e_SelectionFlag
  , qtExport fl_SelectionFlags
  ]

(c_QItemSelectionModel, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QItemSelectionModel"] $
  classSetEntityPrefix "" $
  makeClass (ident "QItemSelectionModel") Nothing [c_QObject] $
  collect
  [ -- The nullary constructor requires at least Qt >=5.0.  Release notes[1]
    -- point at QItemSelectionModel gaining some methods in 5.5:
    --
    --     QAbstractItemModel* model()
    --     void modelChanged(QAbstractItemModel* model)
    --     void setModel(QAbstractItemModel* model)
    --
    -- So I suspect that this constructor appeared at that point.
    --
    -- [1] https://doc.qt.io/qt-5/newclasses55.html
    test (qtVersion >= [5, 5]) $ mkCtor "new" np
  , just $ mkCtor "newWithModel" [ptrT $ objT c_QAbstractItemModel]
  , just $ mkCtor "newWithModelAndParent" [ptrT $ objT c_QAbstractItemModel, ptrT $ objT c_QObject]
  , just $ mkMethod "clear" np voidT
  , test (qtVersion >= [5, 0]) $ mkMethod "clearCurrentIndex" np voidT
  , just $ mkMethod "clearSelection" np voidT
  , just $ mkConstMethod "columnIntersectsSelection" [intT, objT c_QModelIndex] boolT
  , just $ mkConstMethod "currentIndex" np $ objT c_QModelIndex
  , just $ mkConstMethod "hasSelection" np boolT
  , just $ mkConstMethod "isColumnSelected" [intT, objT c_QModelIndex] boolT
  , just $ mkConstMethod "isRowSelected" [intT, objT c_QModelIndex] boolT
  , just $ mkConstMethod "isSelected" [objT c_QModelIndex] boolT
  , test (qtVersion >= [5, 5]) $ mkMethod' "model" "model" np $ ptrT $ objT c_QAbstractItemModel
  , just $ mkConstMethod' "model" "modelConst" np $ ptrT $ constT $ objT c_QAbstractItemModel
  , just $ mkMethod "reset" np voidT
  , just $ mkConstMethod "rowIntersectsSelection" [intT, objT c_QModelIndex] boolT
  , just $ mkMethod' "select" "selectIndex" [objT c_QModelIndex, flagsT fl_SelectionFlags] voidT
  , just $ mkMethod' "select" "selectSelection" [objT c_QItemSelection, flagsT fl_SelectionFlags]
    voidT
  , just $ mkConstMethod "selectedColumns" [intT] $ objT c_QListQModelIndex
  , test (qtVersion >= [5, 5]) $ mkConstMethod "selectedIndexes" np $ objT c_QListQModelIndex
  , just $ mkConstMethod "selectedRows" [intT] $ objT c_QListQModelIndex
  , just $ mkConstMethod "selection" np $ objT c_QItemSelection
  , just $ mkMethod "setCurrentIndex" [objT c_QModelIndex, flagsT fl_SelectionFlags] voidT
  , test (qtVersion >= [5, 5]) $ mkMethod "setModel" [ptrT $ objT c_QAbstractItemModel] voidT
  ]

signalGens :: [SignalGen]
signalGens =
  collect
  [ just $ makeSignal "currentChanged" listenerQModelIndexQModelIndex
  , just $ makeSignal "currentColumnChanged" listenerQModelIndexQModelIndex
  , just $ makeSignal "currentRowChanged" listenerQModelIndexQModelIndex
  , test (qtVersion >= [5, 5]) $ makeSignal "modelChanged" listenerPtrQAbstractItemModel
  , just $ makeSignal "selectionChanged" listenerRefConstQItemSelectionRefConstQItemSelection
  ]

(e_SelectionFlag, fl_SelectionFlags) =
  makeQtEnumAndFlagsWithOverrides (ident1 "QItemSelectionModel" "SelectionFlag") "SelectionFlags"
  [includeStd "QItemSelectionModel"]
  [ "NoUpdate"
  , "Clear"
  , "Select"
  , "Deselect"
  , "Toggle"
  , "Current"
  , "Rows"
  , "Columns"
  , "SelectCurrent"
  , "ToggleCurrent"
  , "ClearAndSelect"
  ]
  [ ("Clear", "ClearFlag")
  ]
