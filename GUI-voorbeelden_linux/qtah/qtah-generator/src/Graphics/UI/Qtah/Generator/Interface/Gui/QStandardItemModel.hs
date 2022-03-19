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

module Graphics.UI.Qtah.Generator.Interface.Gui.QStandardItemModel (
  aModule,
  itemModule,
  itemListModule,
  c_QStandardItemModel,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Class,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (
  constT, boolT, enumT, intT, objT, ptrT, voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (
  c_QAbstractItemModel,
  )
-- import Graphics.UI.Qtah.Generator.Interface.Core.QDataStream (c_QDataStream)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (
  Contents, c_QList, instantiate,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  fl_Alignment,
  e_CheckState,
  fl_ItemFlags,
  fl_MatchFlags,
  e_SortOrder,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QBrush (c_QBrush)
import Graphics.UI.Qtah.Generator.Interface.Gui.QFont (c_QFont)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Module (
  AModule (AQtModule), makeQtModule, makeQtModuleWithMinVersion,
  )
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Gui", "QStandardItemModel"]
  [qtExport c_QStandardItemModel]

itemModule :: AModule
itemModule =
  AQtModule $
  makeQtModuleWithMinVersion
    ["Gui", "QStandardItem"]
    [4, 2]
    [qtExport c_QStandardItem]

itemListModule :: AModule
itemListModule =
  AQtModule $
  makeQtModuleWithMinVersion
    ["Core", "QList", "QStandardItem"]
    [4, 2]
    [qtExport c_QListQStandardItem]

c_QStandardItemModel :: Class
c_QStandardItemModel =
  addReqIncludes [includeStd "QStandardItemModel"] $
  classSetEntityPrefix "" $
  makeClass (ident "QStandardItemModel") Nothing [c_QAbstractItemModel] $
  collect
  [
  -- Properties
    test (qtVersion >= [4, 2]) $ mkProp "sortRole" intT
  -- Public Functions
  , just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkCtor "newWithRowsAndColumns" [intT, intT]
  , just $
    mkCtor "newWithRowsAndColumnsAndParent" [intT, intT, ptrT $ objT c_QObject]
  , test (qtVersion >= [4, 2]) $
    mkMethod "appendColumn" [objT c_QListQStandardItem] voidT
  , test (qtVersion >= [4, 2]) $
    mkMethod' "appendRow" "appendRowItems" [objT c_QListQStandardItem] voidT
  , test (qtVersion >= [4, 2]) $
    mkMethod' "appendRow" "appendRowItem" [ptrT $ objT c_QStandardItem] voidT
  , just $ mkMethod "clear" np voidT
  , test (qtVersion >= [4, 2]) $
    mkConstMethod "findItems" [objT c_QString] (objT c_QListQStandardItem)
  , test (qtVersion >= [4, 2]) $ mkConstMethod'
      "findItems"
      "findItemsWithFlags"
      [objT c_QString, flagsT fl_MatchFlags]
      (objT c_QListQStandardItem)
  , test (qtVersion >= [4, 2]) $ mkConstMethod'
      "findItems"
      "findItemsWithFlagsAndColumn"
      [objT c_QString, flagsT fl_MatchFlags, intT]
      (objT c_QListQStandardItem)
  , test (qtVersion >= [4, 2]) $
    mkConstMethod "horizontalHeaderItem" [intT] (ptrT $ objT c_QStandardItem)
  , test (qtVersion >= [4, 2]) $ mkConstMethod
      "indexFromItem"
      [ptrT . constT $ objT c_QStandardItem]
      (objT c_QModelIndex)
  , test (qtVersion >= [4, 2]) $ mkMethod'
      "insertColumn"
      "insertColumnWithItems"
      [intT, objT c_QListQStandardItem]
      voidT
  , just $ mkMethod "insertColumn" [intT] boolT
  , just $ mkMethod'
      "insertColumn" "insertColumnWithParent" [intT, objT c_QModelIndex] boolT
  , test (qtVersion >= [4, 2]) $ mkMethod'
      "insertRow" "insertRowWithItems" [intT, objT c_QListQStandardItem] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod'
      "insertRow" "insertRowWithItem" [intT, ptrT $ objT c_QStandardItem] voidT
  , just $ mkMethod "insertRow" [intT] boolT
  , just $
    mkMethod' "insertRow" "insertRowWithParent" [intT, objT c_QModelIndex] boolT
  , test (qtVersion >= [4, 2]) $
    mkConstMethod "invisibleRootItem" np (ptrT $ objT c_QStandardItem)
  , test (qtVersion >= [4, 2]) $
    mkConstMethod "item" [intT] (ptrT $ objT c_QStandardItem)
  , test (qtVersion >= [4, 2]) $ mkConstMethod'
      "item" "itemWithColumn" [intT, intT] (ptrT $ objT c_QStandardItem)
  , test (qtVersion >= [4, 2]) $ mkConstMethod
      "itemFromIndex" [objT c_QModelIndex] (ptrT $ objT c_QStandardItem)
  , test (qtVersion >= [4, 2]) $
    mkConstMethod "itemPrototype" np (ptrT . constT $ objT c_QStandardItem)
  , test (qtVersion >= [4, 2]) $ mkMethod "setColumnCount" [intT] voidT
  , test (qtVersion >= [4, 2]) $
    mkMethod "setHorizontalHeaderItem" [intT, ptrT $ objT c_QStandardItem] voidT
  , test (qtVersion >= [4, 2]) $
    mkMethod "setHorizontalHeaderLabels" [objT c_QStringList] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod'
      "setItem"
      "setItemWithColumn"
      [intT, intT, ptrT $ objT c_QStandardItem]
      voidT
  , just $ mkMethod "setItem" [intT, ptrT $ objT c_QStandardItem] voidT
  , test (qtVersion >= [4, 2]) $
    mkMethod "setItemPrototype" [ptrT . constT $ objT c_QStandardItem] voidT
  -- TODO just $ mkMethod "setItemRoleNames" [objT c_QHash_int_QByteArray] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setRowCount" [intT] voidT
  , test (qtVersion >= [4, 2]) $
    mkMethod "setVerticalHeaderItem" [intT, ptrT $ objT c_QStandardItem] voidT
  , test (qtVersion >= [4, 2]) $
    mkMethod "setVerticalHeaderLabels" [objT c_QStringList] voidT
  , test (qtVersion >= [4, 2]) $
    mkMethod "takeColumn" [intT] (objT c_QListQStandardItem)
  , test (qtVersion >= [4, 2]) $
    mkMethod "takeHorizontalHeaderItem" [intT] (ptrT $ objT c_QStandardItem)
  , test (qtVersion >= [4, 2]) $
    mkMethod "takeItem" [intT] (ptrT $ objT c_QStandardItem)
  , test (qtVersion >= [4, 2]) $ mkMethod'
      "takeItem" "takeItemWithColumn" [intT, intT] (ptrT $ objT c_QStandardItem)
  , test (qtVersion >= [4, 2]) $
    mkMethod "takeRow" [intT] (objT c_QListQStandardItem)
  , test (qtVersion >= [4, 2]) $
    mkMethod "takeVerticalHeaderItem" [intT] (ptrT $ objT c_QStandardItem)
  , test (qtVersion >= [4, 2]) $
    mkConstMethod "verticalHeaderItem" [intT] (ptrT $ objT c_QStandardItem)
  ]

c_QStandardItem :: Class
c_QStandardItem =
  addReqIncludes [includeStd "QStandardItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QStandardItem") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithText" [objT c_QString]
  , just $ mkCtor "newWithIconAndText" [objT c_QIcon, objT c_QString]
  , just $ mkCtor "newWithRows" [intT]
  , just $ mkCtor "newWithRowsAndColumns" [intT, intT]
  , just $ mkConstMethod "accessibleDescription" np (objT c_QString)
  , just $ mkConstMethod "accessibleText" np (objT c_QString)
  , just $ mkMethod "appendColumn" [objT c_QListQStandardItem] voidT
  , just $
    mkMethod' "appendRow" "appendRowItems" [objT c_QListQStandardItem] voidT
  , just $
    mkMethod' "appendRow" "appendRowItem" [ptrT $ objT c_QStandardItem] voidT
  , just $ mkMethod "appendRows" [objT c_QListQStandardItem] voidT
  , just $ mkConstMethod "background" np (objT c_QBrush)
  , just $ mkConstMethod "checkState" np (enumT e_CheckState)
  , just $ mkConstMethod "child" [intT] (ptrT $ objT c_QStandardItem)
  , just $ mkConstMethod'
      "child" "childWithColumn" [intT, intT] (ptrT $ objT c_QStandardItem)
  , just $ mkConstMethod "clone" np (ptrT $ objT c_QStandardItem)
  , just $ mkConstMethod "column" np intT
  , just $ mkConstMethod "columnCount" np intT
  , just $ mkConstMethod' "data" "getData" np (objT c_QVariant)
  , just $ mkConstMethod' "data" "getDataWithRole" [intT] (objT c_QVariant)
  , just $ mkConstMethod "flags" np (flagsT fl_ItemFlags)
  , just $ mkConstMethod "font" np (objT c_QFont)
  , just $ mkConstMethod "foreground" np (objT c_QBrush)
  , just $ mkConstMethod "hasChildren" np boolT
  , just $ mkConstMethod "icon" np (objT c_QIcon)
  , just $ mkConstMethod "index" np (objT c_QModelIndex)
  , just $ mkMethod "insertColumn" [intT, objT c_QListQStandardItem] voidT
  , just $ mkMethod "insertColumns" [intT, intT] voidT
  , just $ mkMethod'
      "insertRow" "insertRowItems" [intT, objT c_QListQStandardItem] voidT
  , just $ mkMethod'
      "insertRow" "insertRowItem" [intT, ptrT $ objT c_QStandardItem] voidT
  , just $ mkMethod'
      "insertRows" "insertRowsItems" [intT, objT c_QListQStandardItem] voidT
  , just $ mkMethod' "insertRows" "insertRowsCount" [intT, intT] voidT
  , test (qtVersion >= [5, 6]) $ mkConstMethod "isAutoTristate" np boolT
  , just $ mkConstMethod "isCheckable" np boolT
  , just $ mkConstMethod "isDragEnabled" np boolT
  , just $ mkConstMethod "isDropEnabled" np boolT
  , just $ mkConstMethod "isEditable" np boolT
  , just $ mkConstMethod "isEnabled" np boolT
  , just $ mkConstMethod "isSelectable" np boolT
  , test (qtVersion >= [5, 6]) $ mkConstMethod "isUserTristate" np boolT
  , just $ mkConstMethod "model" np (ptrT $ objT c_QStandardItemModel)
  , just $ mkConstMethod "parent" np (ptrT $ objT c_QStandardItem)
  -- TODO mkMethod "read" [objT c_QDataStream] voidT
  , just $ mkMethod "removeColumn" [intT] voidT
  , just $ mkMethod "removeColumns" [intT, intT] voidT
  , just $ mkMethod "removeRow" [intT] voidT
  , just $ mkMethod "removeRows" [intT, intT] voidT
  , just $ mkConstMethod "row" np intT
  , just $ mkConstMethod "rowCount" np intT
  , just $ mkMethod "setAccessibleDescription" [objT c_QString] voidT
  , just $ mkMethod "setAccessibleText" [objT c_QString] voidT
  , test (qtVersion >= [5, 6]) $ mkMethod "setAutoTristate" [boolT] voidT
  , just $ mkMethod "setBackground" [objT c_QBrush] voidT
  , just $ mkMethod "setCheckState" [enumT e_CheckState] voidT
  , just $ mkMethod "setCheckable" [boolT] voidT
  , just $ mkMethod'
      "setChild"
      "setChildWithColumn"
      [intT, intT, ptrT $ objT c_QStandardItem]
      voidT
  , just $ mkMethod "setChild" [intT, ptrT $ objT c_QStandardItem] voidT
  , just $ mkMethod "setColumnCount" [intT] voidT
  , just $ mkMethod "setData" [objT c_QVariant] voidT
  , just $ mkMethod' "setData" "setDataWithRole" [objT c_QVariant, intT] voidT
  , just $ mkMethod "setDragEnabled" [boolT] voidT
  , just $ mkMethod "setDropEnabled" [boolT] voidT
  , just $ mkMethod "setEditable" [boolT] voidT
  , just $ mkMethod "setEnabled" [boolT] voidT
  , just $ mkMethod "setFlags" [flagsT fl_ItemFlags] voidT
  , just $ mkMethod "setFont" [objT c_QFont] voidT
  , just $ mkMethod "setForeground" [objT c_QBrush] voidT
  , just $ mkMethod "setIcon" [objT c_QIcon] voidT
  , just $ mkMethod "setRowCount" [intT] voidT
  , just $ mkMethod "setSelectable" [boolT] voidT
  , just $ mkMethod "setSizeHint" [objT c_QSize] voidT
  , just $ mkMethod "setStatusTip" [objT c_QString] voidT
  , just $ mkMethod "setText" [objT c_QString] voidT
  , just $ mkMethod "setTextAlignment" [flagsT fl_Alignment] voidT
  , just $ mkMethod "setToolTip" [objT c_QString] voidT
  , test (qtVersion >= [5, 6]) $ mkMethod "setUserTristate" [boolT] voidT
  , just $ mkMethod "setWhatsThis" [objT c_QString] voidT
  , just $ mkConstMethod "sizeHint" np (objT c_QSize)
  , just $ mkMethod "sortChildren" [intT] voidT
  , just $ mkMethod'
      "sortChildren" "sortChildrenWithOrder" [intT, enumT e_SortOrder] voidT
  , just $ mkConstMethod "statusTip" np (objT c_QString)
  , just $ mkMethod "takeChild" [intT] (ptrT $ objT c_QStandardItem)
  , just $ mkMethod'
      "takeChild"
      "takeChildWithColumn"
      [intT, intT]
      (ptrT $ objT c_QStandardItem)
  , just $ mkMethod "takeColumn" [intT] (objT c_QListQStandardItem)
  , just $ mkMethod "takeRow" [intT] (objT c_QListQStandardItem)
  , just $ mkConstMethod "text" np (objT c_QString)
  , just $ mkConstMethod "textAlignment" np (flagsT fl_Alignment)
  , just $ mkConstMethod "toolTip" np (objT c_QString)
  , just $ mkConstMethod' "type" "getType" np intT
  , just $ mkConstMethod "whatsThis" np (objT c_QString)
  -- TODO mkConstMethod "write" [objT c_QDataStream] voidT
  ]

c_QListQStandardItem :: Class
c_QListQStandardItem = c_QList contents_QStandardItem

contents_QStandardItem :: Contents
contents_QStandardItem =
  instantiate "QListQStandardItem" (ptrT $ objT c_QStandardItem) mempty
