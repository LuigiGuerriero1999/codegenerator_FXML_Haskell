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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeWidgetItem (
  -- modules
  aModule,
  c_QTreeWidgetItem,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Class,
  CppEnum,
  Operator (OpLt),
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
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, refT, constT, voidT, boolT, enumT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQTreeWidgetItem)
import Graphics.UI.Qtah.Generator.Interface.Gui.QBrush (c_QBrush)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeWidget (c_QTreeWidget)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Gui.QFont (c_QFont)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_CheckState, e_SortOrder, fl_ItemFlags)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTreeWidgetItem"] $
  [ qtExport c_QTreeWidgetItem
  , qtExport e_ChildIndicatorPolicy
  , qtExport e_ItemType
  ]

c_QTreeWidgetItem :: Class
c_QTreeWidgetItem =
  addReqIncludes [includeStd "QTreeWidgetItem"] $
  classAddFeatures [Assignable] $
  classSetEntityPrefix "" $
  makeClass (ident "QTreeWidgetItem") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithType" [intT]
  , just $ mkCtor "newWithStrings" [objT c_QStringList]
  , just $ mkCtor "newWithStringsAndType" [objT c_QStringList, intT]
  , just $ mkCtor "newWithParentTree" [ptrT $ objT c_QTreeWidget]
  , just $ mkCtor "newWithParentTreeAndType" [ptrT $ objT c_QTreeWidget, intT]
  , just $ mkCtor "newWithParentTreeAndStrings" [ptrT $ objT c_QTreeWidget, objT c_QStringList]
  , just $ mkCtor "newWithParentTreeAndStringsAndType"
    [ptrT $ objT c_QTreeWidget, objT c_QStringList, intT]
  , just $ mkCtor "newWithParentItem" [ptrT $ objT c_QTreeWidgetItem]
  , just $ mkCtor "newWithParentItemAndType" [ptrT $ objT c_QTreeWidgetItem, intT]
  , just $ mkCtor "newWithParentItemAndStrings" [ptrT $ objT c_QTreeWidgetItem, objT c_QStringList]
  , just $ mkCtor "newWithParentItemAndStringsAndType"
    [ptrT $ objT c_QTreeWidgetItem, objT c_QStringList, intT]
  , just $ mkMethod "addChild" [ptrT $ objT c_QTreeWidgetItem] voidT
  , test (qtVersion >= [4, 1]) $ mkMethod "addChildren" [refT $ constT $ objT c_QListQTreeWidgetItem] voidT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "background" [intT] $ objT c_QBrush
  , just $ mkConstMethod "checkState" [intT] $ enumT e_CheckState
  , just $ mkConstMethod "child" [intT] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod "childCount" np intT
  , just $ mkConstMethod "childIndicatorPolicy" np (enumT e_ChildIndicatorPolicy)
  , just $ mkConstMethod "clone" np $ ptrT $ objT c_QTreeWidgetItem
  , just $ mkConstMethod "columnCount" np intT
  , just $ mkConstMethod' "data" "getData" [intT, intT] (objT c_QVariant)
  , just $ mkConstMethod "flags" np $ flagsT fl_ItemFlags
  , just $ mkConstMethod "font" [intT] $ objT c_QFont
  , test (qtVersion >= [4, 2]) $ mkConstMethod "foreground" [intT] $ objT c_QBrush
  , just $ mkConstMethod "icon" [intT] $ objT c_QIcon
  , just $ mkConstMethod "indexOfChild" [ptrT $ objT c_QTreeWidgetItem] intT
  , just $ mkMethod "insertChild" [intT, ptrT $ objT c_QTreeWidgetItem] voidT
  , test (qtVersion >= [4, 1]) $ mkMethod "insertChildren" [intT, refT $ constT $ objT c_QListQTreeWidgetItem] voidT
  , test (qtVersion >= [4, 3]) $ mkConstMethod "isDisabled" np boolT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "isExpanded" np boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod "isFirstColumnSpanned" np boolT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "isHidden" np boolT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "isSelected" np boolT
  , just $ mkConstMethod "parent" np (ptrT $ objT c_QTreeWidgetItem)
  -- TODO void QTreeWidgetItem::read(QDataStream &in)
  , just $ mkMethod "removeChild" [ptrT $ objT c_QTreeWidgetItem] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setBackground" [intT, objT c_QBrush] voidT
  , just $ mkMethod "setCheckState" [intT, enumT e_CheckState] voidT
  , just $ mkMethod "setChildIndicatorPolicy" [enumT e_ChildIndicatorPolicy] voidT
  , just $ mkMethod "setData" [intT, intT, objT c_QVariant] voidT
  , test (qtVersion >= [4, 3]) $ mkMethod "setDisabled" [boolT] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setExpanded" [boolT] voidT
  , test (qtVersion >= [4, 3]) $ mkMethod "setFirstColumnSpanned" [boolT] voidT
  , just $ mkMethod "setFlags" [flagsT fl_ItemFlags] voidT
  , just $ mkMethod "setFont" [intT, objT c_QFont] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setForeground" [intT, objT c_QBrush] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setHidden" [boolT] voidT
  , just $ mkMethod "setIcon" [intT, objT c_QIcon] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setSelected" [boolT] voidT
  , test (qtVersion >= [4, 1]) $ mkMethod "setSizeHint" [intT, objT c_QSize] voidT
  , just $ mkMethod "setStatusTip" [intT, objT c_QString] voidT
  , just $ mkMethod "setText" [intT, objT c_QString] voidT
  , just $ mkMethod "setTextAlignment" [intT, intT] voidT
  , just $ mkMethod "setToolTip" [intT, objT c_QString] voidT
  , just $ mkMethod "setWhatsThis" [intT, objT c_QString] voidT
  , test (qtVersion >= [4, 1]) $ mkConstMethod "sizeHint" [intT] $ objT c_QSize
  , test (qtVersion >= [4, 2]) $ mkMethod "sortChildren" [intT, enumT e_SortOrder] voidT
  , just $ mkConstMethod "statusTip" [intT] $ objT c_QString
  , just $ mkMethod "takeChild" [intT] $ ptrT $ objT c_QTreeWidgetItem
  , test (qtVersion >= [4, 1]) $ mkMethod "takeChildren" np $ objT c_QListQTreeWidgetItem
  , just $ mkConstMethod "text" [intT] (objT c_QString)
  , just $ mkConstMethod "textAlignment" [intT] intT
  , just $ mkConstMethod "toolTip" [intT] $ objT c_QString
  , just $ mkConstMethod "treeWidget" np $ ptrT $ objT c_QTreeWidget
  , just $ mkConstMethod' "type" "getType" np intT
  , just $ mkConstMethod "whatsThis" [intT] $ objT c_QString
  , just $ mkMethod OpLt [refT $ constT $ objT c_QTreeWidgetItem] boolT
  -- TODO void QTreeWidgetItem::write(QDataStream &out) const
  ]


e_ChildIndicatorPolicy :: CppEnum
e_ChildIndicatorPolicy =
  makeQtEnum
    (ident1 "QTreeWidgetItem" "ChildIndicatorPolicy")
    [includeStd "QTreeWidgetItem"]
    [ "ShowIndicator"
    , "DontShowIndicator"
    , "DontShowIndicatorWhenChildless"
    ]

e_ItemType :: CppEnum
e_ItemType =
  makeQtEnum
    (ident1 "QTreeWidgetItem" "ItemType")
    [includeStd "QTreeWidgetItem"]
    [ "Type"
    , "UserType"
    ]

