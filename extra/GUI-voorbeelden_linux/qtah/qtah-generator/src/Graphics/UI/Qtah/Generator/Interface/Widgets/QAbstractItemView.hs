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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemView (
  aModule,
  c_QAbstractItemView,
  e_ScrollHint,
  e_SelectionMode,
  e_ScrollMode,
  e_DragDropMode,
  e_EditTrigger,
  fl_EditTriggers,
  e_SelectionBehavior,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolHasProp,
  mkConstMethod,
  mkConstMethod',
  mkMethod,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  enumT,
  intT,
  objT,
  ptrT,
  voidT,
  )
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (c_QAbstractItemModel)
import Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionModel (c_QItemSelectionModel)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  e_DropAction,
  e_TextElideMode,
  )
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerQModelIndex,
  listenerQSize,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemDelegate (c_QAbstractItemDelegate)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractScrollArea (c_QAbstractScrollArea)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractItemView"]
  [ QtExportClassAndSignals c_QAbstractItemView signals
  , qtExport e_DragDropMode
  , qtExport e_EditTrigger
  , qtExport fl_EditTriggers
  , qtExport e_ScrollHint
  , qtExport e_ScrollMode
  , qtExport e_SelectionBehavior
  , qtExport e_SelectionMode
  ]

(c_QAbstractItemView, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QAbstractItemView"] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractItemView") Nothing [c_QAbstractScrollArea]
  [ mkProp "alternatingRowColors" boolT
  , mkBoolHasProp "autoScroll"
  , mkProp "autoScrollMargin" intT
  , mkMethod "clearSelection" np voidT
  , mkMethod "closePersistentEditor" [objT c_QModelIndex] voidT
  , mkProp "currentIndex" $ objT c_QModelIndex
  , mkProp "defaultDropAction" $ enumT e_DropAction
  , mkProp "dragDropMode" $ enumT e_DragDropMode
  , mkProp "dragDropOverwriteMode" boolT
  , mkProp "dragEnabled" boolT
  , mkMethod "edit" [objT c_QModelIndex] voidT
  , mkProp "editTriggers" $ flagsT fl_EditTriggers
  , mkProp "horizontalScrollMode" $ enumT e_ScrollMode
  , mkProp "iconSize" $ objT c_QSize
  , mkConstMethod "indexAt" [objT c_QPoint] $ objT c_QModelIndex
  , mkConstMethod "indexWidget" [objT c_QModelIndex] $ ptrT $ objT c_QWidget
  , mkConstMethod' "itemDelegate" "itemDelegate" np $
    ptrT $ objT c_QAbstractItemDelegate
  , mkConstMethod' "itemDelegate" "itemDelegateAt" [objT c_QModelIndex] $
    ptrT $ objT c_QAbstractItemDelegate
  , mkConstMethod "itemDelegateForColumn" [intT] $
    ptrT $ objT c_QAbstractItemDelegate
  , mkConstMethod "itemDelegateForRow" [intT] $
    ptrT $ objT c_QAbstractItemDelegate
  , mkMethod "keyboardSearch" [objT c_QString] voidT
  , mkProp "model" $ ptrT $ objT c_QAbstractItemModel
  , mkMethod "openPersistentEditor" [objT c_QModelIndex] voidT
  , mkMethod "reset" np voidT
  , mkProp "rootIndex" $ objT c_QModelIndex
  , mkMethod' "scrollTo" "scrollTo" [objT c_QModelIndex] voidT
  , mkMethod' "scrollTo" "scrollToWithHint" [objT c_QModelIndex, enumT e_ScrollHint] voidT
  , mkMethod "scrollToBottom" np voidT
  , mkMethod "scrollToTop" np voidT
  , mkMethod "selectAll" np voidT
  , mkProp "selectionBehavior" $ enumT e_SelectionBehavior
  , mkProp "selectionMode" $ enumT e_SelectionMode
  , mkProp "selectionModel" $ ptrT $ objT c_QItemSelectionModel
  , mkMethod "setDropIndicatorShown" [boolT] voidT
  , mkMethod "setIndexWidget" [objT c_QModelIndex, ptrT $ objT c_QWidget] voidT
  , mkMethod "setItemDelegate" [ptrT $ objT c_QAbstractItemDelegate] voidT
  , mkMethod "setItemDelegateForColumn" [intT, ptrT $ objT c_QAbstractItemDelegate] voidT
  , mkMethod "setItemDelegateForRow" [intT, ptrT $ objT c_QAbstractItemDelegate] voidT
  , mkConstMethod "showDropIndicator" np boolT
  , mkConstMethod "sizeHintForColumn" [intT] intT
  , mkConstMethod "sizeHintForIndex" [objT c_QModelIndex] $ objT c_QSize
  , mkConstMethod "sizeHintForRow" [intT] intT
  , mkProp "tabKeyNavigation" boolT
  , mkProp "textElideMode" $ enumT e_TextElideMode
  , mkMethod "update" [objT c_QModelIndex] voidT
  , mkProp "verticalScrollMode" $ enumT e_ScrollMode
  , mkConstMethod "visualRect" [objT c_QModelIndex] $ objT c_QRect
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "activated" listenerQModelIndex
  , makeSignal "clicked" listenerQModelIndex
  , makeSignal "doubleClicked" listenerQModelIndex
  , makeSignal "entered" listenerQModelIndex
  , makeSignal "iconSizeChanged" listenerQSize
  , makeSignal "pressed" listenerQModelIndex
  , makeSignal "viewportEntered" listener
  ]

e_DragDropMode =
  makeQtEnum (ident1 "QAbstractItemView" "DragDropMode") [includeStd "QAbstractItemView"]
  [ "NoDragDrop"
  , "DragOnly"
  , "DropOnly"
  , "DragDrop"
  , "InternalMove"
  ]

(e_EditTrigger, fl_EditTriggers) =
  makeQtEnumAndFlags (ident1 "QAbstractItemView" "EditTrigger") "EditTriggers"
  [includeStd "QAbstractItemView"]
  [ "NoEditTriggers"
  , "CurrentChanged"
  , "DoubleClicked"
  , "SelectedClicked"
  , "EditKeyPressed"
  , "AnyKeyPressed"
  , "AllEditTriggers"
  ]

e_ScrollHint =
  makeQtEnum (ident1 "QAbstractItemView" "ScrollHint") [includeStd "QAbstractItemView"]
  [ "EnsureVisible"
  , "PositionAtTop"
  , "PositionAtBottom"
  , "PositionAtCenter"
  ]

e_ScrollMode =
  makeQtEnum (ident1 "QAbstractItemView" "ScrollMode") [includeStd "QAbstractItemView"]
  [ "ScrollPerItem"
  , "ScrollPerPixel"
  ]

e_SelectionBehavior =
  makeQtEnum (ident1 "QAbstractItemView" "SelectionBehavior") [includeStd "QAbstractItemView"]
  [ "SelectItems"
  , "SelectRows"
  , "SelectColumns"
  ]

e_SelectionMode =
  makeQtEnum (ident1 "QAbstractItemView" "SelectionMode") [includeStd "QAbstractItemView"]
  [ "NoSelection"
  , "SingleSelection"
  , "MultiSelection"
  , "ExtendedSelection"
  , "ContiguousSelection"
  ]
