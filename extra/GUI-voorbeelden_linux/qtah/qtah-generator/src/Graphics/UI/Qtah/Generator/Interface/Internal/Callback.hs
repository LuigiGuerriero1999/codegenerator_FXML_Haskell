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

module Graphics.UI.Qtah.Generator.Interface.Internal.Callback where

import Foreign.Hoppy.Generator.Spec (
  Callback,
  Export (Export),
  addReqIncludes,
  includeStd,
  makeCallback,
  makeModule,
  moduleAddExports,
  moduleAddHaskellName,
  moduleModify',
  np,
  toExtName,
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  constT,
  doubleT,
  enumT,
  intT,
  objT,
  ptrT,
  refT,
  toGcT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (c_QAbstractItemModel)
import Graphics.UI.Qtah.Generator.Interface.Core.QDate (c_QDate)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.QItemSelection (c_QItemSelection)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractAnimation (c_QAbstractAnimation)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractAnimation (e_Direction, e_State)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QProcess (e_ProcessError, e_ExitStatus, e_ProcessState)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorInt)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  e_DockWidgetArea,
  fl_DockWidgetAreas,
  e_Orientation,
  e_ScreenOrientation,
  e_ScreenOrientation_minVersion,
  fl_ToolBarAreas,
  e_ToolButtonStyle,
  e_WindowModality,
  e_WindowState,
  fl_WindowStates,
  qreal,
  qlonglong,
  )
import {-# SOURCE #-} qualified Graphics.UI.Qtah.Generator.Interface.Gui.QClipboard as QClipboard
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintEvent (c_QPaintEvent)
import {-# SOURCE #-} qualified Graphics.UI.Qtah.Generator.Interface.Gui.QWindow as QWindow
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton
  (c_QAbstractButton)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSlider (e_SliderAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QDockWidget (
  fl_DockWidgetFeatures,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsItem (c_QGraphicsItem)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QMdiSubWindow (c_QMdiSubWindow)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QSystemTrayIcon (
  e_ActivationReason,
  )
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeWidgetItem (c_QTreeWidgetItem)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AHoppyModule))

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AHoppyModule $
  moduleModify' (makeModule "callback" "b_callback.hpp" "b_callback.cpp") $ do
    moduleAddHaskellName ["Internal", "Callback"]
    moduleAddExports $ collect
      [ just $ Export cb_BoolVoid
      , just $ Export cb_DockWidgetAreaVoid
      , just $ Export cb_DockWidgetAreasVoid
      , just $ Export cb_DoubleVoid
      , just $ Export cb_IntVoid
      , just $ Export cb_IntBoolVoid
      , just $ Export cb_IntIntVoid
      , just $ Export cb_OrientationVoid
      , just $ Export cb_OrientationIntIntVoid
      , just $ Export cb_PtrQAbstractButtonVoid
      , just $ Export cb_PtrQAbstractButtonBoolVoid
      , just $ Export cb_PtrQAbstractItemModelVoid
      , just $ Export cb_PtrQActionVoid
      , just $ Export cb_PtrQGraphicsItemPtrQEventBool
      , just $ Export cb_PtrQMdiSubWindowVoid
      , just $ Export cb_PtrQObjectPtrQEventBool
      , just $ Export cb_PtrQObjectVoid
      , just $ Export cb_PtrQPaintEventVoid
      , just $ Export cb_RefConstQModelIndexVoid
      , just $ Export cb_RefConstQListQModelIndexVoid
      , just $ Export cb_PtrQTreeWidgetItemVoid
      , just $ Export cb_PtrQTreeWidgetItemIntVoid
      , just $ Export cb_PtrQTreeWidgetItemPtrQTreeWidgetItemVoid
      , just $ Export cb_PtrQWidgetPtrQWidgetVoid
      , just $ Export cb_QAbstractSliderActionVoid
      , just $ Export cb_QClipboardModeVoid
      , just $ Export cb_QDateVoid
      , just $ Export cb_QDockWidgetFeaturesVoid
      , just $ Export cb_QModelIndexVoid
      , just $ Export cb_QModelIndexIntIntVoid
      , just $ Export cb_QModelIndexIntIntQModelIndexIntVoid
      , just $ Export cb_QModelIndexQModelIndexVoid
      , just $ Export cb_QModelIndexQModelIndexQVectorIntVoid
      , test (qtVersion >= QWindow.minVersion) $ Export cb_QWindowVisibilityVoid
      , just $ Export cb_QPointVoid
      , just $ Export cb_QrealVoid
      , just $ Export cb_QSizeVoid
      , just $ Export cb_QStringVoid
      , just $ Export cb_QSystemTrayIconActivationReasonVoid
      , just $ Export cb_RefConstQIconVoid
      , just $ Export cb_RefConstQItemSelectionRefConstQItemSelectionVoid
      , test (qtVersion >= e_ScreenOrientation_minVersion) $ Export cb_ScreenOrientationVoid
      , just $ Export cb_ToolBarAreasVoid
      , just $ Export cb_ToolButtonStyleVoid
      , just $ Export cb_WindowModalityVoid
      , just $ Export cb_WindowStateVoid
      , just $ Export cb_WindowStatesWindowStatesVoid
      , just $ Export cb_QlonglongVoid
      , just $ Export cb_IntQlonglongVoid
      , just $ Export cb_ProcessErrorVoid
      , just $ Export cb_IntExitStatusVoid
      , just $ Export cb_ProcessStateVoid
      , just $ Export cb_StateStateVoid
      , just $ Export cb_DirectionVoid
      , just $ Export cb_RefConstQVariantVoid
      , just $ Export cb_QAbstractAnimationVoid
      , just $ Export cb_Void
      ]

cb_BoolVoid =
  makeCallback (toExtName "CallbackBoolVoid")
  [boolT] voidT

cb_DockWidgetAreaVoid =
  makeCallback (toExtName "CallbackDockWidgetAreaVoid")
  [enumT e_DockWidgetArea] voidT

cb_DockWidgetAreasVoid =
  makeCallback (toExtName "CallbackDockWidgetAreasVoid")
  [flagsT fl_DockWidgetAreas] voidT

cb_DoubleVoid =
  makeCallback (toExtName "CallbackDoubleVoid")
  [doubleT] voidT

cb_IntVoid =
  makeCallback (toExtName "CallbackIntVoid")
  [intT] voidT

cb_IntBoolVoid =
  makeCallback (toExtName "CallbackIntBoolVoid")
  [intT, boolT] voidT

cb_IntIntVoid =
  makeCallback (toExtName "CallbackIntIntVoid")
  [intT, intT] voidT

cb_OrientationVoid =
  makeCallback (toExtName "CallbackOrientationVoid")
  [enumT e_Orientation] voidT

cb_OrientationIntIntVoid =
  makeCallback (toExtName "CallbackOrientationIntIntVoid")
  [enumT e_Orientation, intT, intT] voidT

cb_PtrQAbstractButtonVoid =
  makeCallback (toExtName "CallbackPtrQAbstractButtonVoid")
  [ptrT $ objT c_QAbstractButton] voidT

cb_PtrQAbstractButtonBoolVoid =
  makeCallback (toExtName "CallbackPtrQAbstractButtonBoolVoid")
  [ptrT $ objT c_QAbstractButton, boolT] voidT

cb_PtrQAbstractItemModelVoid =
  makeCallback (toExtName "CallbackPtrQAbstractItemModelVoid")
  [ptrT $ objT c_QAbstractItemModel] voidT

cb_PtrQActionVoid =
  makeCallback (toExtName "CallbackPtrQActionVoid")
  [ptrT $ objT c_QAction] voidT

cb_PtrQGraphicsItemPtrQEventBool =
  makeCallback (toExtName "CallbackPtrQGraphicsItemPtrQEventBool")
  [ptrT $ objT c_QGraphicsItem, ptrT $ objT c_QEvent] boolT

cb_PtrQMdiSubWindowVoid =
  makeCallback (toExtName "CallbackPtrQMdiSubWindowVoid")
  [ptrT $ objT c_QMdiSubWindow] voidT

cb_PtrQObjectPtrQEventBool =
  makeCallback (toExtName "CallbackPtrQObjectPtrQEventBool")
  [ptrT $ objT c_QObject, ptrT $ objT c_QEvent] boolT

cb_PtrQObjectVoid =
  makeCallback (toExtName "CallbackPtrQObjectVoid")
  [ptrT $ objT c_QObject] voidT

cb_PtrQPaintEventVoid =
  makeCallback (toExtName "CallbackPtrQPaintEventVoid")
  [ptrT $ objT c_QPaintEvent] voidT

cb_RefConstQModelIndexVoid :: Callback
cb_RefConstQModelIndexVoid =
  makeCallback (toExtName "CallbackRefConstQModelIndexVoid")
  [refT $ constT $ objT c_QModelIndex] voidT

cb_RefConstQListQModelIndexVoid :: Callback
cb_RefConstQListQModelIndexVoid =
  makeCallback (toExtName "CallbackRefConstQListQModelIndexVoid")
  [refT $ constT $ objT c_QListQModelIndex] voidT

cb_PtrQTreeWidgetItemVoid :: Callback
cb_PtrQTreeWidgetItemVoid =
  makeCallback (toExtName "CallbackPtrQTreeWidgetItemVoid")
  [ptrT $ objT c_QTreeWidgetItem] voidT

cb_PtrQTreeWidgetItemIntVoid :: Callback
cb_PtrQTreeWidgetItemIntVoid =
  makeCallback (toExtName "CallbackPtrQTreeWidgetItemIntVoid")
  [ptrT $ objT c_QTreeWidgetItem, intT] voidT

cb_PtrQTreeWidgetItemPtrQTreeWidgetItemVoid :: Callback
cb_PtrQTreeWidgetItemPtrQTreeWidgetItemVoid =
  makeCallback (toExtName "CallbackPtrQTreeWidgetItemPtrQTreeWidgetItemVoid")
  [ptrT $ objT c_QTreeWidgetItem, ptrT $ objT c_QTreeWidgetItem] voidT

cb_PtrQWidgetPtrQWidgetVoid =
  makeCallback (toExtName "CallbackPtrQWidgetPtrQWidgetVoid")
  [ptrT $ objT c_QWidget, ptrT $ objT c_QWidget] voidT

cb_QAbstractSliderActionVoid =
  makeCallback (toExtName "CallbackQAbstractSliderActionVoid")
  [enumT e_SliderAction] voidT

cb_QClipboardModeVoid =
  makeCallback (toExtName "CallbackQClipboardModeVoid")
  [enumT QClipboard.e_Mode] voidT

cb_QDateVoid =
  makeCallback (toExtName "CallbackQDateVoid")
  [objT c_QDate] voidT

cb_QDockWidgetFeaturesVoid =
  addReqIncludes [includeStd "QDockWidget"] $
  makeCallback (toExtName "CallbackQDockWidgetFeaturesVoid")
  [flagsT fl_DockWidgetFeatures] voidT

cb_QModelIndexVoid =
  makeCallback (toExtName "CallbackQModelIndexVoid")
  [objT c_QModelIndex] voidT

cb_QModelIndexIntIntVoid =
  makeCallback (toExtName "CallbackQModelIndexIntIntVoid")
  [objT c_QModelIndex, intT, intT] voidT

cb_QModelIndexIntIntQModelIndexIntVoid =
  makeCallback (toExtName "CallbackQModelIndexIntIntQModelIndexIntVoid")
  [objT c_QModelIndex, intT, intT, objT c_QModelIndex, intT] voidT

cb_QModelIndexQModelIndexVoid =
  makeCallback (toExtName "CallbackQModelIndexQModelIndexVoid")
  [objT c_QModelIndex, objT c_QModelIndex] voidT

cb_QModelIndexQModelIndexQVectorIntVoid =
  makeCallback (toExtName "CallbackQModelIndexQModelIndexQVectorIntVoid")
  [objT c_QModelIndex, objT c_QModelIndex, toGcT $ objT c_QVectorInt] voidT

cb_QWindowVisibilityVoid =
  makeCallback (toExtName "CallbackQWindowVisibilityVoid")
  [enumT QWindow.e_Visibility] voidT

cb_QPointVoid =
  makeCallback (toExtName "CallbackQPointVoid")
  [objT c_QPoint] voidT

cb_QrealVoid =
  makeCallback (toExtName "CallbackQrealVoid")
  [qreal] voidT

cb_QSizeVoid =
  makeCallback (toExtName "CallbackQSizeVoid")
  [objT c_QSize] voidT

cb_QStringVoid =
  makeCallback (toExtName "CallbackQStringVoid")
  [objT c_QString] voidT

cb_QSystemTrayIconActivationReasonVoid =
  makeCallback (toExtName "CallbackQSystemTrayIconActivationReasonVoid")
  [enumT e_ActivationReason] voidT

cb_RefConstQIconVoid =
  makeCallback (toExtName "CallbackRefConstQIconVoid")
  [refT $ constT $ objT c_QIcon] voidT

cb_RefConstQItemSelectionRefConstQItemSelectionVoid =
  makeCallback (toExtName "CallbackRefConstQItemSelectionRefConstQItemSelectionVoid")
  [refT $ constT $ objT c_QItemSelection, refT $ constT $ objT c_QItemSelection] voidT

cb_ScreenOrientationVoid =
  makeCallback (toExtName "CallbackScreenOrientationVoid")
  [enumT e_ScreenOrientation] voidT

cb_ToolBarAreasVoid =
  makeCallback (toExtName "CallbackToolBarAreasVoid")
  [flagsT fl_ToolBarAreas] voidT

cb_ToolButtonStyleVoid =
  makeCallback (toExtName "CallbackToolButtonStyleVoid")
  [enumT e_ToolButtonStyle] voidT

cb_WindowModalityVoid =
  makeCallback (toExtName "CallbackWindowModalityVoid")
  [enumT e_WindowModality] voidT

cb_WindowStateVoid =
  makeCallback (toExtName "CallbackWindowStateVoid")
  [enumT e_WindowState] voidT

cb_WindowStatesWindowStatesVoid =
  makeCallback (toExtName "CallbackWindowStatesWindowStatesVoid")
  [flagsT fl_WindowStates, flagsT fl_WindowStates] voidT

cb_QlonglongVoid =
  makeCallback (toExtName "CallbackQlonglongVoid")
  [qlonglong] voidT

cb_IntQlonglongVoid =
  makeCallback (toExtName "CallbackIntQlonglongVoid")
  [intT, qlonglong] voidT


cb_ProcessErrorVoid =
  makeCallback (toExtName "CallbackProcessErrorVoid")
  [enumT e_ProcessError] voidT


cb_IntExitStatusVoid =
  makeCallback (toExtName "CallbackIntExitStatusVoid")
  [intT, enumT e_ExitStatus] voidT


cb_ProcessStateVoid =
  makeCallback (toExtName "CallbackProcessStateVoid")
  [enumT e_ProcessState] voidT


cb_DirectionVoid =
  makeCallback (toExtName "CallbackDirectionVoid")
  [enumT e_Direction] voidT


cb_StateStateVoid =
  makeCallback (toExtName "CallbackStateStateVoid")
  [enumT e_State, enumT e_State] voidT


cb_RefConstQVariantVoid =
  makeCallback (toExtName "CallbackRefConstQVariantVoid")
  [refT $ constT $ objT c_QVariant] voidT


cb_QAbstractAnimationVoid =
  makeCallback (toExtName "CallbackQAbstractAnimationVoid")
  [ptrT $ objT c_QAbstractAnimation] voidT


cb_Void =
  makeCallback (toExtName "CallbackVoid")
  np voidT
