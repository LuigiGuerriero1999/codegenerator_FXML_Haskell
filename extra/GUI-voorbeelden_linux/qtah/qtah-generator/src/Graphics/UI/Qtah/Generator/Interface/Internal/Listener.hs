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

module Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  -- * Listeners.
  ListenerDef (..),
  listeners,
  -- * Haskell listener bindings.
  aModule,
  listener,
  listenerBool,
  listenerDirection,
  listenerDockWidgetArea,
  listenerDockWidgetAreas,
  listenerDouble,
  listenerInt,
  listenerIntBool,
  listenerIntExitStatus,
  listenerIntInt,
  listenerIntQlonglong,
  listenerOrientation,
  listenerOrientationIntInt,
  listenerProcessError,
  listenerProcessState,
  listenerPtrQAbstractButton,
  listenerPtrQAbstractButtonBool,
  listenerPtrQAbstractItemModel,
  listenerPtrQAction,
  listenerPtrQMdiSubWindow,
  listenerPtrQObject,
  listenerPtrQTreeWidgetItem,
  listenerPtrQTreeWidgetItemInt,
  listenerPtrQTreeWidgetItemPtrQTreeWidgetItem,
  listenerPtrQWidgetPtrQWidget,
  listenerQAbstractAnimation,
  listenerQAbstractSliderAction,
  listenerQClipboardMode,
  listenerQDate,
  listenerQDockWidgetFeatures,
  listenerQModelIndex,
  listenerQModelIndexIntInt,
  listenerQModelIndexIntIntQModelIndexInt,
  listenerQModelIndexQModelIndex,
  listenerQModelIndexQModelIndexQVectorInt,
  listenerQPoint,
  listenerQSize,
  listenerQString,
  listenerQSystemTrayIconActivationReason,
  listenerQWindowVisibility,
  listenerQlonglong,
  listenerQreal,
  listenerRefConstQIcon,
  listenerRefConstQItemSelectionRefConstQItemSelection,
  listenerRefConstQListQModelIndex,
  listenerRefConstQModelIndex,
  listenerRefConstQVariant,
  listenerScreenOrientation,
  listenerStateState,
  listenerToolBarAreas,
  listenerToolButtonStyle,
  listenerWindowModality,
  listenerWindowState,
  listenerWindowStatesWindowStates,
  ) where

import Data.List (intercalate)
import Foreign.Hoppy.Generator.Version (Filtered, collect, just, test)
import Foreign.Hoppy.Generator.Spec (
  Callback,
  Export (Export),
  addReqIncludes,
  ident,
  includeLocal,
  makeClass,
  makeModule,
  mkConstMethod,
  mkCtor,
  moduleAddExports,
  moduleAddHaskellName,
  moduleModify',
  np,
  )
import Foreign.Hoppy.Generator.Std.String (c_string)
import Foreign.Hoppy.Generator.Types (boolT, callbackT, objT, ptrT)
import Graphics.UI.Qtah.Generator.Config (Version, qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Internal.Callback hiding (aModule)
import Graphics.UI.Qtah.Generator.Module (AModule (AHoppyModule))
import Graphics.UI.Qtah.Generator.Types (ListenerInfo (ListenerInfo))

aModule :: AModule
aModule =
  AHoppyModule $
  addReqIncludes [includeLocal "listener.hpp"] $
  moduleModify' (makeModule "listener" "b_listener.hpp" "b_listener.cpp") $ do
    moduleAddHaskellName ["Internal", "Listener"]
    moduleAddExports $ map listenerHsExport listeners

data ListenerDef = ListenerDef
  { listenerName :: String
  , listenerMinVersion :: Maybe Version
  , listenerClassName :: String
  , listenerCallbackClassName :: String
  , listenerCppParamList :: String
  , listenerCppParamTypeList :: String
  , listenerCppParamNameList :: String
  , listenerHsExport :: Export
  }

-- | A 'ListenerDef' without a minimum version.
listenerDef :: String -> Callback -> [String] -> (Filtered ListenerDef, ListenerInfo)
listenerDef = listenerDefVersioned Nothing

listenerDefVersioned :: Maybe Version -> String -> Callback -> [String] -> (Filtered ListenerDef, ListenerInfo)
listenerDefVersioned minVersion name callback cppParams = (maybeDef, info)
  where
    maybeDef =
      maybe just (\mv -> test $ qtVersion >= mv) minVersion $
      ListenerDef
      { listenerName = name
      , listenerMinVersion = minVersion
      , listenerClassName = className
      , listenerCallbackClassName = "Callback" ++ name ++ "Void"
      , listenerCppParamList = intercalate ", " $ zipWith (\x y -> x ++ " " ++ y) cppParams paramNames
      , listenerCppParamTypeList = intercalate ", " cppParams
      , listenerCppParamNameList = intercalate ", " $ take (length cppParams) paramNames
      , listenerHsExport = Export cls
      }

    -- 'error' sucks, but a confusing message about c_WhateverClass not being
    -- exported when a user tries to compile qtah-generator would be worse.
    info = case maybeDef of
      Nothing ->
        error $ "Internal error, qtah-generator is making use of listener " ++ show name ++
        " which requires Qt " ++ show minVersion ++ ", but we are building against Qt " ++
        show qtVersion ++ "; aborting."
      Just _ -> ListenerInfo cls callback

    className = "Listener" ++ name

    cls =
      makeClass (ident className) Nothing [c_QObject]
      [ mkCtor "new" [ptrT $ objT c_QObject, objT c_string, callbackT callback]
      , mkConstMethod "isValid" np boolT
      ]

paramNames :: [String]
paramNames = ["arg" ++ show x | x <- [1..]]

listeners :: [ListenerDef]
listeners =
  collect
  [ ld
  , ldBool
  , ldDirection
  , ldDockWidgetArea
  , ldDockWidgetAreas
  , ldDouble
  , ldInt
  , ldIntBool
  , ldIntExitStatus
  , ldIntInt
  , ldIntQlonglong
  , ldOrientation
  , ldOrientationIntInt
  , ldProcessError
  , ldProcessState
  , ldPtrQAbstractButton
  , ldPtrQAbstractButtonBool
  , ldPtrQAbstractItemModel
  , ldPtrQAction
  , ldPtrQMdiSubWindow
  , ldPtrQObject
  , ldPtrQTreeWidgetItem
  , ldPtrQTreeWidgetItemInt
  , ldPtrQTreeWidgetItemPtrQTreeWidgetItem
  , ldPtrQWidgetPtrQWidget
  , ldQAbstractAnimation
  , ldQAbstractSliderAction
  , ldQClipboardMode
  , ldQDate
  , ldQDockWidgetFeatures
  , ldQModelIndex
  , ldQModelIndexIntInt
  , ldQModelIndexIntIntQModelIndexInt
  , ldQModelIndexQModelIndex
  , ldQModelIndexQModelIndexQVectorInt
  , ldQPoint
  , ldQSize
  , ldQString
  , ldQSystemTrayIconActivationReason
  , ldQWindowVisibility
  , ldQlonglong
  , ldQreal
  , ldRefConstQIcon
  , ldRefConstQItemSelectionRefConstQItemSelection
  , ldRefConstQListQModelIndex
  , ldRefConstQModelIndex
  , ldRefConstQVariant
  , ldScreenOrientation
  , ldStateState
  , ldToolBarAreas
  , ldToolButtonStyle
  , ldWindowModality
  , ldWindowState
  , ldWindowStatesWindowStates
  ]

ld :: Filtered ListenerDef
listener :: ListenerInfo
(ld, listener) = listenerDef "" cb_Void []

(ldBool, listenerBool) =
  listenerDef "Bool" cb_BoolVoid ["bool"]

(ldDirection, listenerDirection) =
  listenerDef "Direction" cb_DirectionVoid ["QAbstractAnimation::Direction"]

(ldDockWidgetArea, listenerDockWidgetArea) =
  listenerDef "DockWidgetArea" cb_DockWidgetAreaVoid ["Qt::DockWidgetArea"]

(ldDockWidgetAreas, listenerDockWidgetAreas) =
  listenerDef "DockWidgetAreas" cb_DockWidgetAreasVoid ["Qt::DockWidgetAreas"]

(ldDouble, listenerDouble) =
  listenerDef "Double" cb_DoubleVoid ["double"]

(ldInt, listenerInt) =
  listenerDef "Int" cb_IntVoid ["int"]

(ldIntBool, listenerIntBool) =
  listenerDef "IntBool" cb_IntBoolVoid ["int", "bool"]

(ldIntExitStatus, listenerIntExitStatus) =
  listenerDef "IntExitStatus" cb_IntExitStatusVoid ["int", "QProcess::ExitStatus"]

(ldIntInt, listenerIntInt) =
  listenerDef "IntInt" cb_IntIntVoid ["int", "int"]

(ldIntQlonglong, listenerIntQlonglong) =
  listenerDef "IntQlonglong" cb_IntQlonglongVoid ["int", "qlonglong"]

(ldOrientation, listenerOrientation) =
  listenerDef "Orientation" cb_OrientationVoid ["Qt::Orientation"]

(ldOrientationIntInt, listenerOrientationIntInt) =
  listenerDef "OrientationIntInt" cb_OrientationIntIntVoid ["Qt::Orientation", "int", "int"]

(ldProcessError, listenerProcessError) =
  listenerDef "ProcessError" cb_ProcessErrorVoid ["QProcess::ProcessError"]

(ldProcessState, listenerProcessState) =
  listenerDef "ProcessState" cb_ProcessStateVoid ["QProcess::ProcessState"]

(ldPtrQAbstractButton, listenerPtrQAbstractButton) =
  listenerDef "PtrQAbstractButton" cb_PtrQAbstractButtonVoid ["QAbstractButton*"]

(ldPtrQAbstractButtonBool, listenerPtrQAbstractButtonBool) =
  listenerDef "PtrQAbstractButtonBool" cb_PtrQAbstractButtonBoolVoid ["QAbstractButton*", "bool"]

(ldPtrQAbstractItemModel, listenerPtrQAbstractItemModel) =
  listenerDef "PtrQAbstractItemModel" cb_PtrQAbstractItemModelVoid ["QAbstractItemModel*"]

(ldPtrQAction, listenerPtrQAction) =
  listenerDef "PtrQAction" cb_PtrQActionVoid ["QAction*"]

(ldPtrQMdiSubWindow, listenerPtrQMdiSubWindow) =
  listenerDef "PtrQMdiSubWindow" cb_PtrQMdiSubWindowVoid ["QMdiSubWindow*"]

(ldPtrQObject, listenerPtrQObject) =
  listenerDef "PtrQObject" cb_PtrQObjectVoid ["QObject*"]

(ldPtrQTreeWidgetItem, listenerPtrQTreeWidgetItem) =
  listenerDef "PtrQTreeWidgetItem" cb_PtrQTreeWidgetItemVoid ["QTreeWidgetItem*"]

(ldPtrQTreeWidgetItemInt, listenerPtrQTreeWidgetItemInt) =
  listenerDef "PtrQTreeWidgetItemInt" cb_PtrQTreeWidgetItemIntVoid ["QTreeWidgetItem*", "int"]

(ldPtrQTreeWidgetItemPtrQTreeWidgetItem, listenerPtrQTreeWidgetItemPtrQTreeWidgetItem) =
  listenerDef "PtrQTreeWidgetItemPtrQTreeWidgetItem" cb_PtrQTreeWidgetItemPtrQTreeWidgetItemVoid ["QTreeWidgetItem*", "QTreeWidgetItem*"]

(ldPtrQWidgetPtrQWidget, listenerPtrQWidgetPtrQWidget) =
  listenerDef "PtrQWidgetPtrQWidget" cb_PtrQWidgetPtrQWidgetVoid ["QWidget*", "QWidget*"]

(ldQAbstractAnimation, listenerQAbstractAnimation) =
  listenerDef "QAbstractAnimation" cb_QAbstractAnimationVoid ["QAbstractAnimation*"]

(ldQAbstractSliderAction, listenerQAbstractSliderAction) =
  listenerDef "QAbstractSliderAction" cb_QAbstractSliderActionVoid ["QAbstractSlider::SliderAction"]

(ldQClipboardMode, listenerQClipboardMode) =
  listenerDef "QClipboardMode" cb_QClipboardModeVoid ["QClipboard::Mode"]

(ldQDate, listenerQDate) =
  listenerDef "QDate" cb_QDateVoid ["QDate"]

(ldQDockWidgetFeatures, listenerQDockWidgetFeatures) =
  listenerDef "QDockWidgetFeatures" cb_QDockWidgetFeaturesVoid ["QDockWidget::DockWidgetFeatures"]

(ldQModelIndex, listenerQModelIndex) =
  listenerDef "QModelIndex" cb_QModelIndexVoid ["QModelIndex"]

(ldQModelIndexIntInt, listenerQModelIndexIntInt) =
  listenerDef "QModelIndexIntInt" cb_QModelIndexIntIntVoid ["QModelIndex", "int", "int"]

(ldQModelIndexIntIntQModelIndexInt, listenerQModelIndexIntIntQModelIndexInt) =
  listenerDef "QModelIndexIntIntQModelIndexInt" cb_QModelIndexIntIntQModelIndexIntVoid ["QModelIndex", "int", "int", "QModelIndex", "int"]

(ldQModelIndexQModelIndex, listenerQModelIndexQModelIndex) =
  listenerDef "QModelIndexQModelIndex" cb_QModelIndexQModelIndexVoid ["QModelIndex", "QModelIndex"]

(ldQModelIndexQModelIndexQVectorInt, listenerQModelIndexQModelIndexQVectorInt) =
  listenerDef "QModelIndexQModelIndexQVectorInt" cb_QModelIndexQModelIndexQVectorIntVoid ["QModelIndex", "QModelIndex", "QVector<int>"]

(ldQPoint, listenerQPoint) =
  listenerDef "QPoint" cb_QPointVoid ["QPoint"]

(ldQSize, listenerQSize) =
  listenerDef "QSize" cb_QSizeVoid ["QSize"]

(ldQString, listenerQString) =
  listenerDef "QString" cb_QStringVoid ["QString"]

(ldQSystemTrayIconActivationReason, listenerQSystemTrayIconActivationReason) =
  listenerDef "QSystemTrayIconActivationReason" cb_QSystemTrayIconActivationReasonVoid ["QSystemTrayIcon::ActivationReason"]

(ldQWindowVisibility, listenerQWindowVisibility) =
  listenerDefVersioned (Just [5, 0]) "QWindowVisibility" cb_QWindowVisibilityVoid ["QWindow::Visibility"]

(ldQlonglong, listenerQlonglong) =
  listenerDef "Qlonglong" cb_QlonglongVoid ["qlonglong"]

(ldQreal, listenerQreal) =
  listenerDef "Qreal" cb_QrealVoid ["qreal"]

(ldRefConstQIcon, listenerRefConstQIcon) =
  listenerDef "RefConstQIcon" cb_RefConstQIconVoid ["const QIcon&"]

(ldRefConstQItemSelectionRefConstQItemSelection, listenerRefConstQItemSelectionRefConstQItemSelection) =
  listenerDef "RefConstQItemSelectionRefConstQItemSelection" cb_RefConstQItemSelectionRefConstQItemSelectionVoid ["const QItemSelection&", "const QItemSelection&"]

(ldRefConstQListQModelIndex, listenerRefConstQListQModelIndex) =
  listenerDef "RefConstQListQModelIndex" cb_RefConstQListQModelIndexVoid ["const QList<QModelIndex>&"]

(ldRefConstQModelIndex, listenerRefConstQModelIndex) =
  listenerDef "RefConstQModelIndex" cb_RefConstQModelIndexVoid ["const QModelIndex&"]

(ldRefConstQVariant, listenerRefConstQVariant) =
  listenerDef "RefConstQVariant" cb_RefConstQVariantVoid ["const QVariant&"]

(ldScreenOrientation, listenerScreenOrientation) =
  listenerDefVersioned (Just [5, 0]) "ScreenOrientation" cb_ScreenOrientationVoid ["Qt::ScreenOrientation"]

(ldStateState, listenerStateState) =
  listenerDef "StateState" cb_StateStateVoid ["QAbstractAnimation::State", "QAbstractAnimation::State"]

(ldToolBarAreas, listenerToolBarAreas) =
  listenerDef "ToolBarAreas" cb_ToolBarAreasVoid ["Qt::ToolBarAreas"]

(ldToolButtonStyle, listenerToolButtonStyle) =
  listenerDef "ToolButtonStyle" cb_ToolButtonStyleVoid ["Qt::ToolButtonStyle"]

(ldWindowModality, listenerWindowModality) =
  listenerDef "WindowModality" cb_WindowModalityVoid ["Qt::WindowModality"]

(ldWindowState, listenerWindowState) =
  listenerDef "WindowState" cb_WindowStateVoid ["Qt::WindowState"]

(ldWindowStatesWindowStates, listenerWindowStatesWindowStates) =
  listenerDef "WindowStatesWindowStates" cb_WindowStatesWindowStatesVoid ["Qt::WindowStates", "Qt::WindowStates"]

