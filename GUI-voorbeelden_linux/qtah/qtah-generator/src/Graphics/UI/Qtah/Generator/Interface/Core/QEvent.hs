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

module Graphics.UI.Qtah.Generator.Interface.Core.QEvent (
  aModule,
  c_QEvent,
  e_Type,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkStaticMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QEvent"]
  [ QtExportEvent c_QEvent
  , qtExport e_Type
  ]

c_QEvent =
  addReqIncludes [includeStd "QEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QEvent") Nothing [] $
  collect
  [ just $ mkCtor "new" [enumT e_Type]
  , just $ mkMethod "accept" np voidT
  , just $ mkBoolIsProp "accepted"
  , just $ mkMethod "ignore" np voidT
  , test (qtVersion >= [4, 4]) $
    mkStaticMethod' "registerEventType" "registerEventType" np intT
  , test (qtVersion >= [4, 4]) $
    mkStaticMethod' "registerEventType" "registerEventTypeWithHint" [intT] intT
  , just $ mkConstMethod "spontaneous" np boolT
  , just $ mkConstMethod' "type" "eventType" np $ enumT e_Type  -- 'type' is a Haskell keyword.
  ]

e_Type =
  makeQtEnum (ident1 "QEvent" "Type") [includeStd "QEvent"]
  [ -- Built-in event types:
    "None"
  , "ActionAdded"
  , "ActionChanged"
  , "ActionRemoved"
  , "ActivationChange"
  , "ApplicationActivate"
  , "ApplicationActivated"
  , "ApplicationDeactivate"
  , "ApplicationFontChange"
  , "ApplicationLayoutDirectionChange"
  , "ApplicationPaletteChange"
  , "ApplicationStateChange"
  , "ApplicationWindowIconChange"
  , "ChildAdded"
  , "ChildPolished"
  , "ChildRemoved"
  , "Clipboard"
  , "Close"
  , "CloseSoftwareInputPanel"
  , "ContentsRectChange"
  , "ContextMenu"
  , "CursorChange"
  , "DeferredDelete"
  , "DragEnter"
  , "DragLeave"
  , "DragMove"
  , "Drop"
  , "DynamicPropertyChange"
  , "EnabledChange"
  , "Enter"
    -- "EnterEditFocus" is omitted -- it depends on keypadNavigation.
  , "EnterWhatsThisMode"
  , "Expose"
  , "FileOpen"
  , "FocusIn"
  , "FocusOut"
  , "FocusAboutToChange"
  , "FontChange"
  , "Gesture"
  , "GestureOverride"
  , "GrabKeyboard"
  , "GrabMouse"
  , "GraphicsSceneContextMenu"
  , "GraphicsSceneDragEnter"
  , "GraphicsSceneDragLeave"
  , "GraphicsSceneDragMove"
  , "GraphicsSceneDrop"
  , "GraphicsSceneHelp"
  , "GraphicsSceneHoverEnter"
  , "GraphicsSceneHoverLeave"
  , "GraphicsSceneHoverMove"
  , "GraphicsSceneMouseDoubleClick"
  , "GraphicsSceneMouseMove"
  , "GraphicsSceneMousePress"
  , "GraphicsSceneMouseRelease"
  , "GraphicsSceneMove"
  , "GraphicsSceneResize"
  , "GraphicsSceneWheel"
  , "Hide"
  , "HideToParent"
  , "HoverEnter"
  , "HoverLeave"
  , "HoverMove"
  , "IconDrag"
  , "IconTextChange"
  , "InputMethod"
  , "InputMethodQuery"
  , "KeyboardLayoutChange"
  , "KeyPress"
  , "KeyRelease"
  , "LanguageChange"
  , "LayoutDirectionChange"
  , "LayoutRequest"
  , "Leave"
    -- "LeaveEditFocus" is omitted -- it depends on keypadNavigation.
  , "LeaveWhatsThisMode"
  , "LocaleChange"
  , "NonClientAreaMouseButtonDblClick"
  , "NonClientAreaMouseButtonPress"
  , "NonClientAreaMouseButtonRelease"
  , "NonClientAreaMouseMove"
  , "MacSizeChange"
  , "MetaCall"
  , "ModifiedChange"
  , "MouseButtonDblClick"
  , "MouseButtonPress"
  , "MouseButtonRelease"
  , "MouseMove"
  , "MouseTrackingChange"
  , "Move"
  , "NativeGesture"
  , "OrientationChange"
  , "Paint"
  , "PaletteChange"
  , "ParentAboutToChange"
  , "ParentChange"
  , "PlatformPanel"
  , "PlatformSurface"
  , "Polish"
  , "PolishRequest"
  , "QueryWhatsThis"
  , "ReadOnlyChange"
  , "RequestSoftwareInputPanel"
  , "Resize"
  , "ScrollPrepare"
  , "Scroll"
  , "Shortcut"
  , "ShortcutOverride"
  , "Show"
  , "ShowToParent"
  , "SockAct"
  , "StateMachineSignal"
  , "StateMachineWrapped"
  , "StatusTip"
  , "StyleChange"
  , "TabletMove"
  , "TabletPress"
  , "TabletRelease"
  , "OkRequest"
  , "TabletEnterProximity"
  , "TabletLeaveProximity"
  , "ThreadChange"
  , "Timer"
  , "ToolBarChange"
  , "ToolTip"
  , "ToolTipChange"
  , "TouchBegin"
  , "TouchCancel"
  , "TouchEnd"
  , "TouchUpdate"
  , "UngrabKeyboard"
  , "UngrabMouse"
  , "UpdateLater"
  , "UpdateRequest"
  , "WhatsThis"
  , "WhatsThisClicked"
  , "Wheel"
  , "WinEventAct"
  , "WindowActivate"
  , "WindowBlocked"
  , "WindowDeactivate"
  , "WindowIconChange"
  , "WindowStateChange"
  , "WindowTitleChange"
  , "WindowUnblocked"
  , "WinIdChange"
  , "ZOrderChange"

    -- Custom event types:
  , "User"
  , "MaxUser"
  ]
