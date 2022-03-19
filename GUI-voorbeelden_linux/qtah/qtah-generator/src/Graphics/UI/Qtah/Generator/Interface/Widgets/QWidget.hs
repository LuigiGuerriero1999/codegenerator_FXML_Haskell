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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (
  aModule,
  c_QWidget,
  ) where

import Foreign.Hoppy.Generator.Spec (
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
  mkStaticMethod,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (keypadNavigation, qdoc, qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  e_ContextMenuPolicy,
  e_LayoutDirection,
  fl_WindowFlags,
  e_WindowModality,
  fl_WindowStates,
  e_WindowType,
  qreal,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QPalette (
  c_QPalette,
  e_ColorRole,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QCursor (c_QCursor)
import Graphics.UI.Qtah.Generator.Interface.Gui.QFont (c_QFont)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDevice (c_QPaintDevice)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPainter (c_QPainter)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import Graphics.UI.Qtah.Generator.Interface.Gui.QRegion (c_QRegion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listenerQPoint,
  listenerQString,
  listenerRefConstQIcon,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QSizePolicy (c_QSizePolicy, e_Policy)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QWidget"]
  [ QtExportClassAndSignals c_QWidget signals ]

(c_QWidget, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QWidget"] $
  classSetEntityPrefix "" $
  makeClass (ident "QWidget") Nothing [c_QObject, c_QPaintDevice] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkConstMethod "acceptDrops" np boolT
  , just $ mkConstMethod "accessibleDescription" np $ objT c_QString
  , just $ mkConstMethod "accessibleName" np $ objT c_QString
    -- TODO actions
  , just $ mkMethod "activateWindow" np voidT
  , just $ mkMethod "addAction" [ptrT $ objT c_QAction] voidT
    -- TODO addActions
  , just $ mkMethod "adjustSize" np voidT
  , just $ mkConstMethod "autoFillBackground" np boolT
  , just $ mkProp "backgroundRole" $ enumT e_ColorRole
  , just $ mkConstMethod "baseSize" np $ objT c_QSize
  , just $ mkConstMethod' "childAt" "childAtRaw" [intT, intT] $ ptrT $ objT c_QWidget
  , just $ mkConstMethod' "childAt" "childAtPoint" [objT c_QPoint] $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "childrenRect" np $ objT c_QRect
    -- TODO childrenRegion
  , just $ mkMethod "clearFocus" np voidT
  , just $ mkMethod "clearMask" np voidT
  , just $ mkMethod "close" np boolT
  , just $ mkConstMethod "contentsMargins" np $ objT c_QMargins
  , just $ mkConstMethod "contentsRect" np $ objT c_QRect
  , just $ mkProp "contextMenuPolicy" $ enumT e_ContextMenuPolicy
  , just $ mkProp "cursor" $ objT c_QCursor
    -- TODO effectiveWinId
  , just $ mkConstMethod "ensurePolished" np voidT
    -- TODO find
    -- TODO focusPolicy
  , just $ mkConstMethod "focusProxy" np $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "focusWidget" np $ ptrT $ objT c_QWidget
  , just $ mkProp "font" $ objT c_QFont
    -- TODO fontInfo
    -- TODO fontMetrics
    -- TODO foregroundRole
  , just $ mkConstMethod "frameGeometry" np $ objT c_QRect
  , just $ mkConstMethod "frameSize" np $ objT c_QSize
  , just $ mkConstMethod "geometry" np $ objT c_QRect
  , test (qtVersion >= [5, 0]) $ mkMethod "grab" np $ objT c_QPixmap
  , test (qtVersion >= [5, 0]) $
    mkMethod' "grab" "grabWithRect" [objT c_QRect] $ objT c_QPixmap
    -- TODO grabGesture
  , just $ mkMethod "grabKeyboard" np voidT
  , just $ mkMethod "grabMouse" np voidT
  , just $ mkMethod' "grabMouse" "grabMouseWithCursor" [objT c_QCursor] voidT
    -- TODO grabShortcut
    -- TODO graphicsEffect
    -- TODO graphicsProxyWidget
  , test keypadNavigation $ mkConstMethod "hasEditFocus" np boolT
  , just $ mkConstMethod "hasFocus" np boolT
  , just $ mkConstMethod "hasMouseTracking" np boolT
  , just $ mkConstMethod "height" np intT
  , just $ mkConstMethod "heightForWidth" [intT] intT
  , just $ mkMethod "hide" np voidT
    -- TODO inputContext
    -- TODO inputMethodHints
    -- TODO inputMethodQuery
  , just $ mkMethod "insertAction" [ptrT $ objT c_QAction, ptrT $ objT c_QAction] voidT
    -- TODO insertActions
  , just $ mkConstMethod "isActiveWindow" np boolT
  , just $ mkConstMethod "isAncestorOf" [ptrT $ objT c_QWidget] boolT
  , just $ mkConstMethod "isEnabled" np boolT
  , just $ mkConstMethod "isEnabledTo" [ptrT $ objT c_QWidget] boolT
  , just $ mkConstMethod "isFullScreen" np boolT
  , just $ mkConstMethod "isHidden" np boolT
  , just $ mkConstMethod "isMaximized" np boolT
  , just $ mkConstMethod "isMinimized" np boolT
  , just $ mkConstMethod "isModal" np boolT
  , just $ mkConstMethod "isVisible" np boolT
  , just $ mkConstMethod "isVisibleTo" [ptrT $ objT c_QWidget] boolT
  , just $ mkConstMethod "isWindow" np boolT
  , just $ mkConstMethod "isWindowModified" np boolT
  , just $ mkStaticMethod "keyboardGrabber" np $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "layout" np $ ptrT $ objT c_QLayout
  , just $ mkConstMethod "layoutDirection" np $ enumT e_LayoutDirection
    -- TODO locale
    -- TODO macCGHandle
    -- TODO macQDHandle
  , just $ mkMethod "lower" np voidT
  , just $ mkConstMethod "mapFrom" [ptrT $ objT c_QWidget, objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "mapFromGlobal" [objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "mapFromParent" [objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "mapTo" [ptrT $ objT c_QWidget, objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "mapToGlobal" [objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "mapToParent" [objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "maximumHeight" np intT
  , just $ mkConstMethod "maximumSize" np $ objT c_QSize
  , just $ mkConstMethod "maximumWidth" np intT
  , just $ mkConstMethod "minimumHeight" np intT
  , just $ mkConstMethod "minimumSize" np $ objT c_QSize
  , just $ mkConstMethod "minimumWidth" np intT
  , just $ mkStaticMethod "mouseGrabber" np $ ptrT $ objT c_QWidget
  , just $ mkMethod "move" [objT c_QPoint] voidT
  , just $ mkConstMethod "nativeParentWidget" np $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "nextInFocusChain" np $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "normalGeometry" np $ objT c_QRect
    -- TODO overrideWindowFlags
  , just $ mkProp "palette" $ objT c_QPalette
  , just $ mkConstMethod "parentWidget" np $ ptrT $ objT c_QWidget
    -- TODO platformWindow
    -- TODO platformWindowFormat
  , just $ mkConstMethod "pos" np $ objT c_QPoint
  , just $ mkConstMethod "previousInFocusChain" np $ ptrT $ objT c_QWidget
  , just $ mkMethod "raise" np voidT
  , just $ mkConstMethod "rect" np $ objT c_QRect
  , just $ mkMethod "releaseKeyboard" np voidT
  , just $ mkMethod "releaseMouse" np voidT
    -- TODO releaseShortcut
  , just $ mkMethod "removeAction" [ptrT $ objT c_QAction] voidT
  , test (qtVersion >= [4, 3]) $
    mkMethod' "render" "renderWithTarget" [ptrT $ objT c_QPaintDevice] voidT
  , test (qtVersion >= [4, 3]) $
    mkMethod' "render" "renderWithTargetAndOffset"
      [ptrT $ objT c_QPaintDevice, objT c_QPoint] voidT
  , test (qtVersion >= [4, 3]) $
    mkMethod' "render" "renderWithTargetAndOffsetAndRegion"
      [ptrT $ objT c_QPaintDevice, objT c_QPoint, objT c_QRegion] voidT
    -- TODO [4.3] void render(QPaintDevice *target, const QPoint &targetOffset,
    --      const QRegion &sourceRegion, RenderFlags renderFlags)
  , just $ mkMethod' "render" "renderWithPainter" [ptrT $ objT c_QPainter] voidT
  , just $
    mkMethod' "render" "renderWithPainterAndOffset"
      [ptrT $ objT c_QPainter, objT c_QPoint] voidT
  , just $
    mkMethod' "render" "renderWithPainterAndOffsetAndRegion"
      [ptrT $ objT c_QPainter, objT c_QPoint, objT c_QRegion] voidT
    -- TODO void render(QPainter *painter, const QPoint &targetOffset, const QRegion &sourceRegion,
    --      RenderFlags renderFlags)
  , just $ mkMethod' "repaint" "repaint" np voidT
  , just $ mkMethod' "repaint" "repaintRaw" [intT, intT, intT, intT] voidT
  , just $ mkMethod' "repaint" "repaintRect" [objT c_QRect] voidT
    -- TODO repaint(const QRegion&)
  , just $ mkMethod' "resize" "resize" [objT c_QSize] voidT
  , just $ mkMethod' "resize" "resizeRaw" [intT, intT] voidT
  , just $ mkMethod "restoreGeometry" [objT c_QByteArray] boolT
  , just $ mkConstMethod "saveGeometry" np (objT c_QByteArray)
  , just $ mkMethod' "scroll" "scrollRaw" [intT, intT] voidT
  , just $ mkMethod' "scroll" "scrollRect" [intT, intT, objT c_QRect] voidT
  , just $ mkMethod "setAcceptDrops" [boolT] voidT
  , just $ mkMethod "setAccessibleDescription" [objT c_QString] voidT
  , just $ mkMethod "setAccessibleName" [objT c_QString] voidT
    -- TODO setAttribute
  , just $ mkMethod "setAutoFillBackground" [boolT] voidT
    -- TODO setBackgroundRole
  , just $ mkMethod' "setBaseSize" "setBaseSize" [objT c_QSize] voidT
  , just $ mkMethod' "setBaseSize" "setBaseSizeRaw" [intT, intT] voidT
  , just $ mkMethod' "setContentsMargins" "setContentsMargins" [objT c_QMargins] voidT
  , just $ mkMethod' "setContentsMargins" "setContentsMarginsRaw" [intT, intT, intT, intT] voidT
    -- TODO setContextMenuPolicy
  , just $ mkMethod "setEnabled" [boolT] voidT
  , just $ mkMethod "setDisabled" [boolT] voidT
  , test keypadNavigation $ mkMethod "setEditFocus" [boolT] voidT
  , just $ mkMethod "setFixedHeight" [intT] voidT
  , just $ mkMethod' "setFixedSize" "setFixedSize" [objT c_QSize] voidT
  , just $ mkMethod' "setFixedSize" "setFixedSizeRaw" [intT, intT] voidT
  , just $ mkMethod "setFixedWidth" [intT] voidT
  , just $ mkMethod "setFocus" np voidT
    -- TODO setFocus(Qt::FocusReason)
    -- TODO setFocusPolicy
  , just $ mkMethod "setFocusProxy" [ptrT $ objT c_QWidget] voidT
    -- TODO setForegroundRole
  , just $ mkMethod' "setGeometry" "setGeometryRaw" [intT, intT, intT, intT] voidT
  , just $ mkMethod' "setGeometry" "setGeometryRect" [objT c_QRect] voidT
    -- TODO setGraphicsEffect
  , just $ mkMethod "setHidden" [boolT] voidT
    -- TODO setInputContext
    -- TODO setInputMethodHints
  , just $ mkMethod "setLayout" [ptrT $ objT c_QLayout] voidT
  , just $ mkMethod "setLayoutDirection" [enumT e_LayoutDirection] voidT
    -- TODO setLocale
    -- TODO setMask
  , just $ mkMethod "setMaximumHeight" [intT] voidT
  , just $ mkMethod' "setMaximumSize" "setMaximumSize" [objT c_QSize] voidT
  , just $ mkMethod' "setMaximumSize" "setMaximumSizeRaw" [intT, intT] voidT
  , just $ mkMethod "setMaximumWidth" [intT] voidT
  , just $ mkMethod "setMinimumHeight" [intT] voidT
  , just $ mkMethod' "setMinimumSize" "setMinimumSize" [objT c_QSize] voidT
  , just $ mkMethod' "setMinimumSize" "setMinimumSizeRaw" [intT, intT] voidT
  , just $ mkMethod "setMinimumWidth" [intT] voidT
  , just $ mkMethod "setMouseTracking" [boolT] voidT
    -- TODO setPalette
  , just $ mkMethod' "setParent" "setParent" [ptrT $ objT c_QWidget] voidT
  , just $ mkMethod' "setParent" "setParentWithFlags"
    [ptrT $ objT c_QWidget, flagsT fl_WindowFlags] voidT
    -- TODO setPlatformWindow
    -- TODO setPlatformWindowFormat
    -- TODO setShortcutAutoRepeat
    -- TODO setShortcutEnabled
  , just $ mkMethod' "setSizeIncrement" "setSizeIncrement" [objT c_QSize] voidT
  , just $ mkMethod' "setSizeIncrement" "setSizeIncrementRaw" [intT, intT] voidT
  , just $ mkMethod' "setSizePolicy" "setSizePolicyRaw" [enumT e_Policy, enumT e_Policy] voidT
  , just $ mkMethod "setStatusTip" [objT c_QString] voidT
    -- TODO setStyle
  , just $ mkMethod "setStyleSheet" [objT c_QString] voidT
  , just $ mkStaticMethod "setTabOrder" [ptrT $ objT c_QWidget, ptrT $ objT c_QWidget] voidT
  , just $ mkMethod "setToolTip" [objT c_QString] voidT
  , just $ mkMethod "setUpdatesEnabled" [boolT] voidT
  , just $ mkMethod "setVisible" [boolT] voidT
  , just $ mkMethod "setWhatsThis" [objT c_QString] voidT
  , just $ mkMethod "setWindowFilePath" [objT c_QString] voidT
  , just $ mkMethod "setWindowIconText" [objT c_QString] voidT
  , just $ mkMethod "setWindowModified" [boolT] voidT
  , just $ mkMethod "setWindowRole" [objT c_QString] voidT
    -- TODO setWindowSurface
  , test qdoc $ mkMethod "setupUi" [ptrT $ objT c_QWidget] voidT
  , just $ mkMethod "show" np voidT
  , just $ mkMethod "showFullScreen" np voidT
  , just $ mkMethod "showMaximized" np voidT
  , just $ mkMethod "showMinimized" np voidT
  , just $ mkMethod "showNormal" np voidT
  , just $ mkConstMethod "size" np $ objT c_QSize
  , just $ mkConstMethod "sizeHint" np $ objT c_QSize
  , just $ mkConstMethod "sizeIncrement" np $ objT c_QSize
  , just $ mkProp "sizePolicy" $ objT c_QSizePolicy
  , just $ mkMethod "stackUnder" [ptrT $ objT c_QWidget] voidT
  , just $ mkConstMethod "statusTip" np $ objT c_QString
  , just $ mkConstMethod "styleSheet" np $ objT c_QString
    -- TODO testAttribute
  , just $ mkConstMethod "toolTip" np $ objT c_QString
  , just $ mkConstMethod "underMouse" np boolT
    -- TODO ungrabGesture
  , just $ mkMethod "unsetCursor" np voidT
  , just $ mkMethod "unsetLayoutDirection" np voidT
  , just $ mkMethod "unsetLocale" np voidT
  , just $ mkMethod' "update" "update" np voidT
  , just $ mkMethod' "update" "updateRaw" [intT, intT, intT, intT] voidT
  , just $ mkMethod' "update" "updateRect" [objT c_QRect] voidT
    -- TODO update(const QRegion&)
  , just $ mkMethod "updateGeometry" np voidT
  , just $ mkConstMethod "updatesEnabled" np boolT
    -- TODO visibleRegion
  , just $ mkConstMethod "whatsThis" np $ objT c_QString
  , just $ mkConstMethod "width" np intT
  , just $ mkConstMethod "window" np $ ptrT $ objT c_QWidget
  , just $ mkConstMethod "windowFilePath" np $ objT c_QString
  , just $ mkProp "windowFlags" $ flagsT fl_WindowFlags
  , just $ mkProp "windowIcon" $ objT c_QIcon
  , -- DEPRECATED by 5.7.
    just $ mkConstMethod "windowIconText" np $ objT c_QString
  , just $ mkProp "windowModality" $ enumT e_WindowModality
  , just $ mkProp "windowOpacity" qreal
  , just $ mkConstMethod "windowRole" np $ objT c_QString
    -- TODO windowSurface
  , just $ mkProp "windowState" $ flagsT fl_WindowStates
  , just $ mkProp "windowTitle" $ objT c_QString
  , test (qtVersion < [5, 0]) $ mkConstMethod "windowType" np $ enumT e_WindowType
    -- TODO winId
  , just $ mkConstMethod "x" np intT
    -- TODO x11Info
    -- TODO x11PictureHandle
  , just $ mkConstMethod "y" np intT
  ]

signalGens :: [SignalGen]
signalGens =
  collect
  [ just $ makeSignal "customContextMenuRequested" listenerQPoint
  , test (qtVersion >= [5, 2]) $ makeSignal "windowIconChanged" listenerRefConstQIcon
    -- TODO windowIconTextChanged (>=5.0?  Deprecated by 5.7.)
  , test (qtVersion >= [5, 2]) $ makeSignal "windowTitleChanged" listenerQString
  ]
