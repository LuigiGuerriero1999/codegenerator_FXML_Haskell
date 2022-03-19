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

module Graphics.UI.Qtah.Generator.Interface.Gui.QWindow (
  minVersion,
  aModule,
  c_QWindow,
  e_Visibility,
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
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  e_ScreenOrientation,
  fl_WindowFlags,
  e_WindowModality,
  e_WindowState,
  e_WindowType,
  qreal,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QCursor (c_QCursor)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Gui.QRegion (c_QRegion)
import Graphics.UI.Qtah.Generator.Interface.Gui.QSurface (c_QSurface, e_SurfaceType)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerBool,
  listenerInt,
  listenerPtrQObject,
  listenerQreal,
  listenerQString,
  listenerQWindowVisibility,
  listenerScreenOrientation,
  listenerWindowModality,
  listenerWindowState,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [5, 0]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Gui", "QWindow"] minVersion $
  [ QtExportClassAndSignals c_QWindow signals
  , qtExport e_AncestorMode
  , qtExport e_Visibility
  ]

(c_QWindow, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QWindow"] $
  classSetEntityPrefix "" $
  makeClass (ident "QWindow") Nothing [c_QObject, c_QSurface] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWindow]
    -- TODO mkCtor "newWithScreen" [ptrT $ objT c_QScreen]
  , test (qtVersion >= [5, 1]) $ mkMethod "alert" [intT] voidT
  , just $ mkProp "baseSize" $ objT c_QSize
  , just $ mkMethod "close" np voidT
  , just $ mkConstMethod "contentOrientation" np $ enumT e_ScreenOrientation
  , just $ mkMethod "create" np voidT
  , just $ mkProp "cursor" $ objT c_QCursor
  , just $ mkMethod "destroy" np voidT
  , just $ mkConstMethod "devicePixelRatio" np qreal
  , just $ mkProp "filePath" $ objT c_QString
  , just $ mkProp "flags" $ flagsT fl_WindowFlags
  , just $ mkConstMethod "focusObject" np $ ptrT $ objT c_QObject
  , just $ mkConstMethod "frameGeometry" np $ objT c_QRect
  , just $ mkConstMethod "frameMargins" np $ objT c_QMargins
  , just $ mkProp "framePosition" $ objT c_QPoint
  , just $ mkProp "geometry" $ objT c_QRect
  , just $ mkProp "height" intT
  , just $ mkMethod "hide" np voidT
  , just $ mkProp "icon" $ objT c_QIcon
  , test (qtVersion >= [5, 1]) $ mkConstMethod "isActive" np boolT
  , just $ mkConstMethod "isAncestorOf" [ptrT $ objT c_QWindow, enumT e_AncestorMode] boolT
  , just $ mkConstMethod "isExposed" np boolT
  , just $ mkConstMethod "isModal" np boolT
  , just $ mkConstMethod "isTopLevel" np boolT
  , just $ mkMethod "lower" np voidT
  , just $ mkConstMethod "mapFromGlobal" [objT c_QPoint] $ objT c_QPoint
  , just $ mkConstMethod "mapToGlobal" [objT c_QPoint] $ objT c_QPoint
  , just $ mkProp "mask" $ objT c_QRegion
  , just $ mkProp "maximumHeight" intT
  , just $ mkProp "maximumSize" $ objT c_QSize
  , just $ mkProp "maximumWidth" intT
  , just $ mkProp "minimumHeight" intT
  , just $ mkProp "minimumSize" $ objT c_QSize
  , just $ mkProp "minimumWidth" intT
  , just $ mkProp "modality" $ enumT e_WindowModality
  , test (qtVersion >= [5, 1]) $ mkProp "opacity" qreal
  , just $ mkProp "parent" $ ptrT $ objT c_QWindow
  , just $ mkProp "position" $ objT c_QPoint
  , just $ mkMethod "raise" np voidT
  , just $ mkMethod "reportContentOrientationChange" [enumT e_ScreenOrientation] voidT
  , just $ mkMethod "requestActivate" np voidT
  , test (qtVersion >= [5, 5]) $ mkMethod "requestUpdate" np voidT
    -- TODO mkConstMethod "requestedFormat" np $ objT c_QSurfaceFormat
  , just $ mkMethod' "resize" "resize" [objT c_QSize] voidT
  , just $ mkMethod' "resize" "resizeRaw" [intT, intT] voidT
    -- TODO mkProp "screen" $ ptrT $ objT c_QScreen
    -- TODO mkMethod "setFormat" [objT c_QSurfaceFormat] voidT
  , just $ mkMethod' "setGeometry" "setGeometryRaw" [intT, intT, intT, intT] voidT
  , just $ mkMethod "setKeyboardGrabEnabled" [boolT] voidT
  , just $ mkMethod "setMouseGrabEnabled" [boolT] voidT
  , just $ mkMethod' "setPosition" "setPositionRaw" [intT, intT] voidT
  , just $ mkMethod "setSurfaceType" [enumT e_SurfaceType] voidT
  , just $ mkMethod "show" np voidT
  , just $ mkMethod "showFullScreen" np voidT
  , just $ mkMethod "showMaximized" np voidT
  , just $ mkMethod "showMinimized" np voidT
  , just $ mkMethod "showNormal" np voidT
  , just $ mkProp "sizeIncrement" $ objT c_QSize
  , just $ mkProp "title" $ objT c_QString
  , just $ mkProp "transientParent" $ ptrT $ objT c_QWindow
  , just $ mkConstMethod' "type" "getType" np $ enumT e_WindowType
  , just $ mkMethod "unsetCursor" np voidT
  , test (qtVersion >= [5, 1]) $ mkProp "visibility" $ enumT e_Visibility
  , just $ mkBoolIsProp "visible"
  , just $ mkProp "width" intT
    -- TODO winId
  , just $ mkProp "windowState" $ enumT e_WindowState
  , just $ mkProp "x" intT
  , just $ mkProp "y" intT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "activeChanged" listener
  , makeSignal "contentOrientationChanged" listenerScreenOrientation
  , makeSignal "focusObjectChanged" listenerPtrQObject
  , makeSignal "heightChanged" listenerInt
  , makeSignal "maximumHeightChanged" listenerInt
  , makeSignal "maximumWidthChanged" listenerInt
  , makeSignal "minimumHeightChanged" listenerInt
  , makeSignal "minimumWidthChanged" listenerInt
  , makeSignal "modalityChanged" listenerWindowModality
  , makeSignal "opacityChanged" listenerQreal
    -- TODO makeSignal "screenChanged" listenerPtrQScreen
  , makeSignal "visibilityChanged" listenerQWindowVisibility
  , makeSignal "visibleChanged" listenerBool
  , makeSignal "widthChanged" listenerInt
  , makeSignal "windowStateChanged" listenerWindowState
  , makeSignal "windowTitleChanged" listenerQString
  , makeSignal "xChanged" listenerInt
  , makeSignal "yChanged" listenerInt
  ]

e_AncestorMode =
  makeQtEnum (ident1 "QWindow" "AncestorMode") [includeStd "QWindow"]
  [ "ExcludeTransients"
  , "IncludeTransients"
  ]

e_Visibility =
  makeQtEnum (ident1 "QWindow" "Visibility") [includeStd "QWindow"]
  [ "Hidden"
  , "AutomaticVisibility"
  , "Windowed"
  , "Minimized"
  , "Maximized"
  , "FullScreen"
  ]
