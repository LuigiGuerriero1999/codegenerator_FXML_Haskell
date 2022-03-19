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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QApplication (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  MethodApplicability (MStatic),
  Purity (Nonpure),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  ident2,
  includeLocal,
  includeStd,
  makeFnMethod,
  makeClass,
  mkConstMethod,
  mkProp,
  mkStaticMethod,
  mkStaticMethod',
  mkStaticProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (keypadNavigation, qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QCoreApplication (c_QCoreApplication)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQWidget)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_LayoutDirection, e_NavigationMode)
import Graphics.UI.Qtah.Generator.Interface.Gui.QClipboard (c_QClipboard)
import Graphics.UI.Qtah.Generator.Interface.Gui.QFont (c_QFont)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerPtrQWidgetPtrQWidget,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QApplication"] $
  collect
  [ just $ QtExportClassAndSignals c_QApplication signals
  , test (qtVersion < [5]) $ qtExport e_Type
  ]

(c_QApplication, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [ includeStd "QApplication"
                 , includeLocal "wrap_qapplication.hpp"
                 ] $
  classSetEntityPrefix "" $
  makeClass (ident "QApplication") Nothing [c_QCoreApplication] $
  collect
  [ just $ makeFnMethod (ident2 "qtah" "qapplication" "create") "new" MStatic Nonpure
    [objT c_QStringList] $ ptrT $ objT c_QApplication
  , just $ mkStaticMethod "aboutQt" np voidT
  , just $ mkStaticMethod "activeModalWidget" np $ ptrT $ objT c_QWidget
  , just $ mkStaticMethod "activePopupWidget" np $ ptrT $ objT c_QWidget
  , just $ mkStaticProp "activeWindow" $ ptrT $ objT c_QWidget
  , just $ mkStaticMethod "alert" [ptrT $ objT c_QWidget, intT] voidT
  , just $ mkStaticMethod "allWidgets" np $ objT c_QListQWidget
  , just $ mkProp "autoSipEnabled" boolT
  , just $ mkStaticMethod "beep" np voidT
    -- TODO changeOverrideCursor
  , just $ mkStaticMethod "clipboard" np $ ptrT $ objT c_QClipboard
  , just $ mkStaticMethod "closeAllWindows" np voidT
  , just $ mkStaticProp "colorSpec" intT
    -- TODO commitData
  , just $ mkStaticProp "cursorFlashTime" intT
    -- TODO desktop
  , just $ mkStaticProp "desktopSettingsAware" boolT
  , just $ mkStaticProp "doubleClickInterval" intT
  , just $ mkStaticMethod "focusWidget" np $ ptrT $ objT c_QWidget
  , just $ mkStaticMethod' "font" "font" np $ objT c_QFont
  , just $ mkStaticMethod' "font" "fontWithWidget" [ptrT $ objT c_QWidget] $ objT c_QFont
  , just $ makeFnMethod (ident2 "qtah" "qapplication" "fontWithClass") "fontWithClass"
    MStatic Nonpure [objT c_QString] $ objT c_QFont
    -- TODO fontMetrics
  , just $ mkStaticProp "globalStrut" $ objT c_QSize
    -- TODO inputContext
    -- TODO isEffectEnabled
  , just $ mkStaticMethod "isLeftToRight" np boolT
  , just $ mkStaticMethod "isRightToLeft" np boolT
  , just $ mkConstMethod "isSessionRestored" np boolT
  , test (qtVersion < [5]) $ mkStaticMethod "keyboardInputDirection" np $ enumT e_LayoutDirection
  , just $ mkStaticMethod "keyboardInputInterval" np intT
    -- TODO keyboardInputLocale (<5)
    -- TODO keyboardModifiers
  , just $ mkStaticMethod "layoutDirection" np $ enumT e_LayoutDirection
    -- TODO macEventFilter
    -- TODO mouseButtons
  , test keypadNavigation $ mkStaticMethod "navigationMode" np $ enumT e_NavigationMode
    -- TODO overrideCursor
    -- TODO palette
    -- TODO queryKeyboardModifiers
  , just $ mkStaticMethod "quitOnLastWindowClosed" np boolT
    -- TODO qwsDecoration
    -- TODO qwsEventFilter
    -- TODO qwsSetCustomColors
    -- TODO qwsSetDecoration
  , just $ mkStaticMethod "restoreOverrideCursor" np voidT
    -- TODO saveState
  , just $ mkConstMethod "sessionId" np $ objT c_QString
  , just $ mkConstMethod "sessionKey" np $ objT c_QString
    -- TODO setEffectEnabled
  , just $ mkStaticMethod' "setFont" "setFont" [objT c_QFont] voidT
  , just $ makeFnMethod (ident2 "qtah" "qapplication" "setFontWithClass") "setFontWithClass"
    MStatic Nonpure [objT c_QFont, objT c_QString] voidT
    -- TODO setFont
    -- TODO setGraphicsSystem (<5)
    -- TODO setInputContext
  , just $ mkStaticMethod "setKeyboardInputInterval" [intT] voidT
  , just $ mkStaticMethod "setLayoutDirection" [enumT e_LayoutDirection] voidT
  , test keypadNavigation $ mkStaticMethod "setNavigationMode" [enumT e_NavigationMode] voidT
    -- TODO setOverrideCursor
    -- TODO setPalette
  , just $ mkStaticMethod "setQuitOnLastWindowClosed" [boolT] voidT
    -- TODO setStyle
  , just $ mkProp "startDragDistance" intT
  , just $ mkProp "startDragTime" intT
    -- TODO style
  , just $ mkProp "styleSheet" $ objT c_QString
  , test (qtVersion < [5]) $ mkStaticMethod "syncX" np voidT
    -- TODO symbianEventFilter
    -- TODO symbianProcessEvent
  , just $ mkStaticMethod' "topLevelAt" "topLevelAtPoint" [objT c_QPoint] $ ptrT $ objT c_QWidget
  , just $ mkStaticMethod' "topLevelAt" "topLevelAtRaw" [intT, intT] $ ptrT $ objT c_QWidget
    -- TODO topLevelWidgets
    -- We rename type() since @type@ is a Haskell keyword.
  , test (qtVersion < [5]) $ mkStaticMethod' "type" "applicationType" np $ enumT e_Type
  , just $ mkStaticProp "wheelScrollLines" intT
  , just $ mkStaticMethod' "widgetAt" "widgetAtPoint" [objT c_QPoint] $ ptrT $ objT c_QWidget
  , just $ mkStaticMethod' "widgetAt" "widgetAtRaw" [intT, intT] $ ptrT $ objT c_QWidget
  , just $ mkProp "windowIcon" $ objT c_QIcon
    -- TODO x11EventFilter
    -- TODO x11ProcessEvent
  ]

signalGens :: [SignalGen]
signalGens =
  [ -- TODO commitDataRequest
    makeSignal "focusChanged" listenerPtrQWidgetPtrQWidget
  , makeSignal "fontDatabaseChanged" listener
  , makeSignal "lastWindowClosed" listener
    -- TODO quit (static!)
    -- TODO saveStateRequest
  ]

-- | Removed in Qt 5.
e_Type =
  makeQtEnum (ident1 "QApplication" "Type") [includeStd "QApplication"]
  [ "Tty"
  , "GuiClient"
  , "GuiServer"
  ]
