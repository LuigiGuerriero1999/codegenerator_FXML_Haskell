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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QSystemTrayIcon (
  aModule,
  c_QSystemTrayIcon,
  e_ActivationReason,
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
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkStaticMethod,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerQSystemTrayIconActivationReason,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Widgets", "QSystemTrayIcon"] [4, 2]
  [ QtExportClassAndSignals c_QSystemTrayIcon signals
  , qtExport e_ActivationReason
  , qtExport e_MessageIcon
  ]

(c_QSystemTrayIcon, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QSystemTrayIcon"] $
  classSetEntityPrefix "" $
  makeClass (ident "QSystemTrayIcon") Nothing [c_QObject]
  [ mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , mkCtor "newWithIcon" [objT c_QIcon]
  , mkCtor "newWithIconAndParent" [objT c_QIcon, ptrT $ objT c_QObject]
  , mkProp "contextMenu" $ ptrT $ objT c_QMenu
  , mkConstMethod "geometry" np $ objT c_QRect
  , mkMethod "hide" np voidT
  , mkProp "icon" $ objT c_QIcon
  , mkStaticMethod "isSystemTrayAvailable" np boolT
  , mkMethod "show" np voidT
  , mkMethod' "showMessage" "showMessage" [objT c_QString, objT c_QString] voidT
  , mkMethod' "showMessage" "showMessageAll"
    [objT c_QString, objT c_QString, enumT e_MessageIcon, intT] voidT
  , mkStaticMethod "supportsMessages" np boolT
  , mkProp "toolTip" $ objT c_QString
  , mkBoolIsProp "visible"
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "activated" listenerQSystemTrayIconActivationReason
  , makeSignal "messageClicked" listener
  ]

e_ActivationReason =
  makeQtEnum (ident1 "QSystemTrayIcon" "ActivationReason") [includeStd "QSystemTrayIcon"]
  [ "Unknown"
  , "Context"
  , "DoubleClick"
  , "Trigger"
  , "MiddleClick"
  ]

e_MessageIcon =
  makeQtEnum (ident1 "QSystemTrayIcon" "MessageIcon") [includeStd "QSystemTrayIcon"]
  [ "NoIcon"
  , "Information"
  , "Warning"
  , "Critical"
  ]
