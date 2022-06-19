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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QDialogButtonBox (
  aModule,
  c_QDialogButtonBox,
  e_ButtonLayout,
  e_ButtonRole,
  e_StandardButton,
  fl_StandardButtons,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_Orientation)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerPtrQAbstractButton,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QPushButton (c_QPushButton)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [4, 2]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Widgets", "QDialogButtonBox"] minVersion
  [ QtExportClassAndSignals c_QDialogButtonBox signals
  , qtExport e_ButtonLayout
  , qtExport e_ButtonRole
  , qtExport e_StandardButton
  , qtExport fl_StandardButtons
  ]

(c_QDialogButtonBox, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QDialogButtonBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QDialogButtonBox") Nothing [c_QWidget]
  [ mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
    -- TODO Other ctors? v v
  --, mkCtor "newWithOrientation" [enumT e_Orientation]
  --, mkCtor "newWithOrientationAndParent" [enumT e_Orientation, ptrT $ objT c_QWidget]
  --, mkCtor "newWithButtons" np
  --, mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  --, mkCtor "new" np
  --, mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkMethod' "addButton" "addButton" [ptrT $ objT c_QAbstractButton, enumT e_ButtonRole] voidT
  , mkMethod' "addButton" "addButtonWithText"
    [objT c_QString, enumT e_ButtonRole] $ ptrT $ objT c_QPushButton
  , mkMethod' "addButton" "addStandardButton" [enumT e_StandardButton] $ ptrT $ objT c_QPushButton
  , mkConstMethod "button" [enumT e_StandardButton] $ ptrT $ objT c_QPushButton
  , mkConstMethod "buttonRole" [ptrT $ objT c_QAbstractButton] $ enumT e_ButtonRole
  , mkConstMethod "buttons" np $ objT c_QListQAbstractButton
  , mkProp "centerButtons" boolT
  , mkMethod "clear" np voidT
  , mkProp "orientation" $ enumT e_Orientation
  , mkMethod "removeButton" [ptrT $ objT c_QAbstractButton] voidT
  , mkConstMethod "standardButton" [ptrT $ objT c_QAbstractButton] $ enumT e_StandardButton
  , mkProp "standardButtons" $ flagsT fl_StandardButtons
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "accepted" listener
  , makeSignal "clicked" listenerPtrQAbstractButton
  , makeSignal "helpRequested" listener
  , makeSignal "rejected" listener
  ]

e_ButtonLayout =
  makeQtEnum (ident1 "QDialogButtonBox" "ButtonLayout") [includeStd "QDialogButtonBox"]
  [ "WinLayout"
  , "MacLayout"
  , "KdeLayout"
  , "GnomeLayout"
  ]

e_ButtonRole =
  makeQtEnum (ident1 "QDialogButtonBox" "ButtonRole") [includeStd "QDialogButtonBox"]
  [ "InvalidRole"
  , "AcceptRole"
  , "RejectRole"
  , "DestructiveRole"
  , "ActionRole"
  , "HelpRole"
  , "YesRole"
  , "NoRole"
  , "ResetRole"
  , "ApplyRole"
  ]

(e_StandardButton, fl_StandardButtons) =
  makeQtEnumAndFlags (ident1 "QDialogButtonBox" "StandardButton") "StandardButtons"
  [includeStd "QDialogButtonBox"]
  [ "Ok"
  , "Open"
  , "Save"
  , "Cancel"
  , "Close"
  , "Discard"
  , "Apply"
  , "Reset"
  , "RestoreDefaults"
  , "Help"
  , "SaveAll"
  , "Yes"
  , "YesToAll"
  , "No"
  , "NoToAll"
  , "Abort"
  , "Retry"
  , "Ignore"
  , "NoButton"
  ]
