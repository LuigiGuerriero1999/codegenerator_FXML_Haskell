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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QMessageBox (
  aModule,
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
  mkStaticMethod,
  mkStaticMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (enumT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  e_TextFormat,
  fl_TextInteractionFlags,
  e_WindowModality,
  )
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listenerPtrQAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QCheckBox (c_QCheckBox)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QDialog (c_QDialog)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QPushButton (c_QPushButton)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QMessageBox"]
  [ QtExportClassAndSignals c_QMessageBox signals
  , qtExport e_ButtonRole
  , qtExport e_Icon
  , qtExport e_StandardButton
  , qtExport fl_StandardButtons
  ]

(c_QMessageBox, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QMessageBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QMessageBox") Nothing [c_QDialog] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkStaticMethod "about" [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] voidT
  , just $ mkStaticMethod "aboutQt" [ptrT $ objT c_QWidget, objT c_QString] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "addButton" "addButton"
    [ptrT $ objT c_QAbstractButton, enumT e_ButtonRole] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "addButton" "addNewButton"
    [objT c_QString, enumT e_ButtonRole] $ ptrT $ objT c_QPushButton
  , test (qtVersion >= [4, 2]) $ mkMethod' "addButton" "addStandardButton"
    [enumT e_StandardButton] $ ptrT $ objT c_QPushButton
  , test (qtVersion >= [4, 2]) $ mkConstMethod "button"
    [enumT e_StandardButton] $ ptrT $ objT c_QAbstractButton
  , test (qtVersion >= [4, 5]) $ mkConstMethod "buttonRole"
    [ptrT $ objT c_QAbstractButton] $ enumT e_ButtonRole
  , test (qtVersion >= [4, 5]) $ mkConstMethod "buttons" np $ objT c_QListQAbstractButton
  , test (qtVersion >= [5, 2]) $ mkProp "checkBox" $ ptrT $ objT c_QCheckBox
  , test (qtVersion >= [4, 2]) $ mkConstMethod "clickedButton" np $ ptrT $ objT c_QAbstractButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "critical" "critical"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] $ enumT e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "critical" "criticalWithButtons"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     flagsT fl_StandardButtons, enumT e_StandardButton] $
    enumT e_StandardButton
  , just $ mkConstMethod "defaultButton" np $ ptrT $ objT c_QPushButton
  , test (qtVersion >= [4, 2]) $ mkProp "detailedText" $ objT c_QString
  , test (qtVersion >= [4, 2]) $ mkConstMethod "escapeButton" np $ ptrT $ objT c_QAbstractButton
  , just $ mkProp "icon" $ enumT e_Icon
  , just $ mkProp "iconPixmap" $ objT c_QPixmap
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "information" "information"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] $ enumT e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "information" "informationWithButtons"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     flagsT fl_StandardButtons, enumT e_StandardButton] $
    enumT e_StandardButton
    -- OMIT open
  , test (qtVersion >= [4, 2]) $ mkProp "informativeText" $ objT c_QString
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "question" "question"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] $ enumT e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "question" "questionWithButtons"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     flagsT fl_StandardButtons, enumT e_StandardButton] $
    enumT e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkMethod "removeButton" [ptrT $ objT c_QAbstractButton] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "setDefaultButton" "setDefaultButton"
    [ptrT $ objT c_QPushButton] voidT
  , test (qtVersion >= [4, 3]) $ mkMethod' "setDefaultButton" "setDefaultButtonStandard"
    [enumT e_StandardButton] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "setEscapeButton" "setEscapeButton"
    [ptrT $ objT c_QPushButton] voidT
  , test (qtVersion >= [4, 3]) $ mkMethod' "setEscapeButton" "setEscapeButtonStandard"
    [enumT e_StandardButton] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setWindowModality" [enumT e_WindowModality] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setWindowTitle" [objT c_QString] voidT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "standardButton"
    [ptrT $ objT c_QAbstractButton] $ enumT e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkProp "standardButtons" $ flagsT fl_StandardButtons
  , just $ mkProp "text" $ objT c_QString
  , just $ mkProp "textFormat" $ enumT e_TextFormat
  , test (qtVersion >= [5, 1]) $ mkProp "textInteractionFlags" $ flagsT fl_TextInteractionFlags
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "warning" "warning"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] $ enumT e_StandardButton
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "warning" "warningWithButtons"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     flagsT fl_StandardButtons, enumT e_StandardButton] $
    enumT e_StandardButton
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "buttonClicked" listenerPtrQAbstractButton
  ]

e_ButtonRole =
  makeQtEnum (ident1 "QMessageBox" "ButtonRole") [includeStd "QMessageBox"]
  [ "InvalidRole"
  , "AcceptRole"
  , "RejectRole"
  , "DestructiveRole"
  , "ActionRole"
  , "HelpRole"
  , "YesRole"
  , "NoRole"
  , "ApplyRole"
  , "ResetRole"
  ]

e_Icon =
  makeQtEnum (ident1 "QMessageBox" "Icon") [includeStd "QMessageBox"]
  [ "NoIcon"
  , "Information"
  , "Warning"
  , "Critical"
  , "Question"
  ]

(e_StandardButton, fl_StandardButtons) =
  makeQtEnumAndFlags (ident1 "QMessageBox" "StandardButton") "StandardButtons"
  [includeStd "QMessageBox"]
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
