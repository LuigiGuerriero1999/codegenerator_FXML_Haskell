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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QLineEdit (
  aModule,
  e_EchoMode,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolHasProp,
  mkBoolIsProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, constT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_Alignment, e_CursorMoveStyle)
import Graphics.UI.Qtah.Generator.Interface.Gui.QValidator (c_QValidator)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerIntInt,
  listenerQString,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QLineEdit"] $
  [ QtExportClassAndSignals c_QLineEdit signals
  , qtExport e_EchoMode
  ]

(c_QLineEdit, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QLineEdit"] $
  classSetEntityPrefix "" $
  makeClass (ident "QLineEdit") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithText" [objT c_QString]
  , just $ mkCtor "newWithTextAndParent" [objT c_QString, ptrT $ objT c_QWidget]
  , just $ mkProp "alignment" $ flagsT fl_Alignment
  , just $ mkMethod "backspace" np voidT
  , just $ mkMethod "clear" np voidT
  , test (qtVersion >= [5, 2]) $ mkBoolIsProp "clearButtonEnabled"
    -- TODO completer
  , just $ mkConstMethod "copy" np voidT
  , just $ mkMethod "createStandardContextMenu" np $ ptrT $ objT c_QMenu
  , just $ mkMethod "cursorBackward" [boolT, intT] voidT
  , just $ mkMethod "cursorForward" [boolT, intT] voidT
  , just $ mkProp "cursorMoveStyle" $ enumT e_CursorMoveStyle
  , just $ mkProp "cursorPosition" intT
  , just $ mkMethod "cursorPositionAt" [objT c_QPoint] intT
  , just $ mkMethod "cursorWordBackward" [boolT] voidT
  , just $ mkMethod "cursorWordForward" [boolT] voidT
  , just $ mkMethod "cut" np voidT
  , just $ mkMethod "del" np voidT
  , just $ mkMethod "deselect" np voidT
  , just $ mkConstMethod "displayText" np $ objT c_QString
  , just $ mkProp "dragEnabled" boolT
  , just $ mkProp "echoMode" $ enumT e_EchoMode
  , just $ mkMethod "end" [boolT] voidT
  , just $ mkBoolHasProp "frame"
  , just $ mkConstMethod "hasAcceptableInput" np boolT
  , just $ mkConstMethod "hasSelectedText" np boolT
  , just $ mkMethod "home" [boolT] voidT
  , just $ mkProp "inputMask" $ objT c_QString
  , just $ mkMethod "insert" [objT c_QString] voidT
  , just $ mkConstMethod "isRedoAvailable" np boolT
  , just $ mkConstMethod "isUndoAvailable" np boolT
  , just $ mkProp "maxLength" intT
  , just $ mkBoolIsProp "modified"
  , just $ mkMethod "paste" np voidT
  , just $ mkProp "placeholderText" $ objT c_QString
  , just $ mkBoolIsProp "readOnly"
  , just $ mkMethod "redo" np voidT
  , just $ mkMethod "selectAll" np voidT
  , just $ mkConstMethod "selectedText" np $ objT c_QString
  , just $ mkConstMethod "selectionStart" np intT
  , just $ mkMethod "setSelection" [intT, intT] voidT
  , just $ mkMethod' "setTextMargins" "setTextMargins" [objT c_QMargins] voidT
  , just $ mkMethod' "setTextMargins" "setTextMarginsRaw" [intT, intT, intT, intT] voidT
  , just $ mkProp "text" $ objT c_QString
  , just $ mkConstMethod "textMargins" np $ objT c_QMargins
  , just $ mkMethod "undo" np voidT
  , just $ mkProp "validator" $ ptrT $ constT $ objT c_QValidator
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "cursorPositionChanged" listenerIntInt
  , makeSignal "editingFinished" listener
  , makeSignal "returnPressed" listener
  , makeSignal "selectionChanged" listener
  , makeSignal "textEdited" listenerQString
  , makeSignal "textChanged" listenerQString
  ]

e_EchoMode =
  makeQtEnum (ident1 "QLineEdit" "EchoMode") [includeStd "QLineEdit"]
  [ "Normal"
  , "NoEcho"
  , "Password"
  , "PasswordEchoOnEdit"
  ]
