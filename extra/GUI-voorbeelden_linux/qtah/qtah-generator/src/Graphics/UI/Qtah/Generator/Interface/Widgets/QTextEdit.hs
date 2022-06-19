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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QTextEdit (
  aModule,
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
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_Alignment, qreal)
import Graphics.UI.Qtah.Generator.Interface.Gui.QColor (c_QColor)
import Graphics.UI.Qtah.Generator.Interface.Gui.QFont (c_QFont)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerBool,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractScrollArea (c_QAbstractScrollArea)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTextEdit"] $
  [ QtExportClassAndSignals c_QTextEdit signals
  , qtExport e_LineWrapMode
  , qtExport e_AutoFormattingFlag
  , qtExport fl_AutoFormatting
  ]

(c_QTextEdit, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QTextEdit"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTextEdit") Nothing [c_QAbstractScrollArea]
  [ mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithText" [objT c_QString]
  , mkCtor "newWithTextAndParent" [objT c_QString, ptrT $ objT c_QWidget]
  , mkProp "acceptRichText" boolT
  , mkProp "alignment" $ flagsT fl_Alignment
  , mkConstMethod "anchorAt" [objT c_QPoint] $ objT c_QString
  , mkMethod "append" [objT c_QString] voidT
  , mkProp "autoFormatting" $ flagsT fl_AutoFormatting
  , mkConstMethod "canPaste" np boolT
  , mkMethod "clear" np voidT
  , mkMethod "copy" np voidT
  , mkMethod' "createStandardContextMenu" "createStandardContextMenu" np $ ptrT $ objT c_QMenu
  , mkMethod' "createStandardContextMenu" "createStandardContextMenuAt" [objT c_QPoint] $
    ptrT $ objT c_QMenu
    -- TODO currentCharFormat
  , mkProp "currentFont" $ objT c_QFont
    -- TODO cursorForPosition
  , mkConstMethod' "cursorRect" "cursorRect" np $ objT c_QRect
    -- TODO cursorRect(const QTextCursor&)
  , mkProp "cursorWidth" intT
  , mkMethod "cut" np voidT
    -- TODO document
  , mkProp "documentTitle" $ objT c_QString
  , mkMethod "ensureCursorVisible" np voidT
    -- TODO extraSelections
  , mkMethod' "find" "find" [objT c_QString] boolT
    -- TODO find with FindFlags
  , mkProp "fontFamily" $ objT c_QString
  , mkProp "fontItalic" boolT
  , mkProp "fontPointSize" qreal
  , mkProp "fontUnderline" boolT
  , mkProp "fontWeight" intT
  , mkMethod "insertHtml" [objT c_QString] voidT
  , mkMethod "insertPlainText" [objT c_QString] voidT
  , mkProp "lineWrapColumnOrWidth" intT
  , mkProp "lineWrapMode" $ enumT e_LineWrapMode
    -- TODO loadResource
    -- TODO mergeCurrentCharFormat
    -- TODO moveCursor
  , mkProp "overwriteMode" boolT
  , mkMethod "paste" np voidT
    -- TODO print
  , mkBoolIsProp "readOnly"
  , mkMethod "redo" np voidT
  , mkMethod "scrollToAnchor" [objT c_QString] voidT
  , mkMethod "selectAll" np voidT
  , mkMethod "setHtml" [objT c_QString] voidT
  , mkMethod "setPlainText" [objT c_QString] voidT
  , mkMethod "setText" [objT c_QString] voidT
  , mkProp "tabChangesFocus" boolT
  , mkProp "tabStopWidth" intT
  , mkProp "textBackgroundColor" $ objT c_QColor
  , mkProp "textColor" $ objT c_QColor
    -- TODO textCursor
    -- TODO textInteractionFlags
  , mkConstMethod "toHtml" np $ objT c_QString
  , mkConstMethod "toPlainText" np $ objT c_QString
  , mkMethod "undo" np voidT
  , mkBoolIsProp "undoRedoEnabled"
    -- TODO wordWrapMode
  , mkMethod "zoomIn" np voidT
  , mkMethod' "zoomIn" "zoomInPoints" [intT] voidT
  , mkMethod "zoomOut" np voidT
  , mkMethod' "zoomOut" "zoomOutPoints" [intT] voidT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "copyAvailable" listenerBool
    -- TODO currentCharFormatChanged
  , makeSignal "cursorPositionChanged" listener
  , makeSignal "redoAvailable" listenerBool
  , makeSignal "selectionChanged" listener
  , makeSignal "textChanged" listener
  , makeSignal "undoAvailable" listenerBool
  ]

e_LineWrapMode =
  makeQtEnum (ident1 "QTextEdit" "LineWrapMode") [includeStd "QTextEdit"]
  [ "NoWrap"
  , "WidgetWidth"
  , "FixedPixelWidth"
  , "FixedColumnWidth"
  ]

(e_AutoFormattingFlag, fl_AutoFormatting) =
  makeQtEnumAndFlags (ident1 "QTextEdit" "AutoFormattingFlag") "AutoFormatting" [includeStd "QTextEdit"]
  [ "AutoNone"
  , "AutoBulletList"
  , "AutoAll"
  ]
