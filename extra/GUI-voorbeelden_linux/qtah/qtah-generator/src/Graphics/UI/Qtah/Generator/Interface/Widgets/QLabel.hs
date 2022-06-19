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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QLabel (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolHasProp,
  mkConstMethod,
  mkCtor,
  mkMethod',
  mkMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  constT,
  doubleT,
  enumT,
  intT,
  objT,
  ptrT,
  voidT,
  )
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  fl_Alignment,
  e_TextFormat,
  fl_TextInteractionFlags,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listenerQString)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QFrame (c_QFrame)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QLabel"]
  [ QtExportClassAndSignals c_QLabel signals ]

(c_QLabel, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QLabel"] $
  classSetEntityPrefix "" $
  makeClass (ident "QLabel") Nothing [c_QFrame]
  [ mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithText" [objT c_QString]
  , mkCtor "newWithTextAndParent" [objT c_QString, ptrT $ objT c_QWidget]
    -- TODO Ctors taking Qt::WindowFlags.
  , mkProp "alignment" $ flagsT fl_Alignment
  , mkProp "buddy" $ ptrT $ objT c_QWidget
  , mkMethod "clear" np voidT
  , mkConstMethod "hasSelectedText" np boolT
  , mkProp "indent" intT
  , mkProp "margin" intT
    -- TODO movie
  , mkProp "openExternalLinks" boolT
    -- TODO picture
  , mkConstMethod "pixmap" np $ ptrT $ constT $ objT c_QPixmap
  , mkBoolHasProp "scaledContents"
  , mkConstMethod "selectedText" np $ objT c_QString
  , mkConstMethod "selectionStart" np intT
    -- TODO mkProp "movie" $ ptrT $ objT c_QMovie
  , mkMethod' "setNum" "setInt" [intT] voidT
  , mkMethod' "setNum" "setDouble" [doubleT] voidT
  , mkMethod "setPixmap" [objT c_QPixmap] voidT
  , mkMethod "setSelection" [intT, intT] voidT
  , mkProp "text" $ objT c_QString
  , mkProp "textFormat" $ enumT e_TextFormat
  , mkProp "textInteractionFlags" $ flagsT fl_TextInteractionFlags
  , mkProp "wordWrap" boolT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "linkActivated" listenerQString
  , makeSignal "linkHovered" listenerQString
  ]
