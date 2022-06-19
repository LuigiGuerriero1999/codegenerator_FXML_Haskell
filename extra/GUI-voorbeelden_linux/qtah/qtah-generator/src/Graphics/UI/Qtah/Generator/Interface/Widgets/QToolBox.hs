-- This file is part of Qtah.
--
-- Copyright 2018-2021 The Qtah Authors.
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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QToolBox (
  aModule,
  c_QToolBox,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Class,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_WindowFlags)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listenerInt)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QFrame (c_QFrame)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Widgets", "QToolBox"]
  [ QtExportClassAndSignals c_QToolBox signals ]

c_QToolBox :: Class
(c_QToolBox, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QToolBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QToolBox") Nothing [c_QFrame]
  [
  -- Properties
    mkConstMethod "count" np intT
  , mkConstMethod "currentIndex" np intT
  -- Public Functions
  , mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor
      "newWithParentAndFlags" [ptrT $ objT c_QWidget, flagsT fl_WindowFlags]
  , mkMethod'
      "addItem"
      "addItemWithIcon"
      [ptrT $ objT c_QWidget, objT c_QIcon, objT c_QString]
      intT
  , mkMethod "addItem" [ptrT $ objT c_QWidget, objT c_QString] intT
  , mkConstMethod "currentWidget" np (ptrT $ objT c_QWidget)
  , mkConstMethod "indexOf" [ptrT $ objT c_QWidget] intT
  , mkMethod'
      "insertItem"
      "insertItemWithIcon"
      [intT, ptrT $ objT c_QWidget, objT c_QIcon, objT c_QString]
      intT
  , mkMethod "insertItem" [intT, ptrT $ objT c_QWidget, objT c_QString] intT
  , mkConstMethod "isItemEnabled" [intT] boolT
  , mkConstMethod "itemIcon" [intT] (objT c_QIcon)
  , mkConstMethod "itemText" [intT] (objT c_QString)
  , mkConstMethod "itemToolTip" [intT] (objT c_QString)
  , mkMethod "removeItem" [intT] voidT
  , mkMethod "setItemEnabled" [intT, boolT] voidT
  , mkMethod "setItemIcon" [intT, objT c_QIcon] voidT
  , mkMethod "setItemText" [intT, objT c_QString] voidT
  , mkMethod "setItemToolTip" [intT, objT c_QString] voidT
  , mkConstMethod "widget" [intT] (ptrT $ objT c_QWidget)
  -- Public Slots
  , mkMethod "setCurrentIndex" [intT] voidT
  , mkMethod "setCurrentWidget" [ptrT $ objT c_QWidget] voidT
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "currentChanged" listenerInt
  ]
