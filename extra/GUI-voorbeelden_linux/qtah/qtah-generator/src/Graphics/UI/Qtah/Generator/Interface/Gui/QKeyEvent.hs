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

module Graphics.UI.Qtah.Generator.Interface.Gui.QKeyEvent (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ushortT, word32T)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (e_Type)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_KeyboardModifiers)
import Graphics.UI.Qtah.Generator.Interface.Gui.QInputEvent (c_QInputEvent)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QKeyEvent"]
  [ QtExportEvent c_QKeyEvent
  ]

c_QKeyEvent =
  addReqIncludes [includeStd "QKeyEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QKeyEvent") Nothing [c_QInputEvent] $
  collect
  [ just $ mkCtor "new" [enumT e_Type, intT, flagsT fl_KeyboardModifiers]
  , just $ mkCtor "newWithText"
    [enumT e_Type, intT, flagsT fl_KeyboardModifiers, objT c_QString, boolT, ushortT]
  , test (qtVersion >= [5, 0]) $ mkCtor "newNative"
    [enumT e_Type, intT, flagsT fl_KeyboardModifiers, word32T, word32T, word32T]
  , test (qtVersion >= [5, 0]) $ mkCtor "newNativeWithText"
    [enumT e_Type, intT, flagsT fl_KeyboardModifiers, word32T, word32T, word32T,
     objT c_QString, boolT, ushortT]
  , just $ mkConstMethod "count" np intT
  , just $ mkConstMethod "isAutoRepeat" np boolT
  , just $ mkConstMethod "key" np intT
    -- TODO matches (>=4.2)
  , just $ mkConstMethod "modifiers" np $ flagsT fl_KeyboardModifiers
  , test (qtVersion >= [4, 2]) $ mkConstMethod "nativeModifiers" np word32T
  , test (qtVersion >= [4, 2]) $ mkConstMethod "nativeScanCode" np word32T
  , test (qtVersion >= [4, 2]) $ mkConstMethod "nativeVirtualKey" np word32T
  , just $ mkConstMethod "text" np $ objT c_QString
  ]
