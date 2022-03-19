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

module Graphics.UI.Qtah.Generator.Interface.Gui.QDragEnterEvent (
  aModule,
  c_QDragEnterEvent,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Types (constT, refT, objT, ptrT)
import Graphics.UI.Qtah.Generator.Interface.Gui.QDragMoveEvent (c_QDragMoveEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QMimeData (c_QMimeData)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_KeyboardModifiers, fl_DropActions, fl_MouseButtons)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QDragEnterEvent"]
  [ QtExportEvent c_QDragEnterEvent ]

c_QDragEnterEvent =
  addReqIncludes [includeStd "QDragEnterEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QDragEnterEvent") Nothing [c_QDragMoveEvent]
  [ mkCtor "new" [refT $ constT $ objT c_QPoint, flagsT fl_DropActions, ptrT $ constT $ objT c_QMimeData, flagsT fl_MouseButtons, flagsT fl_KeyboardModifiers ]
  ]
