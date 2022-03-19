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

module Graphics.UI.Qtah.Generator.Interface.Gui.QFileOpenEvent (
  aModule,
  c_QFileOpenEvent,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  np,
  )
import Foreign.Hoppy.Generator.Types (refT, objT, boolT)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QFile (c_QFile)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (fl_OpenMode)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QFileOpenEvent"]
  [ QtExportEvent c_QFileOpenEvent ]

c_QFileOpenEvent =
  addReqIncludes [includeStd "QFileOpenEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QFileOpenEvent") Nothing [c_QEvent]
  [ mkConstMethod "file" np $ objT c_QString
  , mkConstMethod "openFile" [refT $ objT c_QFile, flagsT fl_OpenMode] boolT
  -- TODO QUrl QFileOpenEvent::url() const
  ]
