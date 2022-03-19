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

module Graphics.UI.Qtah.Generator.Interface.Core.QFileSelector (
  aModule,
  c_QFileSelector,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  np,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Foreign.Hoppy.Generator.Types (voidT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QFileSelector"] [5, 2] $
  [qtExport c_QFileSelector]

c_QFileSelector =
  addReqIncludes [ includeStd "QFileSelector" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QFileSelector") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkConstMethod "allSelectors" np $ objT c_QStringList
  , just $ mkConstMethod "extraSelectors" np $ objT c_QStringList
  , just $ mkConstMethod "select" [refT $ constT $ objT c_QString] $ objT c_QString
  -- TODO QUrl QFileSelector::select(const QUrl &filePath) const
  , just $ mkMethod "setExtraSelectors" [refT $ constT $ objT c_QStringList] voidT
  ]
