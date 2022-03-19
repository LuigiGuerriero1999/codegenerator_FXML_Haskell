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

module Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamEntityResolver (
  aModule,
  c_QXmlStreamEntityResolver,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkMethod,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Foreign.Hoppy.Generator.Types (constT, objT, refT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QXmlStreamEntityResolver"] [4, 4] $
  [qtExport c_QXmlStreamEntityResolver]

c_QXmlStreamEntityResolver =
  addReqIncludes [ includeStd "QXmlStreamEntityResolver" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QXmlStreamEntityResolver") Nothing [] $
  collect
  [ just $ mkMethod "resolveUndeclaredEntity" [refT $ constT $ objT c_QString] $ objT c_QString
  ]
