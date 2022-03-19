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

module Graphics.UI.Qtah.Generator.Interface.Core.QLatin1Char (
  aModule,
  c_QLatin1Char,
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
import Foreign.Hoppy.Generator.Types (charT, ushortT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QLatin1Char"] $
  [qtExport c_QLatin1Char]

c_QLatin1Char =
  addReqIncludes [ includeStd "QLatin1Char" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QLatin1Char") Nothing [] $
  collect
  [ just $ mkCtor "new" [charT]
  , just $ mkConstMethod "toLatin1" np charT
  , just $ mkConstMethod "unicode" np ushortT
  ]
