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

module Graphics.UI.Qtah.Generator.Interface.Core.QStaticPlugin (
  aModule,
  c_QStaticPlugin,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkMethod',
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Types (charT, constT, objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
--import Graphics.UI.Qtah.Generator.Interface.Core.QJsonObject (c_QJsonObject)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QStaticPlugin"] [5, 2] $
  [qtExport c_QStaticPlugin]

c_QStaticPlugin =
  addReqIncludes [ includeStd "QStaticPlugin" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QStaticPlugin") Nothing [] $
  collect
  [ just $ mkMethod' "instance" "getInstance" np $ ptrT $ objT c_QObject
  --, just $ mkConstMethod "metaData" np $ objT c_QJsonObject
  , just $ mkMethod "rawMetaData" np $ ptrT $ constT charT
  ]
