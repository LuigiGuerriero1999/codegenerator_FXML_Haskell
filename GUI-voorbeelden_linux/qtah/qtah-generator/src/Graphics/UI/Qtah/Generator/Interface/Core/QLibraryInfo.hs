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

module Graphics.UI.Qtah.Generator.Interface.Core.QLibraryInfo (
  aModule,
  c_QLibraryInfo,
  e_LibraryLocation,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkStaticMethod,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QVersionNumber (c_QVersionNumber)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QLibraryInfo"] $
  collect
  [ just $ qtExport c_QLibraryInfo
  , just $ qtExport e_LibraryLocation
  ]

c_QLibraryInfo =
  addReqIncludes [ includeStd "QLibraryInfo" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QLibraryInfo") Nothing [] $
  collect
  [ test (qtVersion >= [5, 0]) $ mkStaticMethod "isDebugBuild" np $ boolT
  , just $ mkStaticMethod "location" [enumT e_LibraryLocation] $ objT c_QString
  , test (qtVersion >= [5, 8]) $ mkStaticMethod "version" np $ objT c_QVersionNumber
  ]

e_LibraryLocation =
  makeQtEnum (ident1 "QLibraryInfo" "LibraryLocation") [includeStd "QLibraryInfo"]
  [ "PrefixPath"
  , "DocumentationPath"
  , "HeadersPath"
  , "LibrariesPath"
  , "LibraryExecutablesPath"
  , "BinariesPath"
  , "PluginsPath"
  , "ImportsPath"
  , "Qml2ImportsPath"
  , "ArchDataPath"
  , "DataPath"
  , "TranslationsPath"
  , "ExamplesPath"
  , "TestsPath"
  , "SettingsPath"
  ]
