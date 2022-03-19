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

module Graphics.UI.Qtah.Generator.Interface.Core.QStandardPaths (
  aModule,
  c_QStandardPaths,
  e_StandardLocation,
  e_LocateOption,
  fl_LocateOptions,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  classSetDtorPrivate,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkStaticMethod,
  mkStaticMethod',
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Foreign.Hoppy.Generator.Types (boolT, voidT, enumT, constT, objT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QStandardPaths"] [5, 0] $
  collect
  [ just $ qtExport c_QStandardPaths
  , just $ qtExport e_StandardLocation
  , just $ qtExport e_LocateOption
  , just $ qtExport fl_LocateOptions
  ]

c_QStandardPaths =
  addReqIncludes [ includeStd "QStandardPaths" ] $
  classSetEntityPrefix "" $
  classSetDtorPrivate $
  makeClass (ident "QStandardPaths") Nothing [] $
  collect
  [just $ mkStaticMethod "displayName" [enumT e_StandardLocation] $ objT c_QString
  , just $ mkStaticMethod' "findExecutable" "findExecutable" [constT $ objT c_QString ] $ objT c_QString
  , just $ mkStaticMethod' "findExecutable" "findExecutableWithPaths" [constT $ objT c_QString, constT $ objT c_QStringList] $ objT c_QString
  , just $ mkStaticMethod' "locate" "locate" [enumT e_StandardLocation, constT $ objT c_QString ] $ objT c_QString
  , just $ mkStaticMethod' "locate" "locateWithOptions" [enumT e_StandardLocation, constT $ objT c_QString, flagsT fl_LocateOptions] $ objT c_QString
  , just $ mkStaticMethod' "locateAll" "locateAll" [enumT e_StandardLocation, constT $ objT c_QString ] $ objT c_QStringList
  , just $ mkStaticMethod' "locateAll" "locateAllWithOptions" [enumT e_StandardLocation, constT $ objT c_QString, flagsT fl_LocateOptions] $ objT c_QStringList
  , just $ mkStaticMethod "setTestModeEnabled" [boolT] voidT
  , just $ mkStaticMethod "standardLocations" [enumT e_StandardLocation] $ objT c_QStringList
  , just $ mkStaticMethod "writableLocation" [enumT e_StandardLocation] $ objT c_QString
  ]

e_StandardLocation =
  makeQtEnum (ident1 "QStandardPaths" "StandardLocation") [includeStd "QStandardPaths"] $
  collect
  [ just "DesktopLocation"
  , just "DocumentsLocation"
  , just "FontsLocation"
  , just "ApplicationsLocation"
  , just "MusicLocation"
  , just "MoviesLocation"
  , just "PicturesLocation"
  , just "TempLocation"
  , just "HomeLocation"
  , just "DataLocation"
  , just "CacheLocation"
  , just "GenericDataLocation"
  , just "RuntimeLocation"
  , just "ConfigLocation"
  , just "DownloadLocation"
  , just "GenericCacheLocation"
  , just "GenericConfigLocation"
  , test (qtVersion >= [5, 4]) $ "AppDataLocation"
  , test (qtVersion >= [5, 5]) $ "AppConfigLocation"
  ]

(e_LocateOption, fl_LocateOptions) =
  makeQtEnumAndFlags (ident1 "QStandardPaths" "LocateOption") "LocateOptions" [includeStd "QStandardPaths"]
  [ "LocateFile"
  , "LocateDirectory"
  ]
