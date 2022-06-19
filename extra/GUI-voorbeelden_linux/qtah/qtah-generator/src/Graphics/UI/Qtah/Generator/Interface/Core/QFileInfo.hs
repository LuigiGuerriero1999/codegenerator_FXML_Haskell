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

module Graphics.UI.Qtah.Generator.Interface.Core.QFileInfo (
  aModule,
  c_QFileInfo,
  ) where

import Foreign.Hoppy.Generator.Spec (
  classSetConversionToGc,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkStaticMethod',
  mkCtor,
  mkMethod',
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable, Assignable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, uintT, voidT, enumT, constT, objT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QDir (c_QDir)
import Graphics.UI.Qtah.Generator.Interface.Core.QFile (c_QFile)
import Graphics.UI.Qtah.Generator.Interface.Core.QFileDevice (e_FileTime, fl_Permissions)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QDateTime (c_QDateTime)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qint64)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QFileInfo"] $
  [qtExport c_QFileInfo]

c_QFileInfo =
  addReqIncludes [ includeStd "QFileInfo" ] $
  classSetConversionToGc $
  classAddFeatures [Copyable, Assignable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QFileInfo") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithString" [refT $ constT $ objT c_QString]
  , just $ mkCtor "newWithFile" [refT $ constT $ objT c_QFile]
  , just $ mkCtor "newWithDirAndString" [refT $ constT $ objT c_QDir, refT $ constT $ objT c_QString]
  , just $ mkConstMethod "absoluteDir" np $ objT c_QDir
  , just $ mkConstMethod "absoluteFilePath" np $ objT c_QString
  , just $ mkConstMethod "absolutePath" np $ objT c_QString
  , just $ mkConstMethod "baseName" np $ objT c_QString
  , test (qtVersion >= [5, 10]) $ mkConstMethod "birthTime" np $ objT c_QDateTime
  , test (qtVersion >= [4, 3]) $ mkConstMethod "bundleName" np $ objT c_QString
  , just $ mkConstMethod "caching" np $ boolT
  , just $ mkConstMethod "canonicalFilePath" np $ objT c_QString
  , just $ mkConstMethod "canonicalPath" np $ objT c_QString
  , just $ mkConstMethod "completeBaseName" np $ objT c_QString
  , just $ mkConstMethod "completeSuffix" np $ objT c_QString
  , just $ mkConstMethod "dir" np $ objT c_QDir
  , just $ mkConstMethod' "exists" "exists" np boolT
  , test (qtVersion >= [5, 2]) $ mkStaticMethod' "exists" "existsStatic" [refT $ constT $ objT c_QString] boolT
  , just $ mkConstMethod "fileName" np $ objT c_QString
  , just $ mkConstMethod "filePath" np $ objT c_QString
  , test (qtVersion >= [5, 10]) $ mkConstMethod "fileTime" [enumT e_FileTime] $ objT c_QDateTime
  , just $ mkConstMethod "group" np $ objT c_QString
  , just $ mkConstMethod "groupId" np uintT
  , just $ mkConstMethod "isAbsolute" np boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod "isBundle" np boolT
  , just $ mkConstMethod "isDir" np boolT
  , just $ mkConstMethod "isExecutable" np boolT
  , just $ mkConstMethod "isFile" np boolT
  , just $ mkConstMethod "isHidden" np boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "isNativePath" np boolT
  , just $ mkConstMethod "isReadable" np boolT
  , just $ mkConstMethod "isRelative" np boolT
  , just $ mkConstMethod "isRoot" np boolT
  , just $ mkConstMethod "isSymLink" np boolT
  , just $ mkConstMethod "isWritable" np boolT
  , just $ mkConstMethod "lastModified" np $ objT c_QDateTime
  , just $ mkConstMethod "lastRead" np $ objT c_QDateTime
  , just $ mkMethod "makeAbsolute" np boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "metadataChangeTime" np $ objT c_QDateTime
  , just $ mkConstMethod "owner" np $ objT c_QString
  , just $ mkConstMethod "ownerId" np uintT
  , just $ mkConstMethod "path" np $ objT c_QString
  , just $ mkConstMethod "permission" [flagsT fl_Permissions] boolT
  , just $ mkConstMethod "permissions" np $ flagsT fl_Permissions
  , just $ mkMethod "refresh" np voidT
  , just $ mkMethod "setCaching" [boolT] voidT
  , just $ mkMethod' "setFile" "setFileString" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "setFile" "setFile" [refT $ constT $ objT c_QFile] voidT
  , just $ mkMethod' "setFile" "setFileDirAndString" [refT $ constT $ objT c_QDir, refT $ constT $ objT c_QString] voidT
  , just $ mkConstMethod "size" np $ qint64
  , just $ mkConstMethod "suffix" np $ objT c_QString
  , test (qtVersion >= [5, 10]) $ mkMethod "swap" [refT $ objT c_QFileInfo] voidT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "symLinkTarget" np $ objT c_QString
  ]
