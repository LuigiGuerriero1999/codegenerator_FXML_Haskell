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

module Graphics.UI.Qtah.Generator.Interface.Core.QFile (
  aModule,
  c_QFile,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod',
  mkStaticMethod,
  mkStaticMethod',
  mkCtor,
  mkMethod',
  mkMethod,
  np,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QFileDevice (c_QFileDevice, fl_FileHandleFlags, fl_Permissions)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (fl_OpenMode)
import Foreign.Hoppy.Generator.Types (boolT, intT, voidT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qint64)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QFile"] $
  [qtExport c_QFile]

c_QFile =
  addReqIncludes [ includeStd "QFile" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QFile") Nothing [c_QFileDevice] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkCtor "newWithName" [refT $ objT c_QString]
  , just $ mkCtor "newWithNameAndParent" [refT $ constT $ objT c_QString, ptrT $ objT c_QObject]
  , just $ mkMethod' "copy" "copy" [refT $ constT $ objT c_QString] boolT
  , just $ mkStaticMethod' "copy" "copyStatic" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] boolT
  , just $ mkStaticMethod "decodeName" [refT $ constT $ objT c_QByteArray] $ objT c_QString
  , just $ mkStaticMethod "encodeName" [refT $ constT $ objT c_QString] $ objT c_QByteArray
  , just $ mkConstMethod' "exists" "exists" np boolT
  , just $ mkStaticMethod' "exists" "existsStatic" [refT $ constT $ objT c_QString] boolT
  , just $ mkMethod' "link" "link" [refT $ constT $ objT c_QString] boolT
  , just $ mkStaticMethod' "link" "linkStatic" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] boolT
  -- TODO bool QFile::open(FILE *fh, QIODevice::OpenMode mode, QFileDevice::FileHandleFlags handleFlags = DontCloseHandle)
  , just $ mkMethod' "open" "openWithFileDesc" [intT, flagsT fl_OpenMode ] boolT
  , just $ mkMethod' "open" "openWithFileDescAndFlags" [intT, flagsT fl_OpenMode, flagsT fl_FileHandleFlags] boolT
  , just $ mkStaticMethod' "permissions" "permissionsStatic" [refT $ constT $ objT c_QString] $ flagsT fl_Permissions
  , just $ mkMethod' "remove" "remove" np boolT
  , just $ mkStaticMethod' "remove" "removeStatic" [refT $ constT $ objT c_QString] boolT
  , just $ mkMethod' "rename" "rename" [refT $ constT $ objT c_QString] boolT
  , just $ mkStaticMethod' "rename" "renameStatic" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] boolT
  , just $ mkStaticMethod' "resize" "resizeStatic" [refT $ constT $ objT c_QString, qint64] boolT
  , just $ mkMethod "setFileName" [refT $ constT $ objT c_QString] voidT
  , just $ mkStaticMethod' "setPermissions" "setPermissionsStatic" [refT $ constT $ objT c_QString, flagsT fl_Permissions] boolT
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "symLinkTarget" "symLinkTargetStatic" [refT $ constT $ objT c_QString] $ objT c_QString
  , test (qtVersion >= [4, 2]) $ mkConstMethod' "symLinkTarget" "symLinkTarget" np $ objT c_QString
  ]
