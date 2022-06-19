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

module Graphics.UI.Qtah.Generator.Interface.Core.QFileDevice (
  aModule,
  c_QFileDevice,
  e_FileError,
  e_FileHandleFlag,
  fl_FileHandleFlags,
  e_FileTime,
  e_MemoryMapFlags,
  e_Permission,
  fl_Permissions,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  mkMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (ucharT, boolT, intT, objT, voidT, enumT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QDateTime (c_QDateTime)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qlonglong)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QFileDevice"] [5, 0] $
  collect $
  [ just $ qtExport c_QFileDevice
  , test (qtVersion >= [5, 10]) $ qtExport e_FileTime
  , test (qtVersion >= [4, 4]) $ qtExport e_MemoryMapFlags
  , just $ qtExport e_FileError
  , just $ qtExport e_FileHandleFlag
  , just $ qtExport fl_FileHandleFlags
  , just $ qtExport e_Permission
  , just $ qtExport fl_Permissions
  ]

c_QFileDevice =
  addReqIncludes [includeStd "QFileDevice"] $
  classSetEntityPrefix "" $
  makeClass (ident "QFileDevice") Nothing [c_QIODevice] $
  collect
  [ just $ mkConstMethod "error" np $ enumT e_FileError
  , just $ mkConstMethod "fileName" np $ objT c_QString
  , test (qtVersion >= [5, 10]) $ mkConstMethod "fileTime" [enumT e_FileTime] $ objT c_QDateTime
  , just $ mkMethod "flush" np boolT
  , just $ mkConstMethod "handle" np intT
  , just $ mkMethod' "map" "map" [qlonglong, qlonglong] $ ptrT ucharT
  , just $ mkMethod' "map" "mapWithFlags" [qlonglong, qlonglong, enumT e_MemoryMapFlags] $ ptrT ucharT
  , just $ mkConstMethod "permissions" np $ flagsT fl_Permissions
  , just $ mkMethod "resize" [qlonglong] boolT
  , test (qtVersion >= [5, 10]) $
    mkMethod "setFileTime" [objT c_QDateTime, enumT e_FileTime] boolT
  , just $ mkMethod "setPermissions" [flagsT fl_Permissions] boolT
  , just $ mkMethod "unmap" [ptrT ucharT] boolT
  , just $ mkMethod "unsetError" np voidT
  ]

e_FileError =
  makeQtEnum (ident1 "QFileDevice" "FileError") [includeStd "QFileDevice"]
  [ "NoError"
  , "ReadError"
  , "WriteError"
  , "FatalError"
  , "ResourceError"
  , "OpenError"
  , "AbortError"
  , "TimeOutError"
  , "UnspecifiedError"
  , "RemoveError"
  , "RenameError"
  , "PositionError"
  , "ResizeError"
  , "PermissionsError"
  , "CopyError"
  ]

(e_FileHandleFlag, fl_FileHandleFlags) =
  makeQtEnumAndFlags (ident1 "QFileDevice" "FileHandleFlag") "FileHandleFlags" [includeStd "QFileDevice"]
  [ "DontCloseHandle"
  , "AutoCloseHandle"
  ]

e_FileTime =
  makeQtEnum (ident1 "QFileDevice" "FileTime") [includeStd "QFileDevice"]
  [ "FileAccessTime"
  , "FileBirthTime"
  , "FileMetadataChangeTime"
  , "FileModificationTime"
  ]

e_MemoryMapFlags =
  makeQtEnum (ident1 "QFileDevice" "MemoryMapFlags") [includeStd "QFileDevice"] $
  collect
  [ just "NoOptions"
  , test (qtVersion >= [5, 4]) "MapPrivateOption"
  ]

(e_Permission, fl_Permissions) =
  makeQtEnumAndFlags (ident1 "QFileDevice" "Permission") "Permissions" [includeStd "QFileDevice"]
  [ "ReadOwner"
  , "WriteOwner"
  , "ExeOwner"
  , "ReadUser"
  , "WriteUser"
  , "ExeUser"
  , "ReadGroup"
  , "WriteGroup"
  , "ExeGroup"
  , "ReadOther"
  , "WriteOther"
  , "ExeOther"
  ]
