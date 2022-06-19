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

module Graphics.UI.Qtah.Generator.Interface.Core.QLockFile (
  aModule,
  c_QLockFile,
  e_LockError,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod',
  mkMethod,
  np,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Foreign.Hoppy.Generator.Types (boolT, intT, voidT, enumT, constT, objT, ptrT, refT, llongT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QLockFile"] [5, 1] $
  collect
  [ just $ qtExport c_QLockFile
  , just $ qtExport e_LockError
  ]

c_QLockFile =
  addReqIncludes [ includeStd "QLockFile" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QLockFile") Nothing [] $
  collect
  [ just $ mkCtor "new" [refT $ constT $ objT c_QString]
  , just $ mkConstMethod "error" np $ enumT e_LockError
  , -- TODO Return a tuple instead.
    just $ mkConstMethod "getLockInfo" [ptrT $ llongT, ptrT $ objT c_QString, ptrT $ objT c_QString] boolT
  , just $ mkConstMethod "isLocked" np boolT
  , just $ mkMethod "lock" np boolT
  , just $ mkMethod "removeStaleLockFile" np boolT
  , just $ mkMethod "setStaleLockTime" [intT] voidT
  , just $ mkConstMethod "staleLockTime" np intT
  , just $ mkMethod' "tryLock" "tryLock" np boolT
  , just $ mkMethod' "tryLock" "tryLockWithTimeout" [intT] boolT
  , just $ mkMethod "unlock" np voidT
  ]

e_LockError =
  makeQtEnum (ident1 "QLockFile" "LockError") [includeStd "QLockFile"]
  [ "NoError"
  , "LockFailedError"
  , "PermissionError"
  , "UnknownError"
  ]
