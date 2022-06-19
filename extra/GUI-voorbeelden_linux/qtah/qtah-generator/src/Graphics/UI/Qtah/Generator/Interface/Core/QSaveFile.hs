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

module Graphics.UI.Qtah.Generator.Interface.Core.QSaveFile (
  aModule,
  c_QSaveFile,
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
import Foreign.Hoppy.Generator.Types (ptrT, boolT, objT, voidT, constT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QFileDevice (c_QFileDevice)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QSaveFile"] [5, 1]
  [qtExport c_QSaveFile]

c_QSaveFile =
  addReqIncludes [includeStd "QSaveFile"] $
  classSetEntityPrefix "" $
  makeClass (ident "QSaveFile") Nothing [c_QFileDevice] $
  collect
  [ just $ mkCtor "newWithNameAndParent" [constT $ objT c_QString, ptrT $ objT c_QObject]
  , just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkCtor "newWithName" [constT $ objT c_QString]
  , just $ mkMethod "cancelWriting" np voidT
  , just $ mkMethod "commit" np boolT
  , just $ mkConstMethod "directWriteFallback" np boolT
  , just $ mkMethod "setDirectWriteFallback" [boolT] voidT
  , just $ mkMethod "setFileName" [constT $ objT c_QString] voidT
  ]
