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

module Graphics.UI.Qtah.Generator.Interface.Core.QResource (
  aModule,
  c_QResource,
  e_Compression,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkStaticMethod',
  mkCtor,
  mkMethod,
  np,
  )

import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QDateTime (c_QDateTime)
import Foreign.Hoppy.Generator.Types (ucharT, boolT, voidT, enumT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qint64)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QResource"] [4, 2] $
  collect
  [ just $ qtExport c_QResource
  , test (qtVersion >= [5, 13]) $ qtExport e_Compression
  ]

c_QResource =
  addReqIncludes [ includeStd "QResource" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QResource") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithFile" [refT $ constT $ objT c_QString]
  --, just $ mkCtor "newWithFileAndLocale" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QLocale]
  , just $ mkConstMethod "absoluteFilePath" np $ objT c_QString
  , test (qtVersion >= [5, 13]) $ mkConstMethod "compressionAlgorithm" np $ enumT e_Compression
  , just $ mkConstMethod' "data" "getData" np $ ptrT $ constT ucharT
  , just $ mkConstMethod "fileName" np $ objT c_QString
  , just $ mkConstMethod "isCompressed" np boolT
  , just $ mkConstMethod "isValid" np boolT
  , test (qtVersion >= [5, 8]) $ mkConstMethod "lastModified" np $ objT c_QDateTime
  --, just $ mkConstMethod "locale" np $ objT c_QLocale
  , just $ mkStaticMethod' "registerResource" "registerResourcePath" [refT $ constT $ objT c_QString ] boolT
  , just $ mkStaticMethod' "registerResource" "registerResourcePathAndTree" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkStaticMethod' "registerResource" "registerResourceData" [ptrT $ constT ucharT] boolT
  , test (qtVersion >= [4, 3]) $ mkStaticMethod' "registerResource" "registerResourceDataAndTree" [ptrT $ constT ucharT, refT $ constT $ objT c_QString] boolT
  , just $ mkMethod "setFileName" [refT $ constT $ objT c_QString] voidT
  -- just $ mkMethod "setLocale" [refT $ constT $ objT c_QLocale] $ voidT
  , just $ mkConstMethod "size" np qint64
  , just $ mkStaticMethod' "unregisterResource" "unregisterResourcePath" [refT $ constT $ objT c_QString ] boolT
  , just $ mkStaticMethod' "unregisterResource" "unregisterResourcePathAndTree" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkStaticMethod' "unregisterResource" "unregisterResourceData" [ptrT $ constT ucharT] boolT
  , test (qtVersion >= [4, 3]) $ mkStaticMethod' "unregisterResource" "unregisterResourceDataAndTree" [ptrT $ constT ucharT, refT $ constT $ objT c_QString] boolT
  ]

e_Compression =
  makeQtEnum (ident1 "QResource" "Compression") [includeStd "QResource"]
  [ "NoCompression"
  , "ZlibCompression"
  , "ZstdCompression"
  ]
