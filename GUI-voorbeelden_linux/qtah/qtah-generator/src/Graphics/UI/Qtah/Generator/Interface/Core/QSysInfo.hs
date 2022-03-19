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

module Graphics.UI.Qtah.Generator.Interface.Core.QSysInfo (
  aModule,
  c_QSysInfo,
  e_Endian,
  e_Sizes,
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
import Foreign.Hoppy.Generator.Types (objT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QSysInfo"] $
  collect
  [ just $ qtExport c_QSysInfo
  , just $ qtExport e_Endian
  , just $ qtExport e_Sizes
  ]

c_QSysInfo =
  addReqIncludes [ includeStd "QSysInfo" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QSysInfo") Nothing [] $
  collect
  [ test (qtVersion >= [5, 11]) $ mkStaticMethod "bootUniqueId" np $ objT c_QByteArray
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "buildAbi" np $ objT c_QString
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "buildCpuArchitecture" np $ objT c_QString
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "currentCpuArchitecture" np $ objT c_QString
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "kernelType" np $ objT c_QString
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "kernelVersion" np $ objT c_QString
  , test (qtVersion >= [5, 6]) $ mkStaticMethod "machineHostName" np $ objT c_QString
  , test (qtVersion >= [5, 11]) $ mkStaticMethod "machineUniqueId" np $ objT c_QByteArray
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "prettyProductName" np $ objT c_QString
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "productType" np $ objT c_QString
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "productVersion" np $ objT c_QString
  ]

e_Sizes =
  makeQtEnum (ident1 "QSysInfo" "Sizes") [includeStd "QSysInfo"]
  [ "WordSize"
  ]

e_Endian =
  makeQtEnum (ident1 "QSysInfo" "Endian") [includeStd "QSysInfo"]
  [ "BigEndian"
  , "LittleEndian"
  , "ByteOrder"
  ]
