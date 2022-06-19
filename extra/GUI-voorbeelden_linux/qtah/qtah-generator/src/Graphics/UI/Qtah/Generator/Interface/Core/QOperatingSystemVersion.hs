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

module Graphics.UI.Qtah.Generator.Interface.Core.QOperatingSystemVersion (
  aModule,
  c_QOperatingSystemVersion,
  e_OSType,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkStaticMethod,
  mkCtor,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
    ClassFeature (Copyable),
    classAddFeatures,
    )
import Foreign.Hoppy.Generator.Types (intT, enumT, objT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QOperatingSystemVersion"] [5, 9] $
  collect
  [ just $ qtExport c_QOperatingSystemVersion
  , just $ qtExport e_OSType
  ]

c_QOperatingSystemVersion =
  addReqIncludes [ includeStd "QOperatingSystemVersion" ] $
  classSetConversionToGc $
  classAddFeatures [Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QOperatingSystemVersion") Nothing [] $
  collect
  [ just $ mkCtor "new" [enumT e_OSType, intT]
  , just $ mkCtor "newWithMinor" [enumT e_OSType, intT, intT]
  , just $ mkCtor "newWithMinorAndMicro" [enumT e_OSType, intT, intT, intT]
  , just $ mkStaticMethod "current" np $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 10]) $
    mkStaticMethod "currentType" np $ enumT e_OSType
  -- TODO bool QOperatingSystemVersion::isAnyOfType(std::initializer_list<OSType> types) const
  , just $ mkConstMethod "majorVersion" np intT
  , just $ mkConstMethod "microVersion" np intT
  , just $ mkConstMethod "minorVersion" np intT
  , just $ mkConstMethod "name" np $ objT c_QString
  , just $ mkConstMethod "segmentCount" np intT
  , just $ mkConstMethod' "type" "getType" np $ enumT e_OSType
  ]

e_OSType =
  makeQtEnum (ident1 "QOperatingSystemVersion" "OSType") [includeStd "QOperatingSystemVersion"]
  [ "Unknown"
  , "Windows"
  , "MacOS"
  , "IOS"
  , "TvOS"
  , "WatchOS"
  , "Android"
  ]
