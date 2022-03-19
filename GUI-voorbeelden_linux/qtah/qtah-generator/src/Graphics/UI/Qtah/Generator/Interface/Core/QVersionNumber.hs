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

module Graphics.UI.Qtah.Generator.Interface.Core.QVersionNumber (
  aModule,
  c_QVersionNumber,
  ) where

import Foreign.Hoppy.Generator.Spec (
  classSetConversionToGc,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkStaticMethod,
  mkStaticMethod',
  mkCtor,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Comparable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (toGcT, boolT, intT, constT, objT, refT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorInt)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QVersionNumber"] [5, 6] $
  [qtExport c_QVersionNumber]

c_QVersionNumber =
  addReqIncludes [ includeStd "QVersionNumber" ] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Comparable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QVersionNumber") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithVector" [refT $ objT c_QVectorInt]
  , just $ mkCtor "newWithMajor" [intT]
  , just $ mkCtor "newWithMajorMinor" [intT, intT]
  , just $ mkCtor "newWithMajorMinorMicro" [intT, intT, intT]
  , just $ mkStaticMethod "commonPrefix" [refT $ constT $ objT c_QVersionNumber, refT $ constT $ objT c_QVersionNumber] $ objT c_QVersionNumber
  , just $ mkStaticMethod "compare" [refT $ constT $ objT c_QVersionNumber, refT $ constT $ objT c_QVersionNumber] intT
  , just $ mkStaticMethod' "fromString" "fromString" [refT $ constT $ objT c_QString] $ objT c_QVersionNumber
    -- TODO Use a pair to return the suffix index:
  --, just $ mkStaticMethod' "fromString" "fromStringWithSuffIndex" [refT $ constT $ objT c_QString, ptrT intT] $ objT c_QVersionNumber
  --, test (qtVersion >= [5, 10]) $ mkStaticMethod' "fromString" "fromStringWithQLatin" [objT c_QLatin1String] $ objT c_QVersionNumber
  --, test (qtVersion >= [5, 10]) $ mkStaticMethod' "fromString" "fromStringWithQLatinSuffIndex" [objT c_QLatin1String, ptrT intT] $ objT c_QVersionNumber
  --, test (qtVersion >= [5, 10]) $ mkStaticMethod' "fromString" "fromStringWithQStringView" [objT c_QStringView] $ objT c_QVersionNumber
  --, test (qtVersion >= [5, 10]) $ mkStaticMethod' "fromString" "fromStringWithQStringViewSuffIndex" [objT c_QStringView, ptrT intT] $ objT c_QVersionNumber
  , just $ mkConstMethod "isNormalized" np boolT
  , just $ mkConstMethod "isNull" np boolT
  , just $ mkConstMethod "isPrefixOf" [refT $ constT $ objT c_QVersionNumber] boolT
  , just $ mkConstMethod "majorVersion" np intT
  , just $ mkConstMethod "microVersion" np intT
  , just $ mkConstMethod "minorVersion" np intT
  , just $ mkConstMethod "normalized" np $ objT c_QVersionNumber
  , just $ mkConstMethod "segmentAt" [intT] intT
  , just $ mkConstMethod "segmentCount" np intT
  , just $ mkConstMethod "segments" np $ toGcT $ objT c_QVectorInt
  , just $ mkConstMethod "toString" np $ objT c_QString
  ]
