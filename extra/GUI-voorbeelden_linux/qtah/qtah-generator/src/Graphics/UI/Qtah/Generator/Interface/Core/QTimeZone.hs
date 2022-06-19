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

module Graphics.UI.Qtah.Generator.Interface.Core.QTimeZone (
  aModule,
  c_QTimeZone,
  e_NameType,
  e_TimeType,
  --e_anonymous,
  ) where

import Foreign.Hoppy.Generator.Spec (
  classSetConversionToGc,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkStaticMethod,
  mkStaticMethod',
  mkCtor,
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, voidT, enumT, constT, objT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import {-#SOURCE#-} Graphics.UI.Qtah.Generator.Interface.Core.QDateTime (c_QDateTime)
--import Graphics.UI.Qtah.Generator.Interface.Core.QLocale (c_QLocale, e_Country)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QTimeZone"] [5, 2] $
  collect
  [ just $ qtExport c_QTimeZone
  , just $ qtExport e_NameType
  , just $ qtExport e_TimeType
  --, just $ qtExport e_anonymous
  ]

c_QTimeZone =
  addReqIncludes [ includeStd "QTimeZone" ] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QTimeZone") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newFull1" [refT $ constT $ objT c_QByteArray, intT, refT $ constT $ objT c_QString, refT $ constT $ objT c_QString]
  --, just $ mkCtor "newFull2" [refT $ constT $ objT c_QByteArray, intT, refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, enumT e_Country]
  --, just $ mkCtor "newFull3" [refT $ constT $ objT c_QByteArray, intT, refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, enumT e_Country, refT $ constT $ objT c_QString]
  , just $ mkCtor "newWithOffset" [intT]
  , just $ mkCtor "newWithByteArray" [refT $ constT $ objT c_QByteArray]
  , just $ mkConstMethod "abbreviation" [refT $ constT $ objT c_QDateTime] $ objT c_QString
  , just $ mkStaticMethod' "availableTimeZoneIds" "availableTimeZoneIds" np $ objT c_QListQByteArray
  -- just $ mkStaticMethod' "availableTimeZoneIds" "availableTimeZoneIdsWithCountry" [enumT e_Country] $ objT c_QListQByteArray
  , just $ mkStaticMethod' "availableTimeZoneIds" "availableTimeZoneIdsWithOffset" [intT] $ objT c_QListQByteArray
  , just $ mkConstMethod "comment" np $ objT c_QString
  --, just $ mkConstMethod "country" np $ enumT e_Country
  , just $ mkConstMethod "daylightTimeOffset" [refT $ constT $ objT c_QDateTime] intT
  , just $ mkConstMethod' "displayName" "displayNameWithDateTime" [refT $ constT $ objT c_QDateTime] $ objT c_QString
  , just $ mkConstMethod' "displayName" "displayNameWithDateTimeAndNameType" [refT $ constT $ objT c_QDateTime, enumT e_NameType] $ objT c_QString
  -- just $ mkConstMethod' "displayName" "displayNameQLocale" [refT $ constT $ objT c_QDateTime, enumT e_NameType, refT $ constT $ objT c_QLocale] $ objT c_QString
  , just $ mkConstMethod' "displayName" "displayNameWithTimeType" [enumT e_TimeType] $ objT c_QString
  , just $ mkConstMethod' "displayName" "displayNameWithTimeTypeAndNameType" [enumT e_TimeType, enumT e_NameType] $ objT c_QString
  -- just $ mkConstMethod' "displayName" "displayNameTimeNameQLocale" [enumT e_TimeType, enumT e_NameType, refT $ constT $ objT c_QLocale] $ objT c_QString
  --, test (qtVersion >= [5, 9]) $ mkStaticMethod "fromCFTimeZone" [objT c_CFTimeZoneRef] $ objT c_QTimeZone
  --, test (qtVersion >= [5, 9]) $ mkStaticMethod "fromNSTimeZone" [ptrT $ constT $ objT c_NSTimeZone] $ objT c_QTimeZone
  , just $ mkConstMethod "hasDaylightTime" np boolT
  , just $ mkConstMethod "hasTransitions" np boolT
  , just $ mkStaticMethod "ianaIdToWindowsId" [refT $ constT $ objT c_QByteArray] $ objT c_QByteArray
  , just $ mkConstMethod "id" np $ objT c_QByteArray
  , just $ mkConstMethod "isDaylightTime" [refT $ constT $ objT c_QDateTime] boolT
  , just $ mkStaticMethod "isTimeZoneIdAvailable" [refT $ constT $ objT c_QByteArray] boolT
  , just $ mkConstMethod "isValid" np boolT
  -- TODO QTimeZone::OffsetData QTimeZone::nextTransition(const QDateTime &afterDateTime) const
  -- TODO QTimeZone::OffsetData QTimeZone::offsetData(const QDateTime &forDateTime) const
  , just $ mkConstMethod "offsetFromUtc" [refT $ constT $ objT c_QDateTime] intT
  -- TODO QTimeZone::OffsetData QTimeZone::previousTransition(const QDateTime &beforeDateTime) const
  , just $ mkConstMethod "standardTimeOffset" [refT $ constT $ objT c_QDateTime] intT
  , just $ mkMethod "swap" [refT $ objT c_QTimeZone] voidT
  , test (qtVersion >= [5, 5]) $ mkStaticMethod "systemTimeZone" np $ objT c_QTimeZone
  , just $ mkStaticMethod "systemTimeZoneId" np $ objT c_QByteArray
  --, test (qtVersion >= [5, 9]) $ mkConstMethod "toCFTimeZone" np $ objT c_CFTimeZoneRef
  --, test (qtVersion >= [5, 9]) $ mkConstMethod "toNSTimeZone" np $ ptrT $ objT c_NSTimeZone
  -- TODO QTimeZone::OffsetDataList QTimeZone::transitions(const QDateTiacvme &fromDateTime, const QDateTime &toDateTime) const
  , test (qtVersion >= [5, 5]) $ mkStaticMethod "utc" np $ objT c_QTimeZone
  , just $ mkStaticMethod "windowsIdToDefaultIanaId" [refT $ constT $ objT c_QByteArray] $ objT c_QByteArray
  --, just $ mkStaticMethod "windowsIdToDefaultIanaId" [refT $ constT $ objT c_QByteArray, enumT e_Country] $ objT c_QByteArray
  , just $ mkStaticMethod' "windowsIdToIanaIds" "windowsIdToIanaIds" [refT $ constT $ objT c_QByteArray] $ objT c_QListQByteArray
  --, just $ mkStaticMethod' "windowsIdToIanaIds" "windowsIdToIanaIdsWithCountry" [refT $ constT $ objT c_QByteArray, enumT e_Country] $ objT c_QListQByteArray
  ]

e_NameType =
  makeQtEnum (ident1 "QTimeZone" "NameType") [includeStd "QTimeZone"]
  [ "DefaultName"
  , "LongName"
  , "ShortName"
  , "OffsetName"
  ]

e_TimeType =
  makeQtEnum (ident1 "QTimeZone" "TimeType") [includeStd "QTimeZone"]
  [ "StandardTime"
  , "DaylightTime"
  , "GenericTime"
  ]

-- TODO Enum autodetection doesn't like this enum.
--e_anonymous =
--  makeQtEnum (ident1 "QTimeZone" "anonymous") [includeStd "QTimeZone"]
--  [ "MinUtcOffsetSecs"
--  , "MaxUtcOffsetSecs"
--  ]
