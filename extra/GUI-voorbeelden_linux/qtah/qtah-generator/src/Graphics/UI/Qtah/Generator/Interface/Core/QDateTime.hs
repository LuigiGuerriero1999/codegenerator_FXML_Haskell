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

module Graphics.UI.Qtah.Generator.Interface.Core.QDateTime (
  aModule,
  c_QDateTime,
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
  mkStaticMethod,
  mkStaticMethod',
  mkCtor,
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Comparable, Equatable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, voidT, enumT, constT, objT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QDate (c_QDate)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QTime (c_QTime)
import Graphics.UI.Qtah.Generator.Interface.Core.QTimeZone (c_QTimeZone)
--import Graphics.UI.Qtah.Generator.Interface.Core.QDataStream (c_QDataStream)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qint64, e_TimeSpec, e_DateFormat)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QDateTime"] $
  [qtExport c_QDateTime]

c_QDateTime =
  addReqIncludes [ includeStd "QDateTime" ] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Comparable, Equatable, Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QDateTime") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithDate" [refT $ constT $ objT c_QDate]
  , just $ mkCtor "newWithDateAndTime" [refT $ constT $ objT c_QDate, refT $ constT $ objT c_QTime]
  , just $ mkCtor "newWithDateAndTimeAndTimeSpec" [refT $ constT $ objT c_QDate, refT $ constT $ objT c_QTime, enumT e_TimeSpec]
  , test (qtVersion >= [5, 2]) $ mkCtor "newWithDateAndTimeAndTimeSpecAndOffset" [refT $ constT $ objT c_QDate, refT $ constT $ objT c_QTime, enumT e_TimeSpec, intT]
  , test (qtVersion >= [5, 2]) $ mkCtor "newWithDateAndTimeAndTimeZone" [refT $ constT $ objT c_QDate, refT $ constT $ objT c_QTime, refT $ constT $ objT c_QTimeZone]
  , just $ mkConstMethod "addDays" [qint64] $ objT c_QDateTime
  , just $ mkConstMethod "addMSecs" [qint64] $ objT c_QDateTime
  , just $ mkConstMethod "addMonths" [intT] $ objT c_QDateTime
  , just $ mkConstMethod "addSecs" [qint64] $ objT c_QDateTime
  , just $ mkConstMethod "addYears" [intT] $ objT c_QDateTime
  , just $ mkStaticMethod "currentDateTime" np $ objT c_QDateTime
  , test (qtVersion >= [4, 7]) $ mkStaticMethod "currentDateTimeUtc" np $ objT c_QDateTime
  , test (qtVersion >= [4, 7]) $ mkStaticMethod "currentMSecsSinceEpoch" np qint64
  , test (qtVersion >= [5, 8]) $ mkStaticMethod "currentSecsSinceEpoch" np qint64
  , just $ mkConstMethod "date" np $ objT c_QDate
  , just $ mkConstMethod "daysTo" [refT $ constT $ objT c_QDateTime] qint64
  --, test (qtVersion >= [5, 5]) $ mkStaticMethod "fromCFDate" [objT c_CFDateRef] $ objT c_QDateTime
  , test (qtVersion >= [4, 7]) $ mkStaticMethod' "fromMSecsSinceEpoch" "fromMSecsSinceEpoch" [qint64] $ objT c_QDateTime
  , test (qtVersion >= [5, 2]) $ mkStaticMethod' "fromMSecsSinceEpoch" "fromMSecsSinceEpochWithTimeSpec" [qint64, enumT e_TimeSpec] $ objT c_QDateTime
  , test (qtVersion >= [5, 2]) $ mkStaticMethod' "fromMSecsSinceEpoch" "fromMSecsSinceEpochWithTimeSpecAndOffset" [qint64, enumT e_TimeSpec, intT] $ objT c_QDateTime
  , test (qtVersion >= [5, 2]) $ mkStaticMethod' "fromMSecsSinceEpoch" "fromMSecsSinceEpochWithTimeZone" [qint64, refT $ constT $ objT c_QTimeZone] $ objT c_QDateTime
  --, test (qtVersion >= [5, 5]) $ mkStaticMethod "fromNSDate" [ptrT $ constT $ objT c_NSDate] $ objT c_QDateTime
  , test (qtVersion >= [5, 8]) $ mkStaticMethod' "fromSecsSinceEpoch" "fromSecsSinceEpoch" [qint64] $ objT c_QDateTime
  , test (qtVersion >= [5, 8]) $ mkStaticMethod' "fromSecsSinceEpoch" "fromSecsSinceEpochWithTimeSpec" [qint64, enumT e_TimeSpec] $ objT c_QDateTime
  , test (qtVersion >= [5, 8]) $ mkStaticMethod' "fromSecsSinceEpoch" "fromSecsSinceEpochWithTimeSpecAndOffset" [qint64, enumT e_TimeSpec, intT] $ objT c_QDateTime
  , test (qtVersion >= [5, 8]) $ mkStaticMethod' "fromSecsSinceEpoch" "fromSecsSinceEpochWithTimeZone" [qint64, refT $ constT $ objT c_QTimeZone] $ objT c_QDateTime
  , just $ mkStaticMethod' "fromString" "fromString" [refT $ constT $ objT c_QString] $ objT c_QDateTime
  , just $ mkStaticMethod' "fromString" "fromStringWithDateFormat" [refT $ constT $ objT c_QString, enumT e_DateFormat] $ objT c_QDateTime
  , just $ mkStaticMethod' "fromString" "fromStringWithStringFormat" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] $ objT c_QDateTime
  , test (qtVersion >= [5, 2]) $ mkConstMethod "isDaylightTime" np boolT
  , just $ mkConstMethod "isNull" np boolT
  , just $ mkConstMethod "isValid" np boolT
  , just $ mkConstMethod "msecsTo" [refT $ constT $ objT c_QDateTime] qint64
  , test (qtVersion >= [5, 2]) $ mkConstMethod "offsetFromUtc" np intT
  , just $ mkConstMethod "secsTo" [refT $ constT $ objT c_QDateTime] qint64
  , just $ mkMethod "setDate" [refT $ constT $ objT c_QDate] voidT
  , test (qtVersion >= [4, 7]) $ mkMethod "setMSecsSinceEpoch" [qint64] voidT
  , test (qtVersion >= [5, 2]) $ mkMethod "setOffsetFromUtc" [intT] voidT
  , test (qtVersion >= [5, 8]) $ mkMethod "setSecsSinceEpoch" [qint64] voidT
  , just $ mkMethod "setTime" [refT $ constT $ objT c_QTime] voidT
  , just $ mkMethod "setTimeSpec" [enumT e_TimeSpec] voidT
  , test (qtVersion >= [5, 2]) $ mkMethod "setTimeZone" [refT $ constT $ objT c_QTimeZone] voidT
  , test (qtVersion >= [5, 0]) $ mkMethod "swap" [refT $ objT c_QDateTime] voidT
  , just $ mkConstMethod "time" np $ objT c_QTime
  , just $ mkConstMethod "timeSpec" np $ enumT e_TimeSpec
  , test (qtVersion >= [5, 2]) $ mkConstMethod "timeZone" np $ objT c_QTimeZone
  , test (qtVersion >= [5, 2]) $ mkConstMethod "timeZoneAbbreviation" np $ objT c_QString
  --, test (qtVersion >= [5, 5]) $ mkConstMethod "toCFDate" np $ objT c_CFDateRef
  , just $ mkConstMethod "toLocalTime" np $ objT c_QDateTime
  , test (qtVersion >= [4, 7]) $ mkConstMethod "toMSecsSinceEpoch" np qint64
  --, test (qtVersion >= [5, 5]) $ mkConstMethod "toNSDate" np $ ptrT $ objT c_NSDate
  , test (qtVersion >= [5, 2]) $ mkConstMethod "toOffsetFromUtc" [intT] $ objT c_QDateTime
  , test (qtVersion >= [5, 8]) $ mkConstMethod "toSecsSinceEpoch" np qint64
  , just $ mkConstMethod' "toString" "toString" np $ objT c_QString
  , just $ mkConstMethod' "toString" "toStringWithDateFormat" [enumT e_DateFormat] $ objT c_QString
  , just $ mkConstMethod' "toString" "toStringWithStringFormat" [enumT e_DateFormat] $ objT c_QString
    -- TODO QString toString(QStringView)
  , just $ mkConstMethod "toTimeSpec" [enumT e_TimeSpec] $ objT c_QDateTime
  , test (qtVersion >= [5, 2]) $ mkConstMethod "toTimeZone" [refT $ constT $ objT c_QTimeZone] $ objT c_QDateTime
  , just $ mkConstMethod "toUTC" np $ objT c_QDateTime
  --, just $ mkMethod OpShl [refT $ objT c_QDataStream, refT $ objT c_QDateTime] $ refT $ objT c_QDataStream
  --, just $ mkMethod OpShr [refT $ objT c_QDataStream, refT $ objT c_QDateTime] $ refT $ objT c_QDataStream
  ]
