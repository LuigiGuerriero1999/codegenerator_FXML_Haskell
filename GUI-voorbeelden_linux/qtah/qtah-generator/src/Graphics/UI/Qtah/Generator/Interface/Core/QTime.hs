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

module Graphics.UI.Qtah.Generator.Interface.Core.QTime (
  aModule,
  c_QTime,
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
  mkMethod',
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Comparable, Equatable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, voidT, enumT, constT, objT, refT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_DateFormat)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QTime"] $
  [qtExport c_QTime]

c_QTime =
  addReqIncludes [ includeStd "QTime" ] $
  classSetConversionToGc $
  classAddFeatures [Comparable, Equatable, Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QTime") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithHM" [intT, intT]
  , just $ mkCtor "newWithHMS" [intT, intT, intT]
  , just $ mkCtor "newWithHMSMs" [intT, intT, intT, intT]
  , just $ mkConstMethod "addMSecs" [intT] $ objT c_QTime
  , just $ mkConstMethod "addSecs" [intT] $ objT c_QTime
  , just $ mkStaticMethod "currentTime" np $ objT c_QTime
  , just $ mkConstMethod "elapsed" np intT
  , just $ mkStaticMethod "fromMSecsSinceStartOfDay" [intT] $ objT c_QTime
  , just $ mkStaticMethod' "fromString" "fromString" [refT $ constT $ objT c_QString] $ objT c_QTime
  , just $ mkStaticMethod' "fromString" "fromStringWithDateFormat" [refT $ constT $ objT c_QString, enumT e_DateFormat] $ objT c_QTime
  , just $ mkStaticMethod' "fromString" "fromStringWithStringFormat" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] $ objT c_QTime
  , just $ mkConstMethod "hour" np intT
  , just $ mkConstMethod "isNull" np boolT
  , just $ mkConstMethod' "isValid" "isValid" np boolT
  , just $ mkMethod' "isValid" "isValidStatic" [intT, intT, intT] boolT
  , just $ mkMethod' "isValid" "isValidStaticWithMs" [intT, intT, intT, intT] boolT
  , just $ mkConstMethod "minute" np intT
  , just $ mkConstMethod "msec" np intT
  , just $ mkConstMethod "msecsSinceStartOfDay" np intT
  , just $ mkConstMethod "msecsTo" [refT $ constT $ objT c_QTime] intT
  , just $ mkMethod "restart" np intT
  , just $ mkConstMethod "second" np intT
  , just $ mkConstMethod "secsTo" [refT $ constT $ objT c_QTime] intT
  , just $ mkMethod' "setHMS" "setHMS" [intT, intT, intT ] boolT
  , just $ mkMethod' "setHMS" "setHMSWithMs" [intT, intT, intT, intT] boolT
  , just $ mkMethod "start" np voidT
  , just $ mkConstMethod' "toString" "toString" np $ objT c_QString
  , just $ mkConstMethod' "toString" "toStringWithDateFormat" [enumT e_DateFormat] $ objT c_QString
  , just $ mkConstMethod' "toString" "toStringWithStringFormat" [objT c_QString] $ objT c_QString
  --, just $ mkMethod OpShl [refT $ objT c_QDataStream, refT $ objT c_QTime] $ refT $ objT c_QDataStream
  --, just $ mkMethod OpShr [refT $ objT c_QDataStream, refT $ objT c_QTime] $ refT $ objT c_QDataStream
  ]
