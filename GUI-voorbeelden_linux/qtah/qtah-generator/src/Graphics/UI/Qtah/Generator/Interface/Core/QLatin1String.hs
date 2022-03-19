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

module Graphics.UI.Qtah.Generator.Interface.Core.QLatin1String (
  aModule,
  c_QLatin1String,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Operator (OpArray, OpNe, OpLt, OpLe, OpEq, OpGt, OpGe),
  classSetConversionToGc,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  np,
  operatorPreferredExtName',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Equatable, Comparable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (toGcT, charT, intT, boolT, voidT, enumT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QLatin1Char (c_QLatin1Char)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
--import Graphics.UI.Qtah.Generator.Interface.Core.QStringView (c_QStringView)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_CaseSensitivity)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QLatin1String"] $
  [qtExport c_QLatin1String]

c_QLatin1String =
  addReqIncludes [ includeStd "QLatin1String" ] $
  classSetConversionToGc $
  classAddFeatures [Equatable, Comparable, Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QLatin1String") Nothing [] $
  collect
  [ test (qtVersion >= [5, 6]) $ mkCtor "new" np
  , just $ mkCtor "newWithCharPtr" [ptrT $ constT charT]
  , test (qtVersion >= [5, 10]) $ mkCtor "newWithCharPtrRange" [ptrT $ constT charT, ptrT $ constT charT]
  , just $ mkCtor "newWithCharPtrAndSize" [ptrT $ constT charT, intT]
  , just $ mkCtor "newWithByteArray" [refT $ constT $ objT c_QByteArray]
  , test (qtVersion >= [5, 8]) $ mkConstMethod' "at" "at" [intT] $ toGcT $ objT c_QLatin1Char
  , test (qtVersion >= [5, 10]) $ mkConstMethod "back" np $ toGcT $ objT c_QLatin1Char
  , test (qtVersion >= [5, 10]) $ mkMethod "chop" [intT] voidT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "chopped" [intT] $ objT c_QLatin1String
  , just $ mkConstMethod' "data" "dataString" np $ ptrT $ constT charT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "front" np $ toGcT $ objT c_QLatin1Char
  , test (qtVersion >= [5, 10]) $ mkConstMethod "isEmpty" np boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "isNull" np boolT
  , just $ mkConstMethod "latin1" np $ ptrT $ constT charT
  , test (qtVersion >= [5, 8]) $ mkConstMethod "left" [intT] $ objT c_QLatin1String
  , test (qtVersion >= [5, 8]) $ mkConstMethod' "mid" "mid" [intT] $ objT c_QLatin1String
  , test (qtVersion >= [5, 8]) $ mkConstMethod' "mid" "midWithLength" [intT, intT] $ objT c_QLatin1String
  , test (qtVersion >= [5, 8]) $ mkConstMethod "right" [intT] $ objT c_QLatin1String
  , just $ mkConstMethod "size" np intT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "trimmed" np $ objT c_QLatin1String
  , test (qtVersion >= [5, 10]) $ mkMethod "truncate" [intT] voidT
  --, test (qtVersion >= [5, 10]) $ mkConstMethod' "endsWith" "endsWithStringView" [objT c_QStringView] boolT
  --, test (qtVersion >= [5, 10]) $ mkConstMethod' "endsWith" "endsWithStringViewWithCase" [objT c_QStringView, enumT e_CaseSensitivity] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "endsWith" "endsWithLatin1String" [objT c_QLatin1String] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "endsWith" "endsWithLatin1StringWithCase" [objT c_QLatin1String, enumT e_CaseSensitivity] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "endsWith" "endsWithChar" [objT c_QChar] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "endsWith" "endsWithCharWithCase" [objT c_QChar, enumT e_CaseSensitivity] boolT
  --, test (qtVersion >= [5, 10]) $ mkConstMethod' "startsWith" "startsWithStringView" [objT c_QStringView] boolT
  --, test (qtVersion >= [5, 10]) $ mkConstMethod' "startsWith" "startsWithStringViewWithCase" [objT c_QStringView, enumT e_CaseSensitivity] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "startsWith" "startsWithLatin1String" [objT c_QLatin1String] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "startsWith" "startsWithLatin1StringWithCase" [objT c_QLatin1String, enumT e_CaseSensitivity] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "startsWith" "startsWithCharWith" [objT c_QChar] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "startsWith" "startsWithCharWithCase" [objT c_QChar, enumT e_CaseSensitivity] boolT
  , just $ mkConstMethod' OpNe (operatorPreferredExtName' OpNe ++ "String")
    [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' OpNe (operatorPreferredExtName' OpNe ++ "PtrConstChar")
    [ptrT $ constT charT] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod' OpNe (operatorPreferredExtName' OpNe ++ "ByteArray")
    [refT $ constT $ objT c_QByteArray] boolT
  , just $ mkConstMethod' OpLt (operatorPreferredExtName' OpLt ++ "String")
    [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' OpLt (operatorPreferredExtName' OpLt ++ "PtrConstChar")
    [ptrT $ constT charT] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod' OpLt (operatorPreferredExtName' OpLt ++ "ByteArray")
    [refT $ constT $ objT c_QByteArray] boolT
  , just $ mkConstMethod' OpLe (operatorPreferredExtName' OpLe ++ "String")
    [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' OpLe (operatorPreferredExtName' OpLe ++ "PtrConstChar")
    [ptrT $ constT charT] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod' OpLe (operatorPreferredExtName' OpLe ++ "ByteArray")
    [refT $ constT $ objT c_QByteArray] boolT
  , just $ mkConstMethod' OpEq (operatorPreferredExtName' OpEq ++ "String")
    [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' OpEq (operatorPreferredExtName' OpEq ++ "PtrConstChar")
    [ptrT $ constT charT] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod' OpEq (operatorPreferredExtName' OpEq ++ "ByteArray")
    [refT $ constT $ objT c_QByteArray] boolT
  , just $ mkConstMethod' OpGt (operatorPreferredExtName' OpGt ++ "String")
    [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' OpGt (operatorPreferredExtName' OpGt ++ "PtrConstChar")
    [ptrT $ constT charT] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod' OpGt (operatorPreferredExtName' OpGt ++ "ByteArray")
    [refT $ constT $ objT c_QByteArray] boolT
  , just $ mkConstMethod' OpGe (operatorPreferredExtName' OpGe ++ "String")
    [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' OpGe (operatorPreferredExtName' OpGe ++ "PtrConstChar")
    [ptrT $ constT charT] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod' OpGe (operatorPreferredExtName' OpGe ++ "ByteArray")
    [refT $ constT $ objT c_QByteArray] boolT
  , test (qtVersion >= [5, 8]) $ mkConstMethod' OpArray "get" [intT] $ toGcT $ objT c_QLatin1Char
  ]
