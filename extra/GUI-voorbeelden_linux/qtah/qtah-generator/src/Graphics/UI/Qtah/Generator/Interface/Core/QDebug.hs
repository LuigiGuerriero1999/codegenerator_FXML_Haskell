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

module Graphics.UI.Qtah.Generator.Interface.Core.QDebug (
  aModule,
  c_QDebug,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Operator (OpShl),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  np,
  operatorPreferredExtName',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  constT,
  enumT,
  intT,
  objT,
  ptrT,
  refT,
  uintT,
  shortT,
  ushortT,
  doubleT,
  floatT,
  longT,
  ulongT,
  charT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_QtMsgType, qlonglong, qulonglong)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QDebug"] $
  [ qtExport c_QDebug
  ]

c_QDebug =
  addReqIncludes [ includeStd "QDebug", includeStd "QtGlobal" ] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QDebug") Nothing [] $
  collect
  [
    just $ mkCtor "newWithMsgType" [enumT e_QtMsgType]
  , just $ mkCtor "newWithString" [ptrT $ objT c_QString]
  , just $ mkCtor "newWithIODevice" [ptrT $ objT c_QIODevice]
  , test (qtVersion >= [5, 0]) $ mkConstMethod "autoInsertSpaces" np boolT
  , test (qtVersion >= [5, 4]) $ mkMethod' "maybeQuote" "maybeQuote" np $ refT $ objT c_QDebug
  , test (qtVersion >= [5, 4]) $ mkMethod' "maybeQuote" "maybeQuoteWithChar" [charT] $ refT $ objT c_QDebug
  , just $ mkMethod "maybeSpace" np $ refT $ objT c_QDebug
  , test (qtVersion >= [5, 4]) $ mkMethod "noquote" np $ refT $ objT c_QDebug
  , just $ mkMethod "nospace" np $ refT $ objT c_QDebug
  , test (qtVersion >= [5, 4]) $ mkMethod "quote" np $ refT $ objT c_QDebug
  , test (qtVersion >= [5, 4]) $ mkMethod "resetFormat" np $ refT $ objT c_QDebug
  , test (qtVersion >= [5, 0]) $ mkMethod "setAutoInsertSpaces" [boolT] voidT
  , test (qtVersion >= [5, 6]) $ mkMethod "setVerbosity" [intT] voidT
  , just $ mkMethod "space" np $ refT $ objT c_QDebug
  , just $ mkMethod "swap" [refT $ objT c_QDebug] voidT
  , test (qtVersion >= [5, 13]) $ mkMethod' "verbosity" "verbosityWithLevel" [intT] $ refT $ objT c_QDebug
  , test (qtVersion >= [5, 6]) $ mkConstMethod' "verbosity" "verbosity" np intT

  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "QChar")
    [objT c_QChar] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "Bool")
    [boolT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "Char")
    [charT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "Short")
    [shortT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "Ushort")
    [ushortT] $ refT $ objT c_QDebug
  --, just $ mkMethod OpShl [char16_t t] $ refT $ objT c_QDebug
  --, just $ mkMethod OpShl [char32_t t] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "Int")
    [intT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "Uint")
    [uintT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "Long")
    [longT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "Ulong")
    [ulongT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "Qlonglong")
    [qlonglong] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "Qulonglong")
    [qulonglong] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "Float")
    [floatT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "Double")
    [doubleT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "PtrConstChar")
    [ptrT $ constT charT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "String")
    [objT c_QString] $ refT $ objT c_QDebug
  --, just $ mkMethod OpShl [refT $ constT $ objT c_QStringRef] $ refT $ objT c_QDebug
  --, test (qtVersion >= [5, 10]) $ mkMethod OpShl [refT $ constT $ objT c_QStringView] $ refT $ objT c_QDebug
  --, just $ mkMethod OpShl [refT $ constT $ objT c_QLatin1String] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "ByteArray")
    [refT $ constT $ objT c_QByteArray] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl (operatorPreferredExtName' OpShl ++ "PtrVoid")
    [ptrT $ constT voidT] $ refT $ objT c_QDebug
  ]
