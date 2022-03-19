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

module Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (
  aModule,
  c_QByteArray,
  e_Base64Option,
  fl_Base64Options,
  ) where

import Foreign.Hoppy.Generator.Language.Haskell (
  addImports,
  indent,
  ln,
  sayLn,
  )
import Foreign.Hoppy.Generator.Spec (
  Class,
  ClassHaskellConversion (
    ClassHaskellConversion,
    classHaskellConversionFromCppFn,
    classHaskellConversionToCppFn,
    classHaskellConversionType
  ),
  addAddendumHaskell,
  addReqIncludes,
  classSetEntityPrefix,
  classSetHaskellConversion,
  hsImport1,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Comparable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (ushortT, shortT, uintT, floatT, doubleT, boolT, charT, constT, intT, ptrT, voidT, refT, objT, enumT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_CaseSensitivity)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Imports
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Core", "QByteArray"] $
  collect
  [ just $ qtExport c_QByteArray
  , test (qtVersion >= [5, 2]) $ qtExport e_Base64Option
  , test (qtVersion >= [5, 2]) $ qtExport fl_Base64Options
  ]

c_QByteArray :: Class
c_QByteArray =
  addReqIncludes [includeStd "QByteArray"] $
  addAddendum $
  classAddFeatures [Assignable, Copyable, Comparable, Equatable] $
  classSetHaskellConversion conversion $
  classSetEntityPrefix "" $
  makeClass (ident "QByteArray") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newFromData" [ptrT $ constT charT]
  , just $ mkCtor "newFromDataAndSize" [ptrT $ constT charT, intT]
  , just $ mkCtor "newFromRepeatedChar" [intT, charT]

  , just $ mkMethod' "append" "append" [objT c_QByteArray] $ refT $ objT c_QByteArray
  , test (qtVersion >= [5, 7]) $ mkMethod' "append" "appendCountAndChar" [intT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "append" "appendPtrConstChar" [ptrT $ constT charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "append" "appendPtrConstCharAndSize" [ptrT $ constT charT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "append" "appendChar" [charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "append" "appendString" [objT c_QString] $ refT $ objT c_QByteArray

  , just $ mkMethod' "insert" "insert" [intT, objT c_QByteArray] $ refT $ objT c_QByteArray
  , test (qtVersion >= [5, 7]) $ mkMethod' "insert" "insertCountAndChar" [intT, intT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "insert" "insertPtrConstChar" [intT, ptrT $ constT charT] $ refT $ objT c_QByteArray
  , test (qtVersion >= [4, 6]) $  mkMethod' "insert" "insertPtrConstCharAndSize" [intT, ptrT $ constT charT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "insert" "insertChar" [intT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "insert" "insertString" [intT, objT c_QString] $ refT $ objT c_QByteArray

  , just $ mkMethod' "prepend" "prepend" [objT c_QByteArray] $ refT $ objT c_QByteArray
  , test (qtVersion >= [5, 7]) $ mkMethod' "prepend" "prependCountAndChar" [intT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "prepend" "prependPtrConstChar" [ptrT $ constT charT] $ refT $ objT c_QByteArray
  , test (qtVersion >= [4, 6]) $ mkMethod' "prepend" "prependPtrConstCharAndSize" [ptrT $ constT charT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "prepend" "prependChar" [charT] $ refT $ objT c_QByteArray

  , just $ mkMethod' "replace" "replaceRangeWithByteArray" [intT, intT, objT c_QByteArray] $ refT $ objT c_QByteArray
  , test (qtVersion >= [4, 7]) $ mkMethod' "replace" "replaceRangeWithPtrConstCharAndSize" [intT, intT, ptrT $ constT charT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceRangeWithPtrConstChar" [intT, intT, ptrT $ constT charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceCharWithPtrConstChar" [charT, ptrT $ constT charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceCharWithByteArray" [charT, objT c_QByteArray] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replacePtrConstChar" [ptrT $ constT charT, ptrT $ constT charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replacePtrConstCharAndSize" [ptrT $ constT charT, intT, ptrT $ constT charT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceByteArray" [objT c_QByteArray, objT c_QByteArray] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceByteArrayWithPtrConstChar" [objT c_QByteArray, ptrT $ constT charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replacePtrConstCharWithByteArray" [ptrT $ constT charT, objT c_QByteArray] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceChar" [charT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceStringWithPtrConstChar" [objT c_QString, ptrT $ constT charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceCharWithString" [charT, objT c_QString] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceStringWithByteArray" [objT c_QString, objT c_QByteArray] $ refT $ objT c_QByteArray

  , just $ mkConstMethod' "indexOf" "indexOf" [objT c_QByteArray] intT
  , just $ mkConstMethod' "indexOf" "indexOfFrom" [objT c_QByteArray, intT] intT
  , just $ mkConstMethod' "indexOf" "indexOfPtrConstChar" [ptrT $ constT charT] intT
  , just $ mkConstMethod' "indexOf" "indexOfPtrConstCharFrom" [ptrT $ constT charT, intT] intT
  , just $ mkConstMethod' "indexOf" "indexOfChar" [charT] intT
  , just $ mkConstMethod' "indexOf" "indexOfCharFrom" [charT, intT] intT
  , just $ mkConstMethod' "indexOf" "indexOfString" [objT c_QString, intT] intT
  , just $ mkConstMethod' "indexOf" "indexOfStringFrom" [objT c_QString, intT] intT

  , just $ mkConstMethod' "lastIndexOf" "lastIndexOf" [objT c_QByteArray] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfFrom" [objT c_QByteArray, intT] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfPtrConstChar" [ptrT $ constT charT] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfPtrConstCharFrom" [ptrT $ constT charT, intT] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfChar" [charT] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfCharFrom" [charT, intT] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfString" [objT c_QString, intT] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfStringFrom" [objT c_QString, intT] intT

  , just $ mkMethod' "setNum" "setNumInt" [intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumIntWithBase" [intT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumUshort" [ushortT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumUshortWithBase" [ushortT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumShort" [shortT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumShortWithBase" [shortT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumUint" [uintT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumUintWithBase" [uintT, intT] $ refT $ objT c_QByteArray
  --, just $ mkMethod' "setNum" "setNumQlonglong" [qlonglong] $ refT $ objT c_QByteArray
  --, just $ mkMethod' "setNum" "setNumQlonglongWithBase" [qlonglong, intT] $ refT $ objT c_QByteArray
  --, just $ mkMethod' "setNum" "setNumQulonglong" [qulonglong] $ refT $ objT c_QByteArray
  --, just $ mkMethod' "setNum" "setNumQulonglongWithBase" [qulonglong, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumFloat" [floatT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumFloatWithFormat" [floatT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumFloatWithFormatAndPrecision" [floatT, charT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumDouble" [doubleT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumDoubleWithFormat" [doubleT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumDoubleWithFormatAndPrecision" [doubleT, charT, intT] $ refT $ objT c_QByteArray

  , just $ mkConstMethod "at" [intT] charT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "back" np charT
  -- TODO QByteRef back()
  -- TODO QByteArray::iterator begin()
  -- TODO QByteArray::const_iterator begin() const
  , just $ mkConstMethod "capacity" np intT
  -- QByteArray::const_iterator cbegin() const
  -- QByteArray::const_iterator cend() const
  , just $ mkMethod "chop" [intT] voidT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "chopped" [intT] $ objT c_QByteArray
  , test (qtVersion >= [5, 12]) $ mkConstMethod' "compare" "comparePtrConstChar" [ptrT $ constT charT] intT
  , test (qtVersion >= [5, 12]) $ mkConstMethod' "compare" "comparePtrConstCharWithCase" [ptrT $ constT charT, enumT e_CaseSensitivity] intT
  , test (qtVersion >= [5, 12]) $ mkConstMethod' "compare" "compare" [objT c_QByteArray] intT
  , test (qtVersion >= [5, 12]) $ mkConstMethod' "compare" "compareWithCase" [objT c_QByteArray, enumT e_CaseSensitivity] intT
  -- TODO QByteArray::const_iterator constBegin() const
  , just $ mkConstMethod "constData" np $ ptrT $ constT charT
  -- TODO QByteArray::const_iterator constEnd() const
  , just $ mkConstMethod' "contains" "contains" [objT c_QByteArray] boolT
  , just $ mkConstMethod' "contains" "containsPtrConstChar" [ptrT $ constT charT] boolT
  , just $ mkConstMethod' "contains" "containsChar" [charT] boolT
  , just $ mkConstMethod' "count" "countByteArray" [objT c_QByteArray] intT
  , just $ mkConstMethod' "count" "countPtrConstChar" [ptrT $ constT charT] intT
  , just $ mkConstMethod' "count" "countChar" [charT] intT
  , just $ mkConstMethod' "count" "count" np intT
  -- TODO QByteArray::const_reverse_iterator crbegin() const
  -- TODO QByteArray::const_reverse_iterator crend() const
  , just $ mkMethod "clear" np voidT
  , just $ mkMethod' "data" "getData" np $ ptrT charT
  , just $ mkConstMethod' "data" "getDataConst" np $ ptrT $ constT charT
  -- TODO QByteArray::iterator end()
  -- TODO QByteArray::const_iterator end() const
  , just $ mkConstMethod' "endsWith" "endsWith" [objT c_QByteArray] boolT
  , just $ mkConstMethod' "endsWith" "endsWithPtrConstChar" [ptrT $ constT charT] boolT
  , just $ mkConstMethod' "endsWith" "endsWithChar" [charT] boolT
  , just $ mkMethod' "fill" "fill" [charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "fill" "fillWithSize" [charT, intT] $ refT $ objT c_QByteArray
  , test (qtVersion >= [5, 10]) $ mkConstMethod "front" np charT
  -- TODO QByteRef front()
  , just $ mkConstMethod "isEmpty" np boolT
  , test (qtVersion >= [5, 12]) $ mkConstMethod "isLower" np boolT
  , just $ mkConstMethod "isNull" np boolT
  , test (qtVersion >= [5, 12]) $ mkConstMethod "isUpper" np boolT
  , just $ mkConstMethod "left" [intT] $ objT c_QByteArray
  , just $ mkConstMethod' "leftJustified" "leftJustified" [intT] $ objT c_QByteArray
  , just $ mkConstMethod' "leftJustified" "leftJustifiedWithChar" [intT, charT] $ objT c_QByteArray
  , just $ mkConstMethod' "leftJustified" "leftJustifiedWithCharAndTruncate" [intT, charT, boolT] $ objT c_QByteArray
  , just $ mkConstMethod "length" np intT
  , just $ mkConstMethod "size" np intT
    -- TODO Lots more methods.
  ]

conversion :: ClassHaskellConversion
conversion =
  ClassHaskellConversion
  { classHaskellConversionType = Just $ do
    addImports importForByteString
    return $ HsTyCon $ UnQual $ HsIdent "QtahDBS.ByteString"
  , classHaskellConversionToCppFn = Just $ sayLn "convertToCpp"
  , classHaskellConversionFromCppFn = Just $ sayLn "convertFromCpp"
  }

addAddendum :: Class -> Class
addAddendum = addAddendumHaskell $ do
  addImports $ mconcat [hsImport1 "Prelude" "($)",
                        importForByteString,
                        importForByteStringUnsafe,
                        importForPrelude]
  ln
  sayLn "convertToCpp :: QtahDBS.ByteString -> QtahP.IO QByteArray"
  sayLn "convertToCpp ="
  indent $
    sayLn "QtahP.flip QtahDBSU.unsafeUseAsCStringLen $ QtahP.uncurry newFromDataAndSize"
  ln
  sayLn "convertFromCpp :: QByteArrayValue ba => ba -> QtahP.IO QtahDBS.ByteString"
  sayLn "convertFromCpp ba = do"
  indent $ do
    sayLn "d <- getDataConst ba"
    sayLn "len <- size ba"
    sayLn "QtahDBS.packCStringLen (d, len)"

(e_Base64Option, fl_Base64Options) =
  makeQtEnumAndFlags (ident1 "QByteArray" "Base64Option") "Base64Options" [includeStd "QByteArray"]
  [ "Base64Encoding"
  , "KeepTrailingEquals"
  , "Base64UrlEncoding"
  , "OmitTrailingEquals"
  ]
