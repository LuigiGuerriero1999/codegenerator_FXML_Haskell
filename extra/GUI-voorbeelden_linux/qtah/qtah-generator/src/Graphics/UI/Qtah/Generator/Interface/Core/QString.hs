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

module Graphics.UI.Qtah.Generator.Interface.Core.QString (
  aModule,
  c_QString,
  e_SectionFlag,
  fl_SectionFlags,
  e_NormalizationForm,
  e_SplitBehavior,
  ) where

import Foreign.Hoppy.Generator.Language.Haskell (
  addImports,
  sayLn,
  )
import Foreign.Hoppy.Generator.Spec (
  ClassHaskellConversion (
    ClassHaskellConversion,
    classHaskellConversionFromCppFn,
    classHaskellConversionToCppFn,
    classHaskellConversionType
  ),
  MethodApplicability (MNormal),
  Operator (OpArray),
  Purity (Nonpure),
  addReqIncludes,
  classSetEntityPrefix,
  classSetHaskellConversion,
  ident,
  ident1,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod',
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Comparable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Std.String (c_string)
import Foreign.Hoppy.Generator.Types (doubleT, charT, constT, intT, objT, ptrT, refT, voidT, longT, ulongT, uintT, shortT, ushortT, boolT, floatT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Generator.Interface.Imports
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qulonglong, qlonglong)
--import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorUInt)
import Graphics.UI.Qtah.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QString"]
  [ qtExport c_QString
  , qtExport e_SectionFlag
  , qtExport fl_SectionFlags
  , qtExport e_NormalizationForm
  , qtExport e_SplitBehavior
  ]

c_QString =
  addReqIncludes [includeStd "QString",
                  includeLocal "wrap_qstring.hpp"] $
  classAddFeatures [Assignable, Copyable, Comparable, Equatable] $
  classSetHaskellConversion
    ClassHaskellConversion
    { classHaskellConversionType = Just $ do
      addImports importForPrelude
      return $ HsTyCon $ UnQual $ HsIdent "QtahP.String"
    , classHaskellConversionToCppFn = Just $ do
      addImports $ mconcat [importForForeignC, importForPrelude]
      sayLn "QtahP.flip QtahFC.withCString newFromCString"
    , classHaskellConversionFromCppFn = Just $ sayLn "toStdString"
    } $
  classSetEntityPrefix "" $
  makeClass (ident "QString") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newFromByteArray" [objT c_QByteArray]
  , just $ mkCtor "newFromCString" [ptrT $ constT charT]
  , just $ mkCtor "newFromSizeQChar" [intT, objT c_QChar]
  , just $ mkCtor "newFromQChar" [ptrT $ objT c_QChar]
  --, just $ mkCtor "newFromQCharSize" [ptrT $ objT c_QChar, intT]

  , just $ makeFnMethod (ident2 "qtah" "qstring" "set") "set" MNormal Nonpure
    [refT $ objT c_QString, intT, objT c_QChar] voidT

    , just $ mkMethod' "append" "append" [objT c_QString] $ refT $ objT c_QString
--, test (qtVersion >= [5, 0]) $ mkMethod' "append" "appendWithCharInt" [ptrT $ constT charT, intT] $ refT $ objT c_QString
  , just $ mkMethod' "append" "appendQChar" [objT c_QChar] $ refT $ objT c_QString
  -- TODO QString &QString::append(const QStringRef &reference)
  -- TODO QString &QString::append(QLatin1String str)
  , just $ mkMethod' "append" "appendPtrConstChar" [ptrT $ constT charT] $ refT $ objT c_QString
  , just $ mkMethod' "append" "appendByteArray" [objT c_QByteArray] $ refT $ objT c_QString

  , just $ mkConstMethod' "arg" "arg" [objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithSize" [objT c_QString, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithSizeAndFill" [objT c_QString, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argUlonglong" [qulonglong] $ objT c_QString
  , just $ mkConstMethod' "arg" "argUlonglongWithField" [qulonglong, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argUlonglongWithFieldAndBase" [qulonglong, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argUlonglongWithFieldAndBaseAndFill" [qulonglong, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argLonglong" [qlonglong] $ objT c_QString
  , just $ mkConstMethod' "arg" "argLonglongWithField" [qlonglong, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argLonglongWithFieldAndBase" [qlonglong, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argLonglongWithFieldAndBaseAndFill" [qlonglong, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argLong" [longT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argLongWithField" [longT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argLongWithFieldAndBase" [longT, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argLongWithFieldAndBaseAndFill" [longT, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argUlong" [ulongT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argUlongWithField" [ulongT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argUlongWithFieldAndBase" [ulongT, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argUlongWithFieldAndBaseAndFill" [ulongT, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argInt" [intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argIntWithField" [intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argIntWithFieldAndBase" [intT, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argIntWithFieldAndBaseAndFill" [intT, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argUintWith" [uintT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argUintWithField" [uintT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argUintWithFieldAndBase" [uintT, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argUintWithFieldAndBaseAndFill" [uintT, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argShortWith" [shortT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argShortWithField" [shortT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argShortWithFieldAndBase" [shortT, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argShortWithFieldAndBaseAndFill" [shortT, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argUshortWith" [ushortT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argUshortWithField" [ushortT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argUshortWithFieldAndBase" [ushortT, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argUshortWithFieldAndBaseAndFill" [ushortT, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argDoubleWith" [doubleT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argDoubleWithField" [doubleT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argDoubleWithFieldFormat" [doubleT, intT, charT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argDoubleWithFieldFormatPrecision" [doubleT, intT, charT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argDoubleWithFieldFormatPrecisionFill" [doubleT, intT, charT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argCharWith" [charT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argCharWithField" [charT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argCharWithFieldFill" [charT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argQCharWith" [objT c_QChar] $ objT c_QString
  , just $ mkConstMethod' "arg" "argQCharWithField" [objT c_QChar, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argQCharWithFieldFill" [objT c_QChar, intT, objT c_QChar] $ objT c_QString

  -- TODO QString QString::arg(QStringView a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const
  -- TODO QString QString::arg(QLatin1String a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const

  , just $ mkConstMethod' "arg" "argTwoQStrings" [objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argThreeQStrings" [objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argFourQStrings" [objT c_QString, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argFiveQStrings" [objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argSixQStrings" [objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argSevenQStrings" [objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argEightQStrings" [objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argNineQStrings" [objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString

  , just $ mkConstMethod' OpArray "at" [intT] $ objT c_QChar
  , test (qtVersion >= [5, 10]) $ mkConstMethod "back" np $ objT c_QChar
  -- TODO QCharRef QString::back()
  -- TODO QString::iterator QString::begin()
  -- TODO QString::const_iterator QString::begin() const

  , just $ mkConstMethod "capacity" np intT

  -- TODO QString::const_iterator QString::cbegin() const
  -- TODO QString::const_iterator QString::cend() const

  , just $ mkMethod "chop" [intT] voidT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "chopped" [intT] $ objT c_QString
  , just $ mkMethod "clear" np voidT

  -- TODO compare methods
  -- TODO QString::const_iterator QString::constBegin() const
  -- TODO const QChar *QString::constData() const
  -- TODO QString::const_iterator QString::constEnd() const

  , test (qtVersion >= [5, 0]) $ mkConstMethod "toHtmlEscaped" np $ objT c_QString
  , just $ mkConstMethod "toLatin1" np $ objT c_QByteArray
  , just $ mkConstMethod "toLocal8Bit" np $ objT c_QByteArray
  , just $ mkConstMethod "toStdString" np $ objT c_string
  , just $ mkConstMethod "toUtf8" np $ objT c_QByteArray
  -- TODO CFStringRef QString::toCFString() const
  , just $ mkConstMethod "toCaseFolded" np $ objT c_QString

  , just $ mkConstMethod' "toDouble" "toDouble" np doubleT
  , just $ mkConstMethod' "toDouble" "toDoubleWithOk" [ptrT boolT] doubleT

  , just $ mkConstMethod' "toFloat" "toFloat" np floatT
  , just $ mkConstMethod' "toFloat" "toFloatWithOk" [ptrT boolT] floatT

  , just $ mkConstMethod' "toInt" "toInt" np intT
  , just $ mkConstMethod' "toInt" "toIntWithOk" [ptrT boolT] intT
  , just $ mkConstMethod' "toInt" "toIntWithOkAndBase" [ptrT boolT, intT] intT

  , just $ mkConstMethod' "toLong" "toLong" np longT
  , just $ mkConstMethod' "toLong" "toLongWithOk" [ptrT boolT] longT
  , just $ mkConstMethod' "toLong" "toLongWithOkAndBase" [ptrT boolT, intT] longT

  , just $ mkConstMethod' "toLongLong" "toLonglong" np qlonglong
  , just $ mkConstMethod' "toLongLong" "toLonglongWithOk" [ptrT boolT] qlonglong
  , just $ mkConstMethod' "toLongLong" "toLonglongWithOkAndBase" [ptrT boolT, intT] qlonglong

  , just $ mkConstMethod "toLower" np $ objT c_QString
  -- TODO NSString *QString::toNSString() const

  , just $ mkConstMethod' "toShort" "toShort" np shortT
  , just $ mkConstMethod' "toShort" "toShortWithOk" [ptrT boolT] shortT
  , just $ mkConstMethod' "toShort" "toShortWithOkAndBase" [ptrT boolT, intT] shortT

  , just $ mkConstMethod' "toUInt" "toUInt" np uintT
  , just $ mkConstMethod' "toUInt" "toUIntWithOk" [ptrT boolT] uintT
  , just $ mkConstMethod' "toUInt" "toUIntWithOkAndBase" [ptrT boolT, intT] uintT

  , just $ mkConstMethod' "toULong" "toULong" np ulongT
  , just $ mkConstMethod' "toULong" "toULongWithOk" [ptrT boolT] ulongT
  , just $ mkConstMethod' "toULong" "toULongWithOkAndBase" [ptrT boolT, intT] ulongT

  , just $ mkConstMethod' "toULongLong" "toUlonglong" np qulonglong
  , just $ mkConstMethod' "toULongLong" "toUlonglongWithOk" [ptrT boolT] qulonglong
  , just $ mkConstMethod' "toULongLong" "toUlonglongWithOkAndBase" [ptrT boolT, intT] qulonglong

  , just $ mkConstMethod' "toUShort" "toUshort" np ushortT
  , just $ mkConstMethod' "toUShort" "toUshortWithOk" [ptrT boolT] ushortT
  , just $ mkConstMethod' "toUShort" "toUshortWithOkAndBase" [ptrT boolT, intT] ushortT

  --, test (qtVersion >= [4, 2]) $ mkConstMethod "toUcs4" np $ toGcT $ objT c_QVectorUInt

  , just $ mkConstMethod "toUpper" np $ objT c_QString

  -- int QString::toWCharArray(wchar_t *array) const

  , just $ mkConstMethod "trimmed" np $ objT c_QString
  , just $ mkMethod "truncate" [intT] voidT

  --, just $ mkConstMethod "unicode" np $ objT c_QChar
  --, just $ mkConstMethod "utf16" np $ ptrT ushortT
    -- TODO Lots more method here.
  ]

e_NormalizationForm =
    makeQtEnum (ident1 "QString" "NormalizationForm") [includeStd "QString"]
    [ "NormalizationForm_D"
    , "NormalizationForm_C"
    , "NormalizationForm_KD"
    , "NormalizationForm_KC"
    ]

(e_SectionFlag, fl_SectionFlags) =
    makeQtEnumAndFlags (ident1 "QString" "SectionFlag") "SectionFlags" [includeStd "QString"]
    [ "SectionDefault"
    , "SectionSkipEmpty"
    , "SectionIncludeLeadingSep"
    , "SectionIncludeTrailingSep"
    , "SectionCaseInsensitiveSeps"
    ]

e_SplitBehavior =
    makeQtEnum (ident1 "QString" "SplitBehavior") [includeStd "QString"]
    [ "KeepEmptyParts"
    , "SkipEmptyParts"
    ]
