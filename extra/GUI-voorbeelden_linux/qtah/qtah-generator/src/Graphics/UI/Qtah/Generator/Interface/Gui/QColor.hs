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

module Graphics.UI.Qtah.Generator.Interface.Gui.QColor (
  aModule,
  c_QColor,
  qrgb,
  ) where

import Control.Monad (forM_)
import Foreign.Hoppy.Generator.Language.Haskell (
  addImports,
  indent,
  sayLn,
  saysLn,
  )
import Foreign.Hoppy.Generator.Spec (
  ClassHaskellConversion (
    ClassHaskellConversion,
    classHaskellConversionFromCppFn,
    classHaskellConversionToCppFn,
    classHaskellConversionType
  ),
  Purity (Pure),
  addReqIncludes,
  classSetEntityPrefix,
  classSetHaskellConversion,
  hsImports,
  hsQualifiedImport,
  ident,
  ident1,
  includeStd,
  makeClass,
  makeFn,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkStaticMethod,
  np,
  toExtName,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, uintT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_GlobalColor, qreal)
import Graphics.UI.Qtah.Generator.Interface.Imports
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QColor"] $
  collect
  [ just $ qtExport c_QColor
  , test (qtVersion >= [5, 2]) $ qtExport e_NameFormat
  , just $ qtExport e_Spec
  , just $ qtExport f_qAlpha
  , just $ qtExport f_qBlue
  , just $ qtExport f_qGray
  , just $ qtExport f_qGrayFromRgb
  , just $ qtExport f_qGreen
  , test (qtVersion >= [5, 3]) $ qtExport f_qPremultiply
  , just $ qtExport f_qRed
  , just $ qtExport f_qRgb
  , just $ qtExport f_qRgba
  , test (qtVersion >= [5, 3]) $ qtExport f_qUnpremultiply
  ]

c_QColor =
  addReqIncludes [includeStd "QColor"] $
  classSetHaskellConversion conversion $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QColor") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newQRgb" [qrgb]
  , just $ mkCtor "newRgb" [intT, intT, intT]
  , just $ mkCtor "newRgba" [intT, intT, intT, intT]
  , just $ mkCtor "newNamedColor" [objT c_QString]
  , just $ mkCtor "newGlobalColor" [enumT e_GlobalColor]
  , just $ mkProp "alpha" intT
  , just $ mkProp "alphaF" qreal
  , just $ mkConstMethod "black" np intT
  , just $ mkConstMethod "blackF" np qreal
  , just $ mkProp "blue" intT
  , just $ mkProp "blueF" qreal
  , just $ mkStaticMethod "colorNames" np $ objT c_QStringList
  , just $ mkConstMethod "convertTo" [enumT e_Spec] $ objT c_QColor
  , just $ mkConstMethod "cyan" np intT
  , just $ mkConstMethod "cyanF" np qreal
  , test (qtVersion >= [4, 3]) $ mkConstMethod' "darker" "darker" np $ objT c_QColor
  , test (qtVersion >= [4, 3]) $ mkConstMethod' "darker" "darkerBy" [intT] $ objT c_QColor
  , just $ mkProp "green" intT
  , just $ mkProp "greenF" qreal
  , test (qtVersion >= [4, 6]) $ mkConstMethod "hslHue" np intT
  , test (qtVersion >= [4, 6]) $ mkConstMethod "hslHueF" np qreal
  , test (qtVersion >= [4, 6]) $ mkConstMethod "hslSaturation" np intT
  , test (qtVersion >= [4, 6]) $ mkConstMethod "hslSaturationF" np qreal
  , just $ mkConstMethod "hsvHue" np intT
  , just $ mkConstMethod "hsvHueF" np qreal
  , just $ mkConstMethod "hsvSaturation" np intT
  , just $ mkConstMethod "hsvSaturationF" np qreal
  , just $ mkConstMethod "hue" np intT
  , just $ mkConstMethod "hueF" np qreal
  , just $ mkConstMethod "isValid" np boolT
  , test (qtVersion >= [4, 7]) $ mkStaticMethod "isValidColor" [objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' "lighter" "lighter" np $ objT c_QColor
  , test (qtVersion >= [4, 3]) $ mkConstMethod' "lighter" "lighterBy" [intT] $ objT c_QColor
  , test (qtVersion >= [4, 6]) $ mkConstMethod "lightness" np intT
  , test (qtVersion >= [4, 6]) $ mkConstMethod "lightnessF" np qreal
  , just $ mkConstMethod "magenta" np intT
  , just $ mkConstMethod "magentaF" np qreal
  , just $ mkConstMethod' "name" "name" np $ objT c_QString
  , test (qtVersion >= [5, 2]) $
    mkConstMethod' "name" "nameWithFormat" [enumT e_NameFormat] $ objT c_QString
  , just $ mkProp "red" intT
  , just $ mkProp "redF" qreal
  , just $ mkConstMethod "rgb" np qrgb
  , just $ mkConstMethod "rgba" np qrgb
  , just $ mkConstMethod "saturation" np intT
  , just $ mkConstMethod "saturationF" np qreal
  , just $ mkMethod' "setCmyk" "setCmyk" [intT, intT, intT, intT] voidT
  , just $ mkMethod' "setCmyk" "setCmyka" [intT, intT, intT, intT, intT] voidT
  , just $ mkMethod' "setCmykF" "setCmykF" [qreal, qreal, qreal, qreal] voidT
  , just $ mkMethod' "setCmykF" "setCmykaF" [qreal, qreal, qreal, qreal, qreal] voidT
  , test (qtVersion >= [4, 6]) $ mkMethod' "setHsl" "setHsl" [intT, intT, intT] voidT
  , test (qtVersion >= [4, 6]) $ mkMethod' "setHsl" "setHsla" [intT, intT, intT, intT] voidT
  , test (qtVersion >= [4, 6]) $ mkMethod' "setHslF" "setHslF" [qreal, qreal, qreal] voidT
  , test (qtVersion >= [4, 6]) $ mkMethod' "setHslF" "setHslaF" [qreal, qreal, qreal, qreal] voidT
  , just $ mkMethod' "setHsv" "setHsv" [intT, intT, intT] voidT
  , just $ mkMethod' "setHsv" "setHsva" [intT, intT, intT, intT] voidT
  , just $ mkMethod' "setHsvF" "setHsvF" [qreal, qreal, qreal] voidT
  , just $ mkMethod' "setHsvF" "setHsvaF" [qreal, qreal, qreal, qreal] voidT
  , just $ mkMethod "setNamedColor" [objT c_QString] voidT
  , just $ mkMethod' "setRgb" "setQRgb" [qrgb] voidT
  , just $ mkMethod' "setRgb" "setQRgba" [qrgb] voidT
  , just $ mkMethod' "setRgb" "setRgb" [intT, intT, intT] voidT
  , just $ mkMethod' "setRgb" "setRgba" [intT, intT, intT, intT] voidT
  , just $ mkMethod' "setRgbF" "setRgbF" [qreal, qreal, qreal] voidT
  , just $ mkMethod' "setRgbF" "setRgbaF" [qreal, qreal, qreal, qreal] voidT
  , just $ mkConstMethod "spec" np $ enumT e_Spec
  , just $ mkConstMethod "toCmyk" np $ objT c_QColor
  , just $ mkConstMethod "toHsl" np $ objT c_QColor
  , just $ mkConstMethod "toHsv" np $ objT c_QColor
  , just $ mkConstMethod "toRgb" np $ objT c_QColor
  , just $ mkConstMethod "value" np intT
  , just $ mkConstMethod "valueF" np qreal
  , just $ mkConstMethod "yellow" np intT
  , just $ mkConstMethod "yellowF" np qreal
  ]

  where
    components =
      [ ("Invalid", "", [])
      , ("Rgb", "rgba", ["red", "green", "blue", "alpha"])
      , ("Cmyk", "cmyka", ["cyan", "magenta", "yellow", "black", "alpha"])
      , ("Hsl", "hsla", ["hslHue", "hslSaturation", "lightness", "alpha"])
      , ("Hsv", "hsva", ["hsvHue", "hsvSaturation", "value", "alpha"])
      ]

    hColorImport = hsQualifiedImport "Graphics.UI.Qtah.Gui.HColor" "HColor"

    conversion =
      ClassHaskellConversion
      { classHaskellConversionType = Just $ do
        addImports hColorImport
        return $ HsTyCon $ UnQual $ HsIdent "HColor.HColor"

      , classHaskellConversionToCppFn = Just $ do
        addImports $ mconcat [importForPrelude,
                              importForRuntime,
                              hColorImport]
        sayLn "\\color' -> do"
        indent $ do
          sayLn "this' <- new"
          sayLn "case color' of"
          indent $ forM_ components $ \(spec, letters, _) ->
            saysLn $ concat
            [ ["HColor.", spec]
            , map (\var -> [' ', var, '\'']) letters
            , if null letters
              then [" -> QtahP.return ()"]
              else [" -> set", spec, "a this'"]
            , concatMap (\var -> [" (QtahFHR.coerceIntegral ", [var], "')"]) letters
            ]
          sayLn "QtahP.return this'"

      , classHaskellConversionFromCppFn = Just $ do
        addImports $ mconcat [hsImports "Prelude" ["($)", "(++)", "(>>=)"],
                              importForPrelude,
                              importForRuntime,
                              hColorImport]
        sayLn "\\this' -> spec this' >>= \\spec' -> case spec' of"
        indent $ do
          forM_ components $ \(spec, letters, getters) -> do
            saysLn [spec, " -> do"]
            indent $ do
              forM_ (zip letters getters) $ \(var, get) ->
                saysLn [[var], "' <- QtahP.fmap QtahFHR.coerceIntegral $ ", get, " this'"]
              saysLn $ ["QtahP.return $ HColor.", spec] ++ map (\var -> [' ', var, '\'']) letters
          saysLn ["_ -> QtahP.error $ \"HColor's Decodable instance doesn't understand ",
                  "QColor::Spec \" ++ QtahP.show spec' ++ \".\""]
      }

-- Introduced in Qt 5.2.
e_NameFormat =
  makeQtEnum (ident1 "QColor" "NameFormat") [includeStd "QColor"]
  [ "HexRgb"
  , "HexArgb"
  ]

e_Spec =
  makeQtEnum (ident1 "QColor" "Spec") [includeStd "QColor"]
  [ "Invalid"
  , "Rgb"
  , "Hsv"
  , "Cmyk"
  , "Hsl"
  ]

-- | This is a typedef QRgb that holds a AARRGGBB value and is
-- "equivalent to an unsigned int."
qrgb = uintT

-- TODO Qrgba64 overloads (and methods above).

f_qAlpha = makeFn (ident "qAlpha") Nothing Pure [qrgb] intT

f_qBlue = makeFn (ident "qBlue") Nothing Pure [qrgb] intT

f_qGray = makeFn (ident "qGray") Nothing Pure [qrgb] intT

f_qGrayFromRgb =
  makeFn (ident "qGray") (Just $ toExtName "qGrayFromRgb") Pure
  [intT, intT, intT] intT

f_qGreen = makeFn (ident "qGreen") Nothing Pure [qrgb] intT

f_qPremultiply = makeFn (ident "qPremultiply") Nothing Pure [qrgb] qrgb

f_qRed = makeFn (ident "qRed") Nothing Pure [qrgb] intT

f_qRgb = makeFn (ident "qRgb") Nothing Pure [intT, intT, intT] qrgb

f_qRgba = makeFn (ident "qRgba") Nothing Pure [intT, intT, intT, intT] qrgb

f_qUnpremultiply = makeFn (ident "qUnpremultiply") Nothing Pure [qrgb] qrgb
