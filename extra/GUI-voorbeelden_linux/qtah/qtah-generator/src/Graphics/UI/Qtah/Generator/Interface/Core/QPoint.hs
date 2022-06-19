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

module Graphics.UI.Qtah.Generator.Interface.Core.QPoint (
  aModule,
  c_QPoint,
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
  Operator (OpAddAssign, OpDivideAssign, OpMultiplyAssign, OpSubtractAssign),
  addReqIncludes,
  classSetEntityPrefix,
  classSetHaskellConversion,
  hsImports,
  hsQualifiedImport,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkStaticMethod,
  np,
  operatorPreferredExtName',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
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
  makeQtModule ["Core", "QPoint"]
  [ qtExport c_QPoint ]

c_QPoint =
  addReqIncludes [includeStd "QPoint"] $
  classSetHaskellConversion
    ClassHaskellConversion
    { classHaskellConversionType = Just $ do
      addImports $ hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint"
      return $ HsTyCon $ UnQual $ HsIdent "HPoint.HPoint"
    , classHaskellConversionToCppFn = Just $ do
      addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                            hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint"]
      sayLn "new <$> HPoint.x <*> HPoint.y"
    , classHaskellConversionFromCppFn = Just $ do
      addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                            hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint"]
      sayLn "\\q -> HPoint.HPoint <$> x q <*> y q"
    } $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QPoint") Nothing [] $
  collect
  [ just $ mkCtor "newNull" np
  , just $ mkCtor "new" [intT, intT]
  , test (qtVersion >= [5, 1]) $ mkStaticMethod "dotProduct" [objT c_QPoint, objT c_QPoint] intT
  , just $ mkConstMethod "isNull" np boolT
  , just $ mkConstMethod "manhattanLength" np intT
  , just $ mkProp "x" intT
  , just $ mkProp "y" intT
  , just $ mkMethod OpAddAssign [objT c_QPoint] $ refT $ objT c_QPoint
  , just $ mkMethod OpSubtractAssign [objT c_QPoint] $ refT $ objT c_QPoint
  , just $ mkMethod' OpMultiplyAssign (operatorPreferredExtName' OpMultiplyAssign)
    [intT] $ refT $ objT c_QPoint
  , just $ mkMethod' OpMultiplyAssign (operatorPreferredExtName' OpMultiplyAssign ++ "Real")
    [qreal] $ refT $ objT c_QPoint
  , just $ mkMethod OpDivideAssign [qreal] $ refT $ objT c_QPoint
  ]
