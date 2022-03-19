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

module Graphics.UI.Qtah.Generator.Interface.Core.QMetaClassInfo (
  aModule,
  c_QMetaClassInfo,
  ) where

import Foreign.Hoppy.Generator.Spec (
  MethodApplicability (MConst),
  Purity (Nonpure),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (objT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QMetaClassInfo"]
  [ qtExport c_QMetaClassInfo ]

c_QMetaClassInfo =
  addReqIncludes [ includeStd "QMetaClassInfo"
                 , includeLocal "wrap_qmetaclassinfo.hpp"
                 ] $
  classSetConversionToGc $
  classAddFeatures [Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QMetaClassInfo") Nothing []
  [ makeFnMethod (ident2 "qtah" "qmetaclassinfo" "name") "name" MConst Nonpure
    [objT c_QMetaClassInfo] $ objT c_QString
  , makeFnMethod (ident2 "qtah" "qmetaclassinfo" "value") "value" MConst Nonpure
    [objT c_QMetaClassInfo] $ objT c_QString
  ]
