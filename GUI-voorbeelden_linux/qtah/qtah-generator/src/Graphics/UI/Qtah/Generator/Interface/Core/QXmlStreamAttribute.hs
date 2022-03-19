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

module Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamAttribute (
  aModule,
  c_QXmlStreamAttribute,
  ) where

import Foreign.Hoppy.Generator.Spec (
  classSetConversionToGc,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable, Assignable, Equatable),
  classAddFeatures,
  )
--import Graphics.UI.Qtah.Generator.Interface.Core.QStringRef (c_QStringRef)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Foreign.Hoppy.Generator.Types (boolT, constT, objT, refT)
import Foreign.Hoppy.Generator.Version (just, collect)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QXmlStreamAttribute"] $
  [qtExport c_QXmlStreamAttribute]

c_QXmlStreamAttribute =
  addReqIncludes [ includeStd "QXmlStreamAttribute" ] $
  classSetConversionToGc $
  classAddFeatures [Copyable, Assignable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QXmlStreamAttribute") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithQualifiedNameAndValue" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString]
  , just $ mkCtor "newWithNamespaceUriAndNameAndValue" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, refT $ constT $ objT c_QString]
  , just $ mkConstMethod "isDefault" np boolT
  --, just $ mkConstMethod "name" np $ objT c_QStringRef
  --, just $ mkConstMethod "namespaceUri" np $ objT c_QStringRef
  --, just $ mkConstMethod "prefix" np $ objT c_QStringRef
  --, just $ mkConstMethod "qualifiedName" np $ objT c_QStringRef
  --, just $ mkConstMethod "value" np $ objT c_QStringRef
  ]
