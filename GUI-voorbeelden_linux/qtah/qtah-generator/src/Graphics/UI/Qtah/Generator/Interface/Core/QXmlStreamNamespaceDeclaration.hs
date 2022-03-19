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

module Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamNamespaceDeclaration (
  aModule,
  c_QXmlStreamNamespaceDeclaration,
  qXmlStreamNamespaceDeclarations,
  ) where

import Foreign.Hoppy.Generator.Spec (
  classSetConversionToGc,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable, Assignable, Equatable),
  classAddFeatures,
  )
--import Graphics.UI.Qtah.Generator.Interface.Core.QStringRef (c_QStringRef)
import Foreign.Hoppy.Generator.Types (constT, objT, refT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorQXmlStreamNamespaceDeclaration)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QXmlStreamNamespaceDeclaration"]
  [qtExport c_QXmlStreamNamespaceDeclaration]

c_QXmlStreamNamespaceDeclaration =
  addReqIncludes [ includeStd "QXmlStreamNamespaceDeclaration" ] $
  classSetConversionToGc $
  classAddFeatures [Copyable, Assignable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QXmlStreamNamespaceDeclaration") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithPrefixAndNamespaceUri" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString]
  --, just $ mkConstMethod "namespaceUri" np $ objT c_QStringRef
  --, just $ mkConstMethod "prefix" np $ objT c_QStringRef
  ]

qXmlStreamNamespaceDeclarations = c_QVectorQXmlStreamNamespaceDeclaration
