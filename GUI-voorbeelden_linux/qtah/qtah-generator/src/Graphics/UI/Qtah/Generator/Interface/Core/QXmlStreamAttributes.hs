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

module Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamAttributes (
  aModule,
  c_QXmlStreamAttributes,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod',
  mkCtor,
  mkMethod',
  np,
  )
--import Graphics.UI.Qtah.Generator.Interface.Core.QStringRef (c_QStringRef)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QLatin1String (c_QLatin1String)
import Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorQXmlStreamAttribute)
import Foreign.Hoppy.Generator.Types (boolT, voidT, constT, objT, refT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QXmlStreamAttributes"]
  [qtExport c_QXmlStreamAttributes]

c_QXmlStreamAttributes =
  addReqIncludes [ includeStd "QXmlStreamAttributes" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QXmlStreamAttributes") Nothing [c_QVectorQXmlStreamAttribute] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkMethod' "append" "appendWithNamespaceUriAndNameAndValue" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "append" "appendWithQualifiedNameAndValue" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkConstMethod' "hasAttribute" "hasAttribute" [refT $ constT $ objT c_QString] boolT
  , just $ mkConstMethod' "hasAttribute" "hasAttributeWithLatin1String" [objT c_QLatin1String] boolT
  , just $ mkConstMethod' "hasAttribute" "hasAttributeWithNamespaceUriAndName" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] boolT
  --, just $ mkConstMethod' "value" "valueWithNamespaceUriAndName" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] $ objT c_QStringRef
  --, just $ mkConstMethod' "value" "valueWithNamespaceUriAndLatin1StringName" [refT $ constT $ objT c_QString, objT c_QLatin1String] $ objT c_QStringRef
  --, just $ mkConstMethod' "value" "valueWithLatin1Strings" [objT c_QLatin1String, objT c_QLatin1String] $ objT c_QStringRef
  --, just $ mkConstMethod' "value" "valueWithQualifiedName)" [refT $ constT $ objT c_QString] $ objT c_QStringRef
  --, just $ mkConstMethod' "value" "valueWithLatin1String" [objT c_QLatin1String] $ objT c_QStringRef
  ]
