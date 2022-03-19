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

module Graphics.UI.Qtah.Generator.Interface.Core.QProcessEnvironment (
  aModule,
  c_QProcessEnvironment,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  classSetConversionToGc,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkStaticMethod,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, objT, refT, voidT, constT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QProcessEnvironment"] [4, 6]
  [qtExport c_QProcessEnvironment]

c_QProcessEnvironment =
  addReqIncludes [includeStd "QProcessEnvironment"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QProcessEnvironment") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkMethod "clear" np voidT
  , just $ mkConstMethod "contains" [refT $ constT $ objT c_QString] boolT
  , just $ mkMethod' "insert" "insertWithName" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , test (qtVersion >= [4, 8]) $ mkMethod' "insert" "insert" [refT $ constT $ objT c_QProcessEnvironment] voidT
  , just $ mkConstMethod "isEmpty" np boolT
  , test (qtVersion >= [4, 8]) $ mkConstMethod "keys" np $ objT c_QStringList
  , just $ mkMethod "remove" [refT $ constT $ objT c_QString] voidT
  , test (qtVersion >= [5, 0]) $ mkMethod "swap" [refT $ objT c_QProcessEnvironment] voidT
  , just $ mkConstMethod "toStringList" np $ objT c_QStringList
  , just $ mkConstMethod' "value" "value" [refT $ constT $ objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "value" "valueWithDefault" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] $ objT c_QString
  , test (qtVersion >= [4, 6]) $ mkStaticMethod "systemEnvironment" np $ objT c_QProcessEnvironment
  ]
