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

module Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (
  aModule,
  c_QModelIndex,
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
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  constT,
  enumT,
  intT,
  objT,
  ptrT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (
  c_QAbstractItemModel,
  )
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_ItemDataRole, fl_ItemFlags)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QModelIndex"]
  [ qtExport c_QModelIndex ]

c_QModelIndex =
  addReqIncludes [includeStd "QModelIndex"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QModelIndex") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkConstMethod "child" [intT, intT] $ objT c_QModelIndex
  , just $ mkConstMethod "column" np intT
  , just $ mkConstMethod' "data" "getData" np $ objT c_QVariant
  , just $ mkConstMethod' "data" "getDataWithRole" [enumT e_ItemDataRole] $ objT c_QVariant
  , test (qtVersion >= [4, 2]) $ mkConstMethod "flags" np $ flagsT fl_ItemFlags
    -- TODO internalId
    -- TODO internalPointer
  , just $ mkConstMethod "isValid" np boolT
  , just $ mkConstMethod "model" np $ ptrT $ constT $ objT c_QAbstractItemModel
  , just $ mkConstMethod "parent" np $ objT c_QModelIndex
  , just $ mkConstMethod "row" np intT
  , just $ mkConstMethod "sibling" [intT, intT] $ objT c_QModelIndex
  ]
