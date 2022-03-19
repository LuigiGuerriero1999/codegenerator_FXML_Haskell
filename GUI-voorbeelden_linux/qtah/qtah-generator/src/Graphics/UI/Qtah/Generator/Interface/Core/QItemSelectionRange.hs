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

module Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionRange (
  aModule,
  c_QItemSelectionRange,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, constT, intT, objT, ptrT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQModelIndex)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (
  c_QAbstractItemModel,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QPersistentModelIndex (c_QPersistentModelIndex)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QItemSelectionRange"]
  [ qtExport c_QItemSelectionRange ]

c_QItemSelectionRange =
  addReqIncludes [includeStd "QItemSelectionRange"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QItemSelectionRange") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithIndex" [objT c_QModelIndex]
  , just $ mkCtor "newWithIndices" [objT c_QModelIndex, objT c_QModelIndex]
  , just $ mkConstMethod "bottom" np intT
  , just $ mkConstMethod "bottomRight" np $
    if qtVersion >= [5, 0]
    then refT $ constT $ objT c_QPersistentModelIndex
    else objT c_QModelIndex
  , just $ mkConstMethod' "contains" "containsIndex" [objT c_QModelIndex] boolT
  , just $ mkConstMethod' "contains" "containsBelowParent" [intT, intT, objT c_QModelIndex] boolT
  , just $ mkConstMethod "height" np intT
  , just $ mkConstMethod "indexes" np $ objT c_QListQModelIndex
  , just $ mkConstMethod "intersected" [objT c_QItemSelectionRange] $ objT c_QItemSelectionRange
  , just $ mkConstMethod "intersects" [objT c_QItemSelectionRange] boolT
  , just $ mkConstMethod "isEmpty" np boolT
  , just $ mkConstMethod "isValid" np boolT
  , just $ mkConstMethod "left" np intT
  , just $ mkConstMethod "model" np $ ptrT $ constT $ objT c_QAbstractItemModel
  , just $ mkConstMethod "parent" np $ objT c_QModelIndex
  , just $ mkConstMethod "right" np intT
  , test (qtVersion >= [5, 6]) $ mkMethod "swap" [refT $ objT c_QItemSelectionRange] voidT
  , just $ mkConstMethod "top" np intT
  , just $ mkConstMethod "topLeft" np $
    if qtVersion >= [5, 0]
    then refT $ constT $ objT c_QPersistentModelIndex
    else objT c_QModelIndex
  , just $ mkConstMethod "width" np intT
  ]
