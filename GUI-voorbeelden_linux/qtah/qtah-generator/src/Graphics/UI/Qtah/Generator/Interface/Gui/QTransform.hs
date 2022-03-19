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

module Graphics.UI.Qtah.Generator.Interface.Gui.QTransform (
  aModule,
  c_QTransform,
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
import Foreign.Hoppy.Generator.Types (boolT, refT, objT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QTransform"]
  [ qtExport c_QTransform ]

c_QTransform =
  addReqIncludes [includeStd "QTransform"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QTransform") Nothing []
  [ mkCtor "new2x2" [qreal, qreal, qreal, qreal, qreal, qreal]
  , mkConstMethod "inverted" np $ objT c_QTransform  -- TODO Version that can fail.
  , mkConstMethod "isInvertible" np boolT
  , mkConstMethod' "map" "mapPoint" [objT c_QPoint] $ objT c_QPoint
  , mkConstMethod' "map" "mapPointF" [objT c_QPointF] $ objT c_QPointF
  , mkMethod "scale" [qreal, qreal] $ refT $ objT c_QTransform
  , mkMethod "translate" [qreal, qreal] $ refT $ objT c_QTransform
  -- TODO More methods.
  ]
