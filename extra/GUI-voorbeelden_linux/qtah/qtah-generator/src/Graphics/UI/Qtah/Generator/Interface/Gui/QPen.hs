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

module Graphics.UI.Qtah.Generator.Interface.Gui.QPen (
  aModule,
  c_QPen,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (enumT, intT, objT, refT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  e_PenCapStyle,
  e_PenJoinStyle,
  e_PenStyle,
  qreal,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QBrush (c_QBrush)
import Graphics.UI.Qtah.Generator.Interface.Gui.QColor (c_QColor)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QPen"]
  [ qtExport c_QPen ]

c_QPen =
  addReqIncludes [includeStd "QPen"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QPen") Nothing []
  [ mkCtor "new" np
  , mkCtor "newWithColor" [objT c_QColor]
  , mkProp "brush" $ objT c_QBrush
  , mkProp "capStyle" $ enumT e_PenCapStyle
  , mkProp "color" $ objT c_QColor
  , mkBoolIsProp "cosmetic"
  , mkProp "dashOffset" qreal
  -- TODO mkProp "dashPattern" $ objT c_QVectorQreal (QVector's not convertible yet.)
  , mkProp "joinStyle" $ enumT e_PenJoinStyle
  , mkProp "miterLimit" qreal
  , mkProp "style" $ enumT e_PenStyle
  , mkMethod "swap" [refT $ objT c_QPen] voidT
  , mkProp "width" intT
  , mkProp "widthF" qreal
  ]
