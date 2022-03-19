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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsPixmapItem (
  aModule,
  c_QGraphicsPixmapItem,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (enumT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_TransformationMode, qreal)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsItem (c_QGraphicsItem)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGraphicsPixmapItem"]
  [ qtExport c_QGraphicsPixmapItem
  , qtExport e_ShapeMode
  ]

c_QGraphicsPixmapItem =
  addReqIncludes [includeStd "QGraphicsPixmapItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGraphicsPixmapItem") Nothing [c_QGraphicsItem]
  [ mkCtor "new" np
  , mkCtor "newWithParent" [ptrT $ objT c_QGraphicsItem]
  , mkCtor "newWithPixmap" [objT c_QPixmap]
  , mkCtor "newWithPixmapAndParent" [objT c_QPixmap, ptrT $ objT c_QGraphicsItem]
  , mkProp "offset" $ objT c_QPointF
  , mkProp "pixmap" $ objT c_QPixmap
  , mkMethod' "setOffset" "setOffsetRaw" [qreal, qreal] voidT
  , mkProp "shapeMode" $ enumT e_ShapeMode
  , mkProp "transformationMode" $ enumT e_TransformationMode
  ]

e_ShapeMode =
  makeQtEnum (ident1 "QGraphicsPixmapItem" "ShapeMode") [includeStd "QGraphicsPixmapItem"]
  [ "MaskShape"
  , "BoundingRectShape"
  , "HeuristicMaskShape"
  ]
