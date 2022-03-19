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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractGraphicsShapeItem (
  aModule,
  c_QAbstractGraphicsShapeItem,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Types (objT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Gui.QBrush (c_QBrush)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPen (c_QPen)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsItem (c_QGraphicsItem)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractGraphicsShapeItem"]
  [ qtExport c_QAbstractGraphicsShapeItem
  ]

c_QAbstractGraphicsShapeItem =
  addReqIncludes [includeStd "QAbstractGraphicsShapeItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractGraphicsShapeItem") Nothing [c_QGraphicsItem]
  [ mkConstMethod "brush" np $ objT c_QBrush
  , mkConstMethod "pen" np $ objT c_QPen
  , mkMethod "setBrush" [objT c_QBrush] voidT
  , mkMethod "setPen" [objT c_QPen] voidT
  ]
