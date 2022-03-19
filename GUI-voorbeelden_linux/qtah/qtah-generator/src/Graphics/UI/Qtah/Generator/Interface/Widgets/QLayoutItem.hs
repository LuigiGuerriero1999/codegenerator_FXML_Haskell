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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QLayoutItem (
  aModule,
  c_QLayoutItem,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_Alignment, fl_Orientations)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QLayout (c_QLayout)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QLayoutItem"]
  [ qtExport c_QLayoutItem ]

c_QLayoutItem =
  addReqIncludes [includeStd "QLayoutItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QLayoutItem") Nothing []
  -- Abstract.
  [ mkProp "alignment" $ flagsT fl_Alignment
    -- TODO controlTypes
  , mkConstMethod "expandingDirections" np $ flagsT fl_Orientations
  , mkProp "geometry" $ objT c_QRect
  , mkConstMethod "hasHeightForWidth" np boolT
  , mkConstMethod "heightForWidth" [intT] intT
  , mkMethod "invalidate" np voidT
  , mkConstMethod "isEmpty" np boolT
  , mkMethod "layout" np $ ptrT $ objT c_QLayout
  , mkConstMethod "maximumSize" np $ objT c_QSize
  , mkConstMethod "minimumHeightForWidth" [intT] intT
  , mkConstMethod "minimumSize" np $ objT c_QSize
  , mkConstMethod "sizeHint" np $ objT c_QSize
    -- TODO spacerItem
  , mkMethod "widget" np $ ptrT $ objT c_QWidget
  ]
