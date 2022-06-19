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

module Graphics.UI.Qtah.Generator.Interface.Gui.QBackingStore (
  aModule,
  c_QBackingStore,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  intT,
  objT,
  ptrT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDevice (c_QPaintDevice)
import Graphics.UI.Qtah.Generator.Interface.Gui.QRegion (c_QRegion)
import Graphics.UI.Qtah.Generator.Interface.Gui.QWindow (c_QWindow)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QBackingStore"]
  [ qtExport c_QBackingStore
  ]

c_QBackingStore =
  addReqIncludes [includeStd "QBackingStore"] $
  classSetEntityPrefix "" $
  makeClass (ident "QBackingStore") Nothing [] $
  collect
  [ just $ mkCtor "new" [ptrT $ objT c_QWindow]
  , just $ mkMethod "beginPaint" [objT c_QRegion] voidT
  , just $ mkMethod "endPaint" np voidT
  , just $ mkMethod' "flush" "flush" [objT c_QRegion] voidT
  , just $ mkMethod' "flush" "flushWithWindow" [objT c_QRegion, ptrT $ objT c_QWindow] voidT
  , just $ mkMethod' "flush" "flushAll" [objT c_QRegion, ptrT $ objT c_QWindow, objT c_QPoint] voidT
  -- TODO QPlatformBackingStore* handle() const
  , just $ mkConstMethod "hasStaticContents" np boolT
  , just $ mkMethod "paintDevice" np $ ptrT $ objT c_QPaintDevice
  , just $ mkMethod "resize" [objT c_QSize] voidT
  , just $ mkMethod "scroll" [objT c_QRegion, intT, intT] boolT
  , just $ mkMethod "setStaticContents" [objT c_QRegion] voidT
  , just $ mkConstMethod "size" np $ objT c_QSize
  , just $ mkConstMethod "staticContents" np $ objT c_QRegion
  , just $ mkConstMethod "window" np $ ptrT $ objT c_QWindow
  ]
