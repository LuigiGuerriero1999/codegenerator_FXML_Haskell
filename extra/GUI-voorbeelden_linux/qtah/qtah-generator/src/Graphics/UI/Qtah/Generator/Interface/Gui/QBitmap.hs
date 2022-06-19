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

module Graphics.UI.Qtah.Generator.Interface.Gui.QBitmap (
  aModule,
  c_QBitmap,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkStaticMethod',
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (
  enumT,
  intT,
  objT,
  refT,
  voidT,
  constT,
  ptrT,
  charT,
  ucharT
  )
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_ImageConversionFlags)
import Graphics.UI.Qtah.Generator.Interface.Gui.QImage (c_QImage, e_Format)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import Graphics.UI.Qtah.Generator.Interface.Gui.QTransform (c_QTransform)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QBitmap"]
  [ qtExport c_QBitmap
  ]

c_QBitmap =
  addReqIncludes [includeStd "QBitmap"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QBitmap") Nothing [c_QPixmap] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithSize" [objT c_QSize]
  , just $ mkCtor "newWithSizeRaw" [intT, intT]
  , just $ mkCtor "newWithFile" [objT c_QString]
    -- TODO QString wrapper:
  , just $ mkCtor "newWithFileAndFormat" [objT c_QString, ptrT $ constT charT ]
  , just $ mkMethod "clear" np voidT
  , just $ mkMethod "swap" [refT $ objT c_QBitmap] voidT
  , just $ mkConstMethod "transformed" [objT c_QTransform] $ objT c_QBitmap
  , just $ mkStaticMethod' "fromData" "fromData" [objT c_QSize, ptrT $ constT ucharT] $ objT c_QBitmap
  , just $ mkStaticMethod' "fromData" "fromDataAll" [objT c_QSize, ptrT $ constT ucharT, enumT e_Format] $ objT c_QBitmap
  , just $ mkStaticMethod' "fromImage" "fromImage" [objT c_QImage] $ objT c_QBitmap
  , just $ mkStaticMethod' "fromImage" "fromImageAll" [objT c_QImage, flagsT fl_ImageConversionFlags] $ objT c_QBitmap
  ]
