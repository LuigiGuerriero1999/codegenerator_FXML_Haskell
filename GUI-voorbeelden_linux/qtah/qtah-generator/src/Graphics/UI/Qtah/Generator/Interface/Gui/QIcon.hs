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

module Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (
  aModule,
  c_QIcon,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkStaticMethod,
  mkStaticMethod',
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  enumT,
  intT,
  int64T,
  objT,
  ptrT,
  refT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_Alignment)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPainter (c_QPainter)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Gui.QWindow (c_QWindow)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QIcon"]
  [ qtExport c_QIcon
  , qtExport e_Mode
  , qtExport e_State
  ]

c_QIcon =
  addReqIncludes [includeStd "QIcon"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QIcon") Nothing [] $
  collect
  [ just $ mkCtor "new" np
    -- TODO QIcon(QIconEngine*)
  , just $ mkCtor "newWithFile" [objT c_QString]
  , just $ mkCtor "newWithPixmap" [objT c_QPixmap]
  , just $ mkConstMethod' "actualSize" "actualSize" [objT c_QSize] $ objT c_QSize
  , just $ mkConstMethod' "actualSize" "actualSizeAll"
    [objT c_QSize, enumT e_Mode, enumT e_State] $ objT c_QSize
  , test (qtVersion >= [5, 1]) $ mkConstMethod' "actualSize" "actualSizeWithWindow"
    [ptrT $ objT c_QWindow, objT c_QSize] $ objT c_QSize
  , test (qtVersion >= [5, 1]) $ mkConstMethod' "actualSize" "actualSizeWithWindowAll"
    [ptrT $ objT c_QWindow, objT c_QSize, enumT e_Mode, enumT e_State] $ objT c_QSize
  , just $ mkMethod' "addFile" "addFile" [objT c_QString] voidT
  , just $ mkMethod' "addFile" "addFileAll"
    [objT c_QString, objT c_QSize, enumT e_Mode, enumT e_State]
    voidT
  , just $ mkMethod' "addPixmap" "addPixmap" [objT c_QPixmap] voidT
  , just $ mkMethod' "addPixmap" "addPixmapAll" [objT c_QPixmap, enumT e_Mode, enumT e_State] voidT
  , just $ mkConstMethod' "availableSizes" "availableSizes" np $ objT c_QListQSize
  , just $ mkConstMethod' "availableSizes" "availableSizesAll" [enumT e_Mode, enumT e_State] $
    objT c_QListQSize
  , just $ mkConstMethod "cacheKey" np int64T
  , just $ mkStaticMethod' "fromTheme" "fromTheme" [objT c_QString] $ objT c_QIcon
  , just $ mkStaticMethod' "fromTheme" "fromThemeWithFallback" [objT c_QString, objT c_QIcon] $
    objT c_QIcon
  , just $ mkStaticMethod "hasThemeIcon" [objT c_QString] boolT
  , test (qtVersion >= [5, 6]) $ mkConstMethod "isMask" np boolT
  , just $ mkConstMethod "isNull" np boolT
  , just $ mkConstMethod "name" np $ objT c_QString
  , just $ mkConstMethod' "paint" "paintWithRect"
    [ptrT $ objT c_QPainter, objT c_QRect] voidT
  , just $ mkConstMethod' "paint" "paintWithRectAll"
    [ptrT $ objT c_QPainter, objT c_QRect, flagsT fl_Alignment, enumT e_Mode, enumT e_State]
    voidT
  , just $ mkConstMethod' "paint" "paintWithRaw"
    [ptrT $ objT c_QPainter, intT, intT, intT, intT] voidT
  , just $ mkConstMethod' "paint" "paintWithRawAll"
    [ptrT $ objT c_QPainter, intT, intT, intT, intT, flagsT fl_Alignment, enumT e_Mode,
     enumT e_State]
    voidT
  , test (qtVersion >= [5, 1]) $ mkConstMethod' "pixmap" "pixmapExtent" [intT] $ objT c_QPixmap
  , test (qtVersion >= [5, 1]) $ mkConstMethod' "pixmap" "pixmapExtentAll"
    [intT, enumT e_Mode, enumT e_State] $ objT c_QPixmap
  , just $ mkConstMethod' "pixmap" "pixmapRaw" [intT, intT] $ objT c_QPixmap
  , just $ mkConstMethod' "pixmap" "pixmapRawAll" [intT, intT, enumT e_Mode, enumT e_State] $
    objT c_QPixmap
  , just $ mkConstMethod' "pixmap" "pixmapSize" [objT c_QSize] $ objT c_QPixmap
  , just $ mkConstMethod' "pixmap" "pixmapSizeAll" [objT c_QSize, enumT e_Mode, enumT e_State] $
    objT c_QPixmap
  , test (qtVersion >= [5, 6]) $ mkMethod "setIsMask" [boolT] voidT
  , just $ mkStaticMethod "setThemeName" [objT c_QString] voidT
  , just $ mkStaticMethod "setThemeSearchPaths" [objT c_QStringList] voidT
  , just $ mkMethod "swap" [refT $ objT c_QIcon] voidT
  , just $ mkStaticMethod "themeName" np $ objT c_QString
  , just $ mkStaticMethod "themeSearchPaths" np $ objT c_QStringList
    -- TODO operator QVariant() const
  ]

e_Mode =
  makeQtEnum (ident1 "QIcon" "Mode") [includeStd "QIcon"]
  [ "Normal"
  , "Disabled"
  , "Active"
  , "Selected"
  ]

e_State =
  makeQtEnum (ident1 "QIcon" "State") [includeStd "QIcon"]
  [ "On"
  , "Off"
  ]
