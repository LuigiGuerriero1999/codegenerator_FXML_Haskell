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

-- | Top-level bindings and bindings in the @Qt::@ namespace.
module Graphics.UI.Qtah.Generator.Interface.Core.QPalette (
  aModule,
  c_QPalette,
  e_ColorRole,
  ) where

import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QColor (c_QColor)
import Graphics.UI.Qtah.Generator.Interface.Gui.QBrush (c_QBrush)
import Foreign.Hoppy.Generator.Types (int64T, constT, objT, refT, voidT, boolT, enumT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Foreign.Hoppy.Generator.Spec (
  Include,
  includeStd,
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod',
  mkMethod,
  np,
  )
import Graphics.UI.Qtah.Generator.Module (AModule(AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_GlobalColor)

{-# ANN module "HLint: ignore Use camelCase" #-}

qPaletteInclude :: [Include]
qPaletteInclude = [includeStd "Qt", includeStd "QPalette"]

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Core", "QPalette"]
  [ qtExport c_QPalette
  , qtExport e_ColorRole
  , qtExport e_ColorGroup
  ]

c_QPalette =
  addReqIncludes [includeStd "QPalette"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QPalette") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithColor" [refT $ constT $ objT c_QColor]
  , just $ mkCtor "newWithColors" [refT $ constT $ objT c_QColor, refT $ constT $ objT c_QColor]
  , just $ mkCtor "newWithGlobalColor" [enumT e_GlobalColor]
  , just $ mkCtor "newWithBrushes" [refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush,
    refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush,
    refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush]
  , just $ mkConstMethod "alternateBase" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "base" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "brightText" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod' "brush" "brushWithGroup" [enumT e_ColorGroup, enumT e_ColorRole] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod' "brush" "brush" [enumT e_ColorRole] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "button" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "buttonText" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "cacheKey" np int64T
  , just $ mkConstMethod' "color" "colorWithGroup" [enumT e_ColorGroup, enumT e_ColorRole] $ refT $ constT $ objT c_QColor
  , just $ mkConstMethod' "color" "color" [enumT e_ColorRole] $ refT $ constT $ objT c_QColor
  , just $ mkConstMethod "currentColorGroup" np $ enumT e_ColorGroup
  , just $ mkConstMethod "dark" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "highlight" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "highlightedText" np $ refT $ constT $ objT c_QBrush
  , test (qtVersion >= [4, 2]) $ mkConstMethod "isBrushSet" [enumT e_ColorGroup, enumT e_ColorRole] boolT
  , just $ mkConstMethod "isCopyOf" [refT $ constT $ objT c_QPalette] boolT
  , just $ mkConstMethod "isEqual" [enumT e_ColorGroup, enumT e_ColorGroup] boolT
  , just $ mkConstMethod "light" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "link" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "linkVisited" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "mid" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "midlight" np $ refT $ constT $ objT c_QBrush
  , test (qtVersion >= [5, 12]) $ mkConstMethod "placeholderText" np $ refT $ constT $ objT c_QBrush
  , just $ mkMethod' "setBrush" "setBrush" [enumT e_ColorRole, refT $ constT $ objT c_QBrush] voidT
  , just $ mkMethod' "setBrush" "setBrushWithGroup" [enumT e_ColorGroup, enumT e_ColorRole, refT $ constT $ objT c_QBrush] voidT
  , just $ mkMethod' "setColor" "setColor" [enumT e_ColorRole, refT $ constT $ objT c_QColor] voidT
  , just $ mkMethod' "setColor" "setColorWithGroup" [enumT e_ColorGroup, enumT e_ColorRole, refT $ constT $ objT c_QColor] voidT
  , just $ mkMethod "setColorGroup" [enumT e_ColorGroup, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush,
    refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush,
    refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush] voidT
  , just $ mkMethod "setCurrentColorGroup" [enumT e_ColorGroup] voidT
  , just $ mkConstMethod "shadow" np $ refT $ constT $ objT c_QBrush
  , test (qtVersion >= [5, 0]) $ mkMethod "swap" [refT $ objT c_QPalette] voidT
  , just $ mkConstMethod "text" np $ refT $ constT $ objT c_QBrush
  , test (qtVersion >= [4, 4]) $ mkConstMethod "toolTipBase" np $ refT $ constT $ objT c_QBrush
  , test (qtVersion >= [4, 4]) $ mkConstMethod "toolTipText" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "window" np $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "windowText" np $ refT $ constT $ objT c_QBrush
  ]

e_ColorGroup =
  makeQtEnum (ident1 "QPalette" "ColorGroup") qPaletteInclude
  [ "Disabled"
  , "Active"
  , "Inactive"
  , "Normal"
  ]

e_ColorRole =
  makeQtEnum (ident1 "QPalette" "ColorRole") qPaletteInclude
  [ "Window"
  , "WindowText"
  , "Base"
  , "AlternateBase"
  , "ToolTipBase"
  , "ToolTipText"
  , "Text"
  , "Button"
  , "ButtonText"
  , "BrightText"
  ]
