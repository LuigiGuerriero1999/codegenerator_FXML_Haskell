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

module Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDevice (
  aModule,
  c_QPaintDevice,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QPaintDevice"]
  [ qtExport c_QPaintDevice
  , qtExport e_PaintDeviceMetric
  ]

c_QPaintDevice =
  addReqIncludes [includeStd "QPaintDevice"] $
  classSetEntityPrefix "" $
  makeClass (ident "QPaintDevice") Nothing [] $
  collect
  [ just $ mkConstMethod "colorCount" np intT
  , just $ mkConstMethod "depth" np intT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "devicePixelRatio" np intT
  , test (qtVersion >= [5, 6]) $ mkConstMethod "devicePixelRatioF" np qreal
  , just $ mkConstMethod "height" np intT
  , just $ mkConstMethod "heightMM" np intT
  , just $ mkConstMethod "logicalDpiX" np intT
  , just $ mkConstMethod "logicalDpiY" np intT
    -- TODO paintEngine
  , just $ mkConstMethod "paintingActive" np boolT
  , just $ mkConstMethod "physicalDpiX" np intT
  , just $ mkConstMethod "physicalDpiY" np intT
  , just $ mkConstMethod "width" np intT
  , just $ mkConstMethod "widthMM" np intT
  ]

e_PaintDeviceMetric =
  makeQtEnum (ident1 "QPaintDevice" "PaintDeviceMetric") [includeStd "QPaintDevice"] $
  collect
  [ just "PdmWidth"
  , just "PdmHeight"
  , just "PdmWidthMM"
  , just "PdmHeightMM"
  , just "PdmNumColors"
  , just "PdmDepth"
  , just "PdmDpiX"
  , just "PdmDpiY"
  , just "PdmPhysicalDpiX"
  , just "PdmPhysicalDpiY"
  , just "PdmDevicePixelRatio"
  , test (qtVersion >= [5, 6]) "PdmDevicePixelRatioScaled"
  ]
