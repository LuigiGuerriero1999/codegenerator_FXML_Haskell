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

module Graphics.UI.Qtah.Generator.Interface.Gui.QPainter (
  aModule,
  c_QPainter,
  e_RenderHint,
  fl_RenderHints,
  ) where

import Foreign.Hoppy.Generator.Spec (
  (~:),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QRectF (c_QRectF)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  e_BGMode,
  e_BrushStyle,
  e_GlobalColor,
  e_PenStyle,
  fl_ImageConversionFlags,
  qreal,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QBrush (c_QBrush)
import Graphics.UI.Qtah.Generator.Interface.Gui.QColor (c_QColor)
import Graphics.UI.Qtah.Generator.Interface.Gui.QImage (c_QImage)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDevice (c_QPaintDevice)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPen (c_QPen)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPolygon (c_QPolygon)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPolygonF (c_QPolygonF)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QPainter"]
  [ qtExport c_QPainter
  , qtExport e_RenderHint
  , qtExport fl_RenderHints
  ]

c_QPainter =
  addReqIncludes [includeStd "QPainter"] $
  classSetEntityPrefix "" $
  makeClass (ident "QPainter") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithDevice" [ptrT $ objT c_QPaintDevice]
  , just $ mkProp "background" $ objT c_QBrush
  , just $ mkProp "brush" $ objT c_QBrush
  , just $ mkMethod' "drawImage" "drawImageAtRaw" [intT, intT, objT c_QImage] voidT
  , just $ mkMethod' "drawImage" "drawImageAtRawAll"
    [intT, intT, objT c_QImage, intT, intT, intT, intT, flagsT fl_ImageConversionFlags] voidT
  , just $ mkMethod' "drawConvexPolygon" "drawConvexPolygon" [objT c_QPolygon] voidT
  , just $ mkMethod' "drawConvexPolygon" "drawConvexPolygonF" [objT c_QPolygonF] voidT
  , just $ mkMethod' "drawEllipse" "drawEllipsePoint"
    ["center" ~: objT c_QPoint, "radiusX" ~: qreal, "radiusY" ~: qreal] voidT
  , just $ mkMethod' "drawEllipse" "drawEllipsePointF"
    ["center" ~: objT c_QPointF, "radiusX" ~: qreal, "radiusY" ~: qreal] voidT
  , just $ mkMethod' "drawEllipse" "drawEllipseRaw"
    ["x" ~: intT, "y" ~: intT, "width" ~: intT, "height" ~: intT] voidT
  , just $ mkMethod' "drawEllipse" "drawEllipseRect"
    ["rect" ~: objT c_QRect] voidT
  , just $ mkMethod' "drawEllipse" "drawEllipseRectF"
    ["rect" ~: objT c_QRectF] voidT
  , just $ mkMethod' "drawLine" "drawLinePoint"
    ["point1" ~: objT c_QPoint, "point2" ~: objT c_QPoint] voidT
  , just $ mkMethod' "drawLine" "drawLinePointF"
    ["point1" ~: objT c_QPointF, "point2" ~: objT c_QPointF] voidT
  , just $ mkMethod' "drawLine" "drawLineRaw"
    ["x1" ~: intT, "y1" ~: intT, "x2" ~: intT, "y2" ~: intT] voidT
  , just $ mkMethod' "drawPolygon" "drawPolygon" [objT c_QPolygon] voidT
  , just $ mkMethod' "drawPolygon" "drawPolygonF" [objT c_QPolygonF] voidT
  , just $ mkMethod' "drawPolyline" "drawPolyline" [objT c_QPolygon] voidT
  , just $ mkMethod' "drawPolyline" "drawPolylineF" [objT c_QPolygonF] voidT
  , just $ mkMethod' "drawRect" "drawRectRaw"
    ["x" ~: intT, "y" ~: intT, "width" ~: intT, "height" ~: intT] voidT
  , just $ mkMethod' "fillRect" "fillRectWithColor" [objT c_QRect, objT c_QColor] voidT
  , just $ mkMethod' "fillRect" "fillRectWithGlobalColor" [objT c_QRect, enumT e_GlobalColor] voidT
  , just $ mkProp "pen" $ objT c_QPen
  , just $ mkProp "renderHints" $ flagsT fl_RenderHints
  , just $ mkMethod "setBackgroundMode" [enumT e_BGMode] voidT
  , just $ mkMethod' "setRenderHint" "setRenderHint" [enumT e_RenderHint] voidT
  , just $ mkMethod' "setRenderHint" "setRenderHintTo" [enumT e_RenderHint, boolT] voidT
  , just $ mkMethod' "setBrush" "setBrushStyle" [enumT e_BrushStyle] voidT
  , just $ mkMethod' "setPen" "setPenColor" [objT c_QColor] voidT
  , just $ mkMethod' "setPen" "setPenStyle" [enumT e_PenStyle] voidT
  , just $ mkConstMethod "testRenderHint" [enumT e_RenderHint] voidT
  , just $ mkMethod' "translate" "translateRaw" [qreal, qreal] voidT
  , just $ mkMethod' "scale" "scaleRaw" [qreal, qreal] voidT
  -- TODO Lots of methods missing.
  ]

(e_RenderHint, fl_RenderHints) =
  makeQtEnumAndFlags (ident1 "QPainter" "RenderHint") "RenderHints" [includeStd "QPainter"]
  [ "Antialiasing"
  , "TextAntialiasing"
  , "SmoothPixmapTransform"
  , "HighQualityAntialiasing"
  , "NonCosmeticDefaultPen"
  , "Qt4CompatiblePainting"
  ]
