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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsView (
  aModule,
  c_QGraphicsView,
  e_DragMode,
  e_ViewportAnchor,
  e_ViewportUpdateMode,
  e_OptimizationFlag,
  fl_OptimizationFlags,
  e_CacheModeFlag,
  fl_CacheMode
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkMethod,
  mkMethod',
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  np,
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  constT,
  enumT,
  intT,
  objT,
  ptrT,
  voidT,
  )
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_AspectRatioMode, fl_Alignment, qreal)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QRectF (c_QRectF)
import Graphics.UI.Qtah.Generator.Interface.Gui.QBrush (c_QBrush)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPainter (c_QPainter, e_RenderHint, fl_RenderHints)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPolygon (c_QPolygon)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPolygonF (c_QPolygonF)
import Graphics.UI.Qtah.Generator.Interface.Gui.QTransform (c_QTransform)
-- import Graphics.UI.Qtah.Generator.Interface.Gui.QPainter
import Graphics.UI.Qtah.Generator.Interface.Gui.QPainterPath (c_QPainterPath)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractScrollArea (c_QAbstractScrollArea)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsItem (c_QGraphicsItem)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsScene (c_QGraphicsScene)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGraphicsView"] $
  [ qtExport c_QGraphicsView
  , qtExport e_CacheModeFlag
  , qtExport fl_CacheMode
  , qtExport e_DragMode
  , qtExport e_OptimizationFlag
  , qtExport fl_OptimizationFlags
  , qtExport e_ViewportAnchor
  , qtExport e_ViewportUpdateMode
  ]

c_QGraphicsView =
  addReqIncludes [includeStd "QGraphicsView"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGraphicsView") Nothing [c_QAbstractScrollArea]
  [ mkCtor "new" np
  , mkCtor "newWithScene" [ptrT $ objT c_QGraphicsScene]
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithSceneAndParent" [ptrT $ objT c_QGraphicsScene, ptrT $ objT c_QWidget]
  , mkConstMethod "alignment" np $ flagsT fl_Alignment
  , mkConstMethod "backgroundBrush" np $ objT c_QBrush
  , mkConstMethod "cacheMode" np $ flagsT fl_CacheMode
  , mkMethod' "centerOn" "centerOnPointF" [objT c_QPointF] voidT
  , mkMethod' "centerOn" "centerOnRaw" [qreal, qreal] voidT
  , mkMethod' "centerOn" "centerOnItem" [ptrT $ constT $ objT c_QGraphicsItem] voidT
  , mkConstMethod "dragMode" np $ enumT e_DragMode
  , mkMethod' "ensureVisible" "ensureVisibleRectF" [objT c_QRectF] voidT
  , mkMethod' "ensureVisible" "ensureVisibleRaw"
      [qreal, qreal, qreal, qreal] voidT
  , mkMethod' "ensureVisible" "ensureVisibleItem"
      [ptrT $ constT $ objT c_QGraphicsItem] voidT
  , mkMethod' "ensureVisible" "ensureVisibleRectFAll"
      [objT c_QRectF, intT, intT] voidT
  , mkMethod' "ensureVisible" "ensureVisibleRawAll"
      [qreal, qreal, qreal, qreal, intT, intT] voidT
  , mkMethod' "ensureVisible" "ensureVisibleItemAll"
      [ptrT $ constT $ objT c_QGraphicsItem, intT, intT] voidT
  , mkMethod' "fitInView" "fitInViewRectF" [objT c_QRectF] voidT
  , mkMethod' "fitInView" "fitInViewRect" [qreal, qreal, qreal, qreal] voidT
  , mkMethod' "fitInView" "fitInViewItem" [ptrT $ constT $ objT c_QGraphicsItem] voidT
  , mkMethod' "fitInView" "fitInViewRectFAll" [objT c_QRectF, enumT e_AspectRatioMode] voidT
  , mkMethod' "fitInView" "fitInViewRectAll"
      [qreal, qreal, qreal, qreal, enumT e_AspectRatioMode] voidT
  , mkMethod' "fitInView" "fitInViewItemAll"
      [ptrT $ constT $ objT c_QGraphicsItem, enumT e_AspectRatioMode] voidT
  , mkConstMethod "foregroundBrush" np $ objT c_QBrush
  , mkConstMethod "isInteractive" np boolT
  , mkConstMethod "isTransformed" np boolT
  , mkConstMethod' "itemAt" "itemAtPoint" [objT c_QPoint] $ ptrT $ objT c_QGraphicsItem
  , mkConstMethod' "itemAt" "itemAtRaw" [intT, intT] $ ptrT $ objT c_QGraphicsItem
  -- TODO mkConstMethod "items" np $ objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod "items" [objT c_QPoint] $ objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod "items" [intT, intT] $ objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod "items" [intT, intT, intT, intT, objT c_Qt::ItemSelectionMode] $
  --   objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod "items" [objT c_QRect, objT c_Qt::ItemSelectionMode] $
  --   objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod "items" [objT c_QPolygon, objT c_Qt::ItemSelectionMode] $
  --   objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod "items" [objT c_QPainterPath, objT c_Qt::ItemSelectionMode] $
  --   objT c_QList<QGraphicsItem $ objT c_*>
  , mkConstMethod' "mapFromScene" "mapFromScenePointF"
      [objT c_QPointF] $ objT c_QPoint
  , mkConstMethod' "mapFromScene" "mapFromSceneRectF"
      [objT c_QRectF] $ objT c_QPolygon
  , mkConstMethod' "mapFromScene" "mapFromScenePolygonF"
      [objT c_QPolygonF] $ objT c_QPolygon
  , mkConstMethod' "mapFromScene" "mapFromScenePainterPath"
      [objT c_QPainterPath] $ objT c_QPainterPath
  , mkConstMethod' "mapFromScene" "mapFromScenePointFRaw"
      [qreal, qreal] $ objT c_QPoint
  , mkConstMethod' "mapFromScene" "mapFromSceneRectFRaw"
      [qreal, qreal, qreal, qreal] $ objT c_QPolygon
  , mkConstMethod' "mapToScene" "mapToScenePoint"
      [objT c_QPoint] $ objT c_QPointF
  , mkConstMethod' "mapToScene" "mapToSceneRect"
      [objT c_QRect] $ objT c_QPolygonF
  , mkConstMethod' "mapToScene" "mapToScenePolygon"
      [objT c_QPolygon] $ objT c_QPolygonF
  , mkConstMethod' "mapToScene" "mapToScenePainterPath"
      [objT c_QPainterPath] $ objT c_QPainterPath
  , mkConstMethod' "mapToScene" "mapToScenePointRaw"
      [intT, intT] $ objT c_QPointF
  , mkConstMethod' "mapToScene" "mapToSceneRectRaw"
      [intT, intT, intT, intT] $ objT c_QPolygonF
  -- TODO mkConstMethod "matrix" np $ objT c_QMatrix
  , mkConstMethod "optimizationFlags" np $ flagsT fl_OptimizationFlags
  , mkMethod "render" [ptrT $ objT c_QPainter] voidT
  , mkMethod' "render" "renderAll"
      [ptrT $ objT c_QPainter, objT c_QRectF, objT c_QRect, enumT e_AspectRatioMode] voidT
  , mkConstMethod "renderHints" np $ flagsT fl_RenderHints
  , mkMethod "resetCachedContent" np voidT
  , mkMethod "resetMatrix" np voidT
  , mkMethod "resetTransform" np voidT
  , mkConstMethod "resizeAnchor" np $ enumT e_ViewportAnchor
  , mkMethod "rotate" [qreal] voidT
  -- TODO mkConstMethod "rubberBandSelectionMode" np $ objT c_Qt::ItemSelectionMode
  , mkMethod "scale" [qreal, qreal] voidT
  , mkConstMethod "scene" np $ ptrT $ objT c_QGraphicsScene
  , mkConstMethod "sceneRect" np $ objT c_QRectF
  , mkMethod "setAlignment" [flagsT fl_Alignment] voidT
  , mkMethod "setBackgroundBrush" [objT c_QBrush] voidT
  , mkMethod "setCacheMode" [flagsT fl_CacheMode] voidT
  , mkMethod "setDragMode" [enumT e_DragMode] voidT
  , mkMethod "setForegroundBrush" [objT c_QBrush] voidT
  , mkMethod "setInteractive" [boolT] voidT
  -- TODO mkMethod "setMatrix" [objT c_QMatrix] voidT
  -- TODO mkMethod' "setMatrix" "setMatrixAll" [objT c_QMatrix, boolT] voidT
  , mkMethod "setOptimizationFlag" [enumT e_OptimizationFlag] voidT
  , mkMethod' "setOptimizationFlag" "setOptimizationFlagAll" [enumT e_OptimizationFlag, boolT] voidT
  , mkMethod "setOptimizationFlags" [flagsT fl_OptimizationFlags] voidT
  , mkMethod "setRenderHint" [enumT e_RenderHint] voidT
  , mkMethod' "setRenderHint" "setRenderHintAll" [enumT e_RenderHint, boolT] voidT
  , mkMethod "setRenderHints" [flagsT fl_RenderHints] voidT
  , mkMethod "setResizeAnchor" [enumT e_ViewportAnchor] voidT
  -- TODO mkMethod "setRubberBandSelectionMode" [objT c_Qt::ItemSelectionMode] voidT
  , mkMethod "setScene" [ptrT $ objT c_QGraphicsScene] voidT
  , mkMethod' "setSceneRect" "setSceneRectF" [objT c_QRectF] voidT
  , mkMethod' "setSceneRect" "setSceneRectRaw" [qreal, qreal, qreal, qreal] voidT
  , mkMethod "setTransform" [objT c_QTransform] voidT
  , mkMethod' "setTransform" "setTransformAll" [objT c_QTransform, boolT] voidT
  , mkMethod "setTransformationAnchor" [enumT e_ViewportAnchor] voidT
  , mkMethod "setViewportUpdateMode" [enumT e_ViewportUpdateMode] voidT
  , mkMethod "shear" [qreal, qreal] voidT
  , mkConstMethod "transform" np $ objT c_QTransform
  , mkConstMethod "transformationAnchor" np $ enumT e_ViewportAnchor
  , mkMethod "translate" [qreal, qreal] voidT
  , mkConstMethod "viewportTransform" np $ objT c_QTransform
  ]

(e_CacheModeFlag, fl_CacheMode) =
  makeQtEnumAndFlags (ident1 "QGraphicsView" "CacheModeFlag") "CacheMode"
  [includeStd "QGraphicsView"]
  [ "CacheNone"
  , "CacheBackground"
  ]

e_DragMode =
  makeQtEnum (ident1 "QGraphicsView" "DragMode") [includeStd "QGraphicsView"]
  [ "NoDrag"
  , "ScrollHandDrag"
  , "RubberBandDrag"
  ]

(e_OptimizationFlag, fl_OptimizationFlags) =
  makeQtEnumAndFlags (ident1 "QGraphicsView" "OptimizationFlag") "OptimizationFlags"
  [includeStd "QGraphicsView"]
  [ "DontClipPainter"
  , "DontSavePainterState"
  , "DontAdjustForAntialiasing"
  , "IndirectPainting"
  ]

e_ViewportAnchor =
  makeQtEnum (ident1 "QGraphicsView" "ViewportAnchor")
  [includeStd "QGraphicsView"]
  [ "NoAnchor"
  , "AnchorViewCenter"
  , "AnchorUnderMouse"
  ]

e_ViewportUpdateMode =
  makeQtEnum (ident1 "QGraphicsView" "ViewportUpdateMode")
  [includeStd "QGraphicsView"]
  [ "FullViewportUpdate"
  , "MinimalViewportUpdate"
  , "SmartViewportUpdate"
  , "BoundingRectViewportUpdate"
  , "NoViewportUpdate"
  ]

-- Methods with optional arguments that weren't handled properly in the bindings above
-- (i.e. `foo` + `fooAll`).
{-
QList<QGraphicsItem *>  items
  (int x, int y, int w, int h, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape) const
QList<QGraphicsItem *>  items
  (const QRect & rect, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape) const
QList<QGraphicsItem *>  items
  (const QPolygon & polygon, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape) const
QList<QGraphicsItem *>  items
  (const QPainterPath & path, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape) const
-}
