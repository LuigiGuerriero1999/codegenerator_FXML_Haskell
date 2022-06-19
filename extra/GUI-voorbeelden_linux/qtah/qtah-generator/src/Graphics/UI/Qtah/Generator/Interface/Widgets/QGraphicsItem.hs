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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsItem (
  aModule,
  c_QGraphicsItem,
  e_CacheMode,
  e_GraphicsItemChange,
  e_PanelModality,
  fl_GraphicsItemFlags,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkMethod,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (voidT, objT, ptrT, boolT, constT, intT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.QRectF (c_QRectF)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Gui.QCursor (c_QCursor)
-- import Graphics.UI.Qtah.Generator.Interface.Gui.QPolygonF (c_QPolygonF)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPainterPath (c_QPainterPath)
-- import Graphics.UI.Qtah.Generator.Interface.Gui.QTransform (c_QTransform)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsScene (c_QGraphicsScene)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGraphicsItem"]
  [ qtExport c_QGraphicsItem
  , qtExport e_CacheMode
  , qtExport e_GraphicsItemChange
  , qtExport e_GraphicsItemFlag
  , qtExport fl_GraphicsItemFlags
  , qtExport e_PanelModality
  ]

c_QGraphicsItem =
  addReqIncludes [includeStd "QGraphicsItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGraphicsItem") Nothing []
  [ mkConstMethod "acceptDrops" np boolT
  , mkConstMethod "acceptHoverEvents" np boolT
  , mkConstMethod "acceptTouchEvents" np boolT
  -- TODO mkConstMethod "acceptedMouseButtons" np $ objT c_Qt::MouseButtons
  , mkMethod "advance" [intT] voidT
  , mkConstMethod "boundingRect" np $ objT c_QRectF
  -- TODO mkConstMethod "boundingRegion" [objT c_QTransform] $ objT c_QRegion
  , mkConstMethod "boundingRegionGranularity" np qreal
  -- TODO mkConstMethod "cacheMode" np $ enumT e_CacheMode
  -- TODO mkConstMethod "childItems" np $ objT c_QList<QGraphicsItem $ objT c_*>
  , mkConstMethod "childrenBoundingRect" np $ objT c_QRectF
  , mkMethod "clearFocus" np voidT
  , mkConstMethod "clipPath" np $ objT c_QPainterPath
  , mkConstMethod "collidesWithItem" [ptrT $ constT $ objT c_QGraphicsItem] boolT
  -- TODO mkConstMethod' "collidesWithItem" "collidesWithItemAll"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_Qt::ItemSelectionMode] boolT
  , mkConstMethod "collidesWithPath" [objT c_QPainterPath] boolT
  -- TODO mkConstMethod' "collidesWithPath" "collidesWithPathAll"
  --   [objT c_QPainterPath, objT c_Qt::ItemSelectionMode] boolT
  -- TODO mkConstMethod "collidingItems" np $ objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "collidingItems" "collidingItemsAll"
  --   [objT c_Qt::ItemSelectionMode] $ objT c_QList<QGraphicsItem $ objT c_*>
  , mkConstMethod "commonAncestorItem" [ptrT $ constT $ objT c_QGraphicsItem] $
      ptrT $ objT c_QGraphicsItem
  , mkConstMethod "contains" [objT c_QPointF] boolT
  , mkProp "cursor" $ objT c_QCursor
  -- TODO mkConstMethod "data" [intT] $ objT c_QVariant
  -- TODO mkConstMethod "deviceTransform" [objT c_QTransform] $ objT c_QTransform
  , mkConstMethod "effectiveOpacity" np qreal
  , mkMethod "ensureVisible" np voidT
  , mkMethod' "ensureVisible" "ensureVisibleRectFAll" [objT c_QRectF, intT, intT] voidT
  , mkMethod' "ensureVisible" "ensureVisibleRaw"
      [qreal, qreal, qreal, qreal] voidT
  , mkMethod' "ensureVisible" "ensureVisibleRawAll"
      [qreal, qreal, qreal, qreal, intT, intT] voidT
  , mkConstMethod "filtersChildEvents" np boolT
  -- TODO mkConstMethod "flags" np $ objT c_GraphicsItemFlags
  , mkConstMethod "focusItem" np $ ptrT $ objT c_QGraphicsItem
  , mkConstMethod "focusProxy" np $ ptrT $ objT c_QGraphicsItem
  , mkMethod "grabKeyboard" np voidT
  , mkMethod "grabMouse" np voidT
  -- TODO mkConstMethod "graphicsEffect" np $ ptrT $ objT c_QGraphicsEffect
  -- TODO mkConstMethod "group" np $ ptrT $ objT c_QGraphicsItemGroup
  , mkConstMethod "hasCursor" np boolT
  , mkConstMethod "hasFocus" np boolT
  , mkMethod "hide" np voidT
  -- TODO mkConstMethod "inputMethodHints" np $ objT c_Qt::InputMethodHints
  , mkMethod "installSceneEventFilter" [ptrT $ objT c_QGraphicsItem] voidT
  , mkConstMethod "isActive" np boolT
  , mkConstMethod "isAncestorOf" [ptrT $ constT $ objT c_QGraphicsItem] boolT
  , mkConstMethod "isBlockedByModalPanel" np boolT
  , mkConstMethod' "isBlockedByModalPanel" "isBlockedByModalPanelAll"
      [ptrT $ ptrT $ objT c_QGraphicsItem] boolT
  , mkConstMethod "isClipped" np boolT
  , mkConstMethod "isEnabled" np boolT
  , mkConstMethod "isObscured" np boolT
  , mkConstMethod' "isObscured" "isObscuredRaw" [qreal, qreal, qreal, qreal] boolT
  , mkConstMethod' "isObscured" "isObscuredRectF" [objT c_QRectF] boolT
  , mkConstMethod "isObscuredBy" [ptrT $ constT $ objT c_QGraphicsItem] boolT
  , mkConstMethod "isPanel" np boolT
  , mkConstMethod "isSelected" np boolT
  , mkConstMethod "isUnderMouse" np boolT
  , mkConstMethod "isVisible" np boolT
  , mkConstMethod "isVisibleTo" [ptrT $ constT $ objT c_QGraphicsItem] boolT
  , mkConstMethod "isWidget" np boolT
  , mkConstMethod "isWindow" np boolT
  -- TODO mkConstMethod "itemTransform" [ptrT $ constT $ objT c_QGraphicsItem] $
  --   objT c_QTransform
  -- TODO mkConstMethod' "itemTransform" "itemTransformAll"
  --   [ptrT $ constT $ objT c_QGraphicsItem, ptrT $ boolT] $ objT c_QTransform
  -- TODO mkConstMethod' "mapFromItem" "mapFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QPointF] $ objT c_QPointF
  -- TODO mkConstMethod' "mapFromItem" "mapFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QRectF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromItem" "mapFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QPolygonF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromItem" "mapFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QPainterPath] $ objT c_QPainterPath
  -- TODO mkConstMethod' "mapFromItem" "mapFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, qreal, qreal, qreal, qreal] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromItem" "mapFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, qreal, qreal] $ objT c_QPointF
  -- TODO mkConstMethod' "mapFromParent" "mapFromParent"
  --   [objT c_QPointF] $ objT c_QPointF
  -- TODO mkConstMethod' "mapFromParent" "mapFromParent"
  --   [objT c_QRectF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromParent" "mapFromParent"
  --   [objT c_QPolygonF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromParent" "mapFromParent"
  --   [objT c_QPainterPath] $ objT c_QPainterPath
  -- TODO mkConstMethod' "mapFromParent" "mapFromParent"
  --   [qreal, qreal, qreal, qreal] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromParent" "mapFromParent"
  --   [qreal, qreal] $ objT c_QPointF
  -- TODO mkConstMethod' "mapFromScene" "mapFromScene"
  --   [objT c_QPointF] $ objT c_QPointF
  -- TODO mkConstMethod' "mapFromScene" "mapFromScene"
  --   [objT c_QRectF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromScene" "mapFromScene"
  --   [objT c_QPolygonF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromScene" "mapFromScene"
  --   [objT c_QPainterPath] $ objT c_QPainterPath
  -- TODO mkConstMethod' "mapFromScene" "mapFromScene"
  --   [qreal, qreal, qreal, qreal] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromScene" "mapFromScene"
  --   [qreal, qreal] $ objT c_QPointF
  -- TODO mkConstMethod' "mapRectFromItem" "mapRectFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QRectF] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectFromItem" "mapRectFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, qreal, qreal, qreal, qreal] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectFromParent" "mapRectFromParent"
  --   [objT c_QRectF] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectFromParent" "mapRectFromParent"
  --   [qreal, qreal, qreal, qreal] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectFromScene" "mapRectFromScene"
  --   [objT c_QRectF] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectFromScene" "mapRectFromScene"
  --   [qreal, qreal, qreal, qreal] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectToItem" "mapRectToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QRectF] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectToItem" "mapRectToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, qreal, qreal, qreal, qreal] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectToParent" "mapRectToParent"
  --   [objT c_QRectF] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectToParent" "mapRectToParent"
  --   [qreal, qreal, qreal, qreal] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectToScene" "mapRectToScene"
  --   [objT c_QRectF] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectToScene" "mapRectToScene"
  --   [qreal, qreal, qreal, qreal] $ objT c_QRectF
  -- TODO mkConstMethod' "mapToItem" "mapToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QPointF] $ objT c_QPointF
  -- TODO mkConstMethod' "mapToItem" "mapToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QRectF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToItem" "mapToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QPolygonF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToItem" "mapToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QPainterPath] $ objT c_QPainterPath
  -- TODO mkConstMethod' "mapToItem" "mapToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, qreal, qreal, qreal, qreal] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToItem" "mapToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, qreal, qreal] $ objT c_QPointF
  -- TODO mkConstMethod' "mapToParent" "mapToParent"
  --   [objT c_QPointF] $ objT c_QPointF
  -- TODO mkConstMethod' "mapToParent" "mapToParent"
  --   [objT c_QRectF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToParent" "mapToParent"
  --   [objT c_QPolygonF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToParent" "mapToParent"
  --   [objT c_QPainterPath] $ objT c_QPainterPath
  -- TODO mkConstMethod' "mapToParent" "mapToParent"
  --   [qreal, qreal, qreal, qreal] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToParent" "mapToParent"
  --   [qreal, qreal] $ objT c_QPointF
  -- TODO mkConstMethod' "mapToScene" "mapToScene"
  --   [objT c_QPointF] $ objT c_QPointF
  -- TODO mkConstMethod' "mapToScene" "mapToScene"
  --   [objT c_QRectF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToScene" "mapToScene"
  --   [objT c_QPolygonF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToScene" "mapToScene"
  --   [objT c_QPainterPath] $ objT c_QPainterPath
  -- TODO mkConstMethod' "mapToScene" "mapToScene"
  --   [qreal, qreal, qreal, qreal] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToScene" "mapToScene"
  --   [qreal, qreal] $ objT c_QPointF
  , mkMethod "moveBy" [qreal, qreal] voidT
  , mkConstMethod "opacity" np qreal
  , mkConstMethod "opaqueArea" np $ objT c_QPainterPath
  -- TODO mkMethod "paint"
  --   [ptrT $ objT c_QPainter, ptrT $ constT $ objT c_QStyleOptionGraphicsItem] voidT
  -- TODO mkMethod' "paint" "paintAll"
  --   [ ptrT $ objT c_QPainter
  --   , ptrT $ constT $ objT c_QStyleOptionGraphicsItem
  --   , ptrT $ objT c_QWidget
  --   ] voidT
  , mkConstMethod "panel" np $ ptrT $ objT c_QGraphicsItem
  -- TODO mkConstMethod "panelModality" np $ objT c_PanelModality
  , mkConstMethod "parentItem" np $ ptrT $ objT c_QGraphicsItem
  -- TODO mkConstMethod "parentObject" np $ ptrT $ objT c_QGraphicsObject
  -- TODO mkConstMethod "parentWidget" np $ ptrT $ objT c_QGraphicsWidget
  , mkConstMethod "pos" np $ objT c_QPointF
  , mkMethod "removeSceneEventFilter" [ptrT $ objT c_QGraphicsItem] voidT
  , mkMethod "resetTransform" np voidT
  , mkConstMethod "rotation" np qreal
  , mkConstMethod "scale" np qreal
  , mkConstMethod "scene" np $ ptrT $ objT c_QGraphicsScene
  , mkConstMethod "sceneBoundingRect" np $ objT c_QRectF
  , mkConstMethod "scenePos" np $ objT c_QPointF
  -- TODO mkConstMethod "sceneTransform" np $ objT c_QTransform
  , mkMethod "scroll" [qreal, qreal] voidT
  , mkMethod' "scroll" "scrollAll" [qreal, qreal, objT c_QRectF] voidT
  , mkMethod "setAcceptDrops" [boolT] voidT
  , mkMethod "setAcceptHoverEvents" [boolT] voidT
  , mkMethod "setAcceptTouchEvents" [boolT] voidT
  -- TODO mkMethod "setAcceptedMouseButtons" [objT c_Qt::MouseButtons] voidT
  , mkMethod "setActive" [boolT] voidT
  , mkMethod "setBoundingRegionGranularity" [qreal] voidT
  -- TODO mkMethod "setCacheMode" [objT c_CacheMode] voidT
  -- TODO mkMethod' "setCacheMode" "setCacheModeAll" [objT c_CacheMode, objT c_QSize] voidT
  -- TODO mkMethod "setData" [intT, objT c_QVariant] voidT
  , mkMethod "setEnabled" [boolT] voidT
  , mkMethod "setFiltersChildEvents" [boolT] voidT
  -- TODO mkMethod "setFlag" [objT c_GraphicsItemFlag] voidT
  -- TODO mkMethod' "setFlag" "setFlagAll" [objT c_GraphicsItemFlag, boolT] voidT
  -- TODO mkMethod "setFlags" [objT c_GraphicsItemFlags] voidT
  , mkMethod "setFocus" np voidT
 --  , mkMethod' "setFocus" "setFocusAll" [objT c_Qt::FocusReason] voidT
  , mkMethod "setFocusProxy" [ptrT $ objT c_QGraphicsItem] voidT
  -- TODO mkMethod "setGraphicsEffect" [ptrT $ objT c_QGraphicsEffect] voidT
  -- TODO mkMethod "setGroup" [ptrT $ objT c_QGraphicsItemGroup] voidT
  -- TODO mkMethod "setInputMethodHints" [objT c_Qt::InputMethodHints] voidT
  , mkMethod "setOpacity" [qreal] voidT
  -- TODO mkMethod "setPanelModality" [objT c_PanelModality] voidT
  , mkMethod "setParentItem" [ptrT $ objT c_QGraphicsItem] voidT
  , mkMethod' "setPos" "setPosPointF" [objT c_QPointF] voidT
  , mkMethod' "setPos" "setPosRaw" [qreal, qreal] voidT
  , mkMethod "setRotation" [qreal] voidT
  , mkMethod "setScale" [qreal] voidT
  , mkMethod "setSelected" [boolT] voidT
  , mkMethod "setToolTip" [objT c_QString] voidT
  -- TODO mkMethod "setTransform" [objT c_QTransform] voidT
  -- TODO mkMethod' "setTransform" "setTransformAll" [objT c_QTransform, boolT] voidT
  , mkMethod' "setTransformOriginPoint" "setTransformOriginPointF" [objT c_QPointF] voidT
  , mkMethod' "setTransformOriginPoint" "setTransformOriginPointRaw" [qreal, qreal] voidT
  -- TODO mkMethod "setTransformations" [objT c_QList<QGraphicsTransform] voidT
  , mkMethod "setVisible" [boolT] voidT
  , mkMethod "setX" [qreal] voidT
  , mkMethod "setY" [qreal] voidT
  , mkMethod "setZValue" [qreal] voidT
  , mkConstMethod "shape" np $ objT c_QPainterPath
  , mkMethod "show" np voidT
  , mkMethod "stackBefore" [ptrT $ constT $ objT c_QGraphicsItem] voidT
  -- TODO mkMethod' "toGraphicsObject" "toGraphicsObject" np $
  --   ptrT $ objT c_QGraphicsObject
  -- TODO mkConstMethod' "toGraphicsObject" "toGraphicsObject" np $
  --   ptrT $ constT $ objT c_QGraphicsObject
  , mkConstMethod "toolTip" np $ objT c_QString
  , mkConstMethod "topLevelItem" np $ ptrT $ objT c_QGraphicsItem
  -- TODO mkConstMethod "topLevelWidget" np $ ptrT $ objT c_QGraphicsWidget
  -- TODO mkConstMethod "transform" np $ objT c_QTransform
  , mkConstMethod "transformOriginPoint" np $ objT c_QPointF
  -- TODO mkConstMethod "transformations" np $ objT c_QList<QGraphicsTransform $ objT c_*>
  , mkConstMethod' "type" "itemType" np intT
  , mkMethod "ungrabKeyboard" np voidT
  , mkMethod "ungrabMouse" np voidT
  , mkMethod "unsetCursor" np voidT
  , mkMethod "update" np voidT
  , mkMethod' "update" "updateRectF" [objT c_QRectF] voidT
  , mkMethod' "update" "updateRaw" [qreal, qreal, qreal, qreal] voidT
  -- TODO mkConstMethod "window" np $ ptrT $ objT c_QGraphicsWidget
  , mkConstMethod "x" np qreal
  , mkConstMethod "y" np qreal
  , mkConstMethod "zValue" np qreal
  ]

e_CacheMode =
  makeQtEnum (ident1 "QGraphicsItem" "CacheMode") [includeStd "QGraphicsItem"]
  [ "NoCache"
  , "ItemCoordinateCache"
  , "DeviceCoordinateCache"
  ]

e_GraphicsItemChange =
  makeQtEnum (ident1 "QGraphicsItem" "GraphicsItemChange") [includeStd "QGraphicsItem"]
  [ "ItemEnabledChange"
  , "ItemEnabledHasChanged"
  , "ItemMatrixChange"
  , "ItemPositionChange"
  , "ItemPositionHasChanged"
  , "ItemTransformChange"
  , "ItemTransformHasChanged"
  , "ItemRotationChange"
  , "ItemRotationHasChanged"
  , "ItemScaleChange"
  , "ItemScaleHasChanged"
  , "ItemTransformOriginPointChange"
  , "ItemTransformOriginPointHasChanged"
  , "ItemSelectedChange"
  , "ItemSelectedHasChanged"
  , "ItemVisibleChange"
  , "ItemVisibleHasChanged"
  , "ItemParentChange"
  , "ItemParentHasChanged"
  , "ItemChildAddedChange"
  , "ItemChildRemovedChange"
  , "ItemSceneChange"
  , "ItemSceneHasChanged"
  , "ItemCursorChange"
  , "ItemCursorHasChanged"
  , "ItemToolTipChange"
  , "ItemToolTipHasChanged"
  , "ItemFlagsChange"
  , "ItemFlagsHaveChanged"
  , "ItemZValueChange"
  , "ItemZValueHasChanged"
  , "ItemOpacityChange"
  , "ItemOpacityHasChanged"
  , "ItemScenePositionHasChanged"
  ]

(e_GraphicsItemFlag, fl_GraphicsItemFlags) =
  makeQtEnumAndFlags (ident1 "QGraphicsItem" "GraphicsItemFlag") "GraphicsItemFlags"
    [includeStd "QGraphicsItem"] $
  collect
  [ just "ItemIsMovable"
  , just "ItemIsSelectable"
  , just "ItemIsFocusable"
  , just "ItemClipsToShape"
  , just "ItemClipsChildrenToShape"
  , just "ItemIgnoresTransformations"
  , just "ItemIgnoresParentOpacity"
  , just "ItemDoesntPropagateOpacityToChildren"
  , just "ItemStacksBehindParent"
  , just "ItemUsesExtendedStyleOption"
  , just "ItemHasNoContents"
  , just "ItemSendsGeometryChanges"
  , just "ItemAcceptsInputMethod"
  , just "ItemNegativeZStacksBehindParent"
  , just "ItemIsPanel"
  , just "ItemSendsScenePositionChanges"
  , test (qtVersion >= [5, 4]) "ItemContainsChildrenInShape"
  ]

e_PanelModality =
  makeQtEnum (ident1 "QGraphicsItem" "PanelModality") [includeStd "QGraphicsItem"]
  [ "NonModal"
  , "PanelModal"
  , "SceneModal"
  ]
