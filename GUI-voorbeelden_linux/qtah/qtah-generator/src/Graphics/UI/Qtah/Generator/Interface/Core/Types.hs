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
module Graphics.UI.Qtah.Generator.Interface.Core.Types (
  aModule,
  qreal,
  qulonglong,
  qlonglong,
  gluint,
  qfunctionpointer,
  qtmessagehandler,
  qint8,
  qint16,
  qint32,
  qint64,
  qintptr,
  qptrdiff,
  qsizetype,
  quint8,
  quint16,
  quint32,
  quint64,
  quintptr,
  wchar_t,
  e_QtMsgType,
  e_AlignmentFlag, fl_Alignment,
  e_AnchorPoint,
  e_ApplicationState, fl_ApplicationStates,
  e_ArrowType,
  e_AspectRatioMode,
  e_Axis,
  e_BGMode,
  e_BrushStyle,
  e_CaseSensitivity,
  e_ApplicationAttribute,
  e_CheckState,
  e_ChecksumType,
  e_ClipOperation,
  e_ConnectionType,
  e_ContextMenuPolicy,
  e_CoordinateSystem,
  e_Corner,
  e_CursorMoveStyle,
  e_CursorShape,
  e_DateFormat,
  e_DayOfWeek,
  e_DockWidgetArea, fl_DockWidgetAreas,
  e_DropAction, fl_DropActions,
  e_Edge, fl_Edges,
  e_EnterKeyType,
  e_EventPriority,
  e_FillRule,
  e_FindChildOption, fl_FindChildOptions,
  e_FocusPolicy,
  e_FocusReason,
  e_GestureFlag, fl_GestureFlags,
  e_GestureState, fl_GestureStates,
  e_GestureType,
  e_HitTestAccuracy,
  e_ImageConversionFlag, fl_ImageConversionFlags,
  e_GlobalColor,
  e_InputMethodHint, fl_InputMethodHints,
  e_InputMethodQuery, fl_InputMethodQueries,
  e_ItemDataRole,
  e_ItemFlag, fl_ItemFlags,
  e_ItemSelectionMode,
  e_ItemSelectionOperation,
  e_Key,
  e_KeyboardModifier, fl_KeyboardModifiers,
  e_LayoutDirection,
  e_MaskMode,
  e_MatchFlag, fl_MatchFlags,
  e_MouseButton, fl_MouseButtons,
  e_MouseEventFlag,
  e_MouseEventFlag_minVersion, fl_MouseEventFlags,
  e_MouseEventSource,
  e_MouseEventSource_minVersion,
  e_NativeGestureType,
  e_NavigationMode,
  e_Orientation, fl_Orientations,
  e_PenCapStyle,
  e_PenJoinStyle,
  e_PenStyle,
  e_ScreenOrientation,
  e_ScreenOrientation_minVersion, fl_ScreenOrientations,
  e_ScrollBarPolicy,
  e_ScrollPhase,
  e_ScrollPhase_minVersion,
  e_ShortcutContext,
  e_SizeHint,
  e_SizeMode,
  e_SortOrder,
  e_TabFocusBehavior,
  e_TextElideMode,
  e_TextFlag,
  e_TextFormat,
  e_TextInteractionFlag, fl_TextInteractionFlags,
  e_TileRule,
  e_TimeSpec,
  e_TimerType,
  e_ToolBarArea, fl_ToolBarAreas,
  e_ToolButtonStyle,
  e_TouchPointState, fl_TouchPointStates,
  e_TransformationMode,
  e_UIEffect,
  e_WhiteSpaceMode,
  e_WidgetAttribute,
  e_WindowFrameSection,
  e_WindowModality,
  e_WindowState, fl_WindowStates,
  e_WindowType, fl_WindowFlags,
  ) where

import Data.Bits (finiteBitSize)
import Foreign.Hoppy.Generator.Spec (
  ForeignLanguage (Haskell),
  Include,
  Purity (Nonpure),
  Type,
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeFn,
  )
import System.Info (os)
import Foreign.Hoppy.Generator.Spec.Enum (CppEnum, enumAddEntryNameOverrides)
import Foreign.Hoppy.Generator.Types (doubleT, floatT, objT, int8T, int16T, int32T, int64T, ssizeT, word8T, word16T, word32T, word64T, ptrT, refT, fnT, voidT, enumT, constT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qrealFloat, qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QMessageLogContext (c_QMessageLogContext)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule = AQtModule $ makeQtModule ["Core", "Types"] exports

exports :: [QtExport]
exports =
  QtExportSpecials :
  collect
  [ test (qtVersion >= [5, 5]) $ qtExport e_QtMsgType
  , just $ qtExport e_AlignmentFlag
  , just $ qtExport fl_Alignment
  , just $ qtExport e_AnchorPoint
  , test (qtVersion >= [5, 1]) $ qtExport e_ApplicationState
  , test (qtVersion >= [5, 1]) $ qtExport fl_ApplicationStates
  , just $ qtExport e_ArrowType
  , just $ qtExport e_AspectRatioMode
  , just $ qtExport e_Axis
  , just $ qtExport e_BGMode
  , just $ qtExport e_BrushStyle
  , just $ qtExport e_CaseSensitivity
  , just $ qtExport e_CheckState
  , test (qtVersion >= [5, 9]) $ qtExport e_ChecksumType
  , just $ qtExport e_ClipOperation
  , just $ qtExport e_ConnectionType
  , just $ qtExport e_ContextMenuPolicy
  , test (qtVersion >= [4, 6]) $ qtExport e_CoordinateSystem
  , just $ qtExport e_Corner
  , just $ qtExport e_CursorMoveStyle
  , just $ qtExport e_CursorShape
  , just $ qtExport e_DateFormat
  , just $ qtExport e_DayOfWeek
  , just $ qtExport e_DockWidgetArea
  , just $ qtExport fl_DockWidgetAreas
  , just $ qtExport e_DropAction
  , just $ qtExport fl_DropActions
  , test (qtVersion >= [5, 1]) $ qtExport e_Edge
  , test (qtVersion >= [5, 1]) $ qtExport fl_Edges
  , test (qtVersion >= [5, 6]) $ qtExport e_EnterKeyType
  , just $ qtExport e_EventPriority
  , just $ qtExport e_FillRule
  , just $ qtExport e_FindChildOption
  , just $ qtExport fl_FindChildOptions
  , just $ qtExport e_FocusPolicy
  , just $ qtExport e_FocusReason
  , just $ qtExport e_GestureFlag
  , just $ qtExport fl_GestureFlags
  , test (qtVersion >= [4, 6]) $ qtExport e_GestureState
  , test (qtVersion >= [4, 6]) $ qtExport fl_GestureStates
  , test (qtVersion >= [4, 6]) $ qtExport e_GestureType
  , just $ qtExport e_HitTestAccuracy
  , just $ qtExport e_ImageConversionFlag
  , just $ qtExport fl_ImageConversionFlags
  , just $ qtExport e_GlobalColor
  , just $ qtExport e_InputMethodHint
  , just $ qtExport fl_InputMethodHints
  , just $ qtExport e_InputMethodQuery
  , just $ qtExport fl_InputMethodQueries
  , just $ qtExport e_ItemDataRole
  , just $ qtExport e_ItemFlag
  , just $ qtExport fl_ItemFlags
  , just $ qtExport e_ItemSelectionMode
  , just $ qtExport e_ItemSelectionOperation
  , just $ qtExport e_Key
  , just $ qtExport e_KeyboardModifier
  , just $ qtExport fl_KeyboardModifiers
  , just $ qtExport e_LayoutDirection
  , just $ qtExport e_MaskMode
  , just $ qtExport e_MatchFlag
  , just $ qtExport fl_MatchFlags
  , just $ qtExport e_MouseButton
  , just $ qtExport fl_MouseButtons
  , test (qtVersion >= e_MouseEventFlag_minVersion) $ qtExport e_MouseEventFlag
  , test (qtVersion >= e_MouseEventFlag_minVersion) $ qtExport fl_MouseEventFlags
  , test (qtVersion >= e_MouseEventSource_minVersion) $ qtExport e_MouseEventSource
  , test (qtVersion >= [5, 2]) $ qtExport e_NativeGestureType
  , just $ qtExport e_NavigationMode
  , just $ qtExport e_Orientation
  , just $ qtExport fl_Orientations
  , just $ qtExport e_PenCapStyle
  , just $ qtExport e_PenJoinStyle
  , just $ qtExport e_PenStyle
  , test (qtVersion >= e_ScreenOrientation_minVersion) $ qtExport e_ScreenOrientation
  , test (qtVersion >= e_ScreenOrientation_minVersion) $ qtExport fl_ScreenOrientations
  , just $ qtExport e_ScrollBarPolicy
  , test (qtVersion >= e_ScrollPhase_minVersion) $ qtExport e_ScrollPhase
  , just $ qtExport e_ShortcutContext
  , test (qtVersion >= [4, 4]) $ qtExport e_SizeHint
  , test (qtVersion >= [4, 4]) $ qtExport e_SizeMode
  , just $ qtExport e_SortOrder
  , test (qtVersion >= [5, 5]) $ qtExport e_TabFocusBehavior
  , just $ qtExport e_TextElideMode
  , just $ qtExport e_TextFlag
  , just $ qtExport e_TextFormat
  , just $ qtExport e_TextInteractionFlag
  , just $ qtExport fl_TextInteractionFlags
  , test (qtVersion >= [4, 6]) $ qtExport e_TileRule
  , just $ qtExport e_TimeSpec
  , just $ qtExport e_TimerType
  , just $ qtExport e_ToolBarArea
  , just $ qtExport fl_ToolBarAreas
  , just $ qtExport e_ToolButtonStyle
  , test (qtVersion >= [4, 6]) $ qtExport e_TouchPointState
  , test (qtVersion >= [4, 6]) $ qtExport fl_TouchPointStates
  , just $ qtExport e_TransformationMode
  , just $ qtExport e_UIEffect
  , just $ qtExport e_WhiteSpaceMode
  , just $ qtExport e_WidgetAttribute
  , test (qtVersion >= [4, 4]) $ qtExport e_WindowFrameSection
  , just $ qtExport e_WindowModality
  , just $ qtExport e_ApplicationAttribute
  , just $ qtExport e_WindowState
  , just $ qtExport fl_WindowStates
  , just $ qtExport e_WindowType
  , just $ qtExport fl_WindowFlags
  , test (qtVersion < [5, 0]) $ qtExport f_escape
  ]

qtInclude :: [Include]
qtInclude = [includeStd "Qt", includeStd "QtGlobal"]

qreal :: Type
qreal = if qrealFloat then floatT else doubleT

qlonglong :: Type
qlonglong = qint64

qulonglong :: Type
qulonglong = quint64

qfunctionpointer :: Type
qfunctionpointer = ptrT $ fnT [] voidT

qtmessagehandler :: Type
qtmessagehandler = ptrT $ fnT [enumT e_QtMsgType, refT $ constT $ objT c_QMessageLogContext, refT $ constT $ objT c_QString] voidT

qint8 :: Type
qint8 = int8T

qint16 :: Type
qint16 = int16T

qint32 :: Type
qint32 = int32T

qint64 :: Type
qint64 = int64T

qintptr :: Type
qintptr = if finiteBitSize (undefined :: Int) == 64 then qint64 else qint32

qptrdiff :: Type
qptrdiff = if finiteBitSize (undefined :: Int) == 64 then qint64 else qint32

qsizetype :: Type
qsizetype = ssizeT

quint8 :: Type
quint8 = word8T

quint16 :: Type
quint16 = word16T

quint32 :: Type
quint32 = word32T

quint64 :: Type
quint64 = word64T

quintptr :: Type
quintptr = if finiteBitSize (undefined :: Int) == 64 then quint64 else quint32

gluint :: Type
gluint = word32T

wchar_t :: Type
wchar_t = if os == "mingw32" then word16T else word32T

e_QtMsgType :: CppEnum
e_QtMsgType =
  makeQtEnum (ident "QtMsgType") qtInclude
  [ "QtDebugMsg"
  , "QtInfoMsg"
  , "QtWarningMsg"
  , "QtCriticalMsg"
  , "QtFatalMsg"
  , "QtSystemMsg"
  ]

(e_AlignmentFlag, fl_Alignment) =
  makeQtEnumAndFlags (ident1 "Qt" "AlignmentFlag") "Alignment" qtInclude
  [ -- Horizontal flags.
    "AlignLeft"
  , "AlignRight"
  , "AlignHCenter"
  , "AlignJustify"
    -- Vertical flags.
  , "AlignTop"
  , "AlignBottom"
  , "AlignVCenter"
    -- Useful in right-to-left mode.
  , "AlignAbsolute"
  ]

e_AnchorPoint =
  makeQtEnum (ident1 "Qt" "AnchorPoint") qtInclude
  [ "AnchorLeft"
  , "AnchorHorizontalCenter"
  , "AnchorRight"
  , "AnchorTop"
  , "AnchorVerticalCenter"
  , "AnchorBottom"
  ]

(e_ApplicationState, fl_ApplicationStates) =
  makeQtEnumAndFlags (ident1 "Qt" "ApplicationState") "ApplicationStates" qtInclude
  [ "ApplicationSuspended"
  , "ApplicationHidden"
  , "ApplicationInactive"
  , "ApplicationActive"
  ]

e_ApplicationAttribute =
  makeQtEnum (ident1 "Qt" "ApplicationAttribute") qtInclude $
  collect
  [ just "AA_DontShowIconsInMenus"
  , test (qtVersion >= [5, 10]) "AA_DontShowShortcutsInContextMenus"
  , just "AA_NativeWindows"
  , just "AA_DontCreateNativeWidgetSiblings"
  , test (qtVersion >= [5, 7]) "AA_PluginApplication"
  , just "AA_DontUseNativeMenuBar"
  , just "AA_MacDontSwapCtrlAndMeta"
  , just "AA_Use96Dpi"
  , just "AA_SynthesizeTouchForUnhandledMouseEvents"
  , just "AA_SynthesizeMouseForUnhandledTouchEvents"
  , just "AA_UseHighDpiPixmaps"
  , just "AA_ForceRasterWidgets"
  , test (qtVersion >= [5, 3]) "AA_UseDesktopOpenGL"
  , test (qtVersion >= [5, 3]) "AA_UseOpenGLES"
  , test (qtVersion >= [5, 4]) "AA_UseSoftwareOpenGL"
  , test (qtVersion >= [5, 4]) "AA_ShareOpenGLContexts"
  , test (qtVersion >= [5, 5]) "AA_SetPalette"
  , test (qtVersion >= [5, 6]) "AA_EnableHighDpiScaling"
  , test (qtVersion >= [5, 6]) "AA_DisableHighDpiScaling"
  , test (qtVersion >= [5, 7]) "AA_UseStyleSheetPropagationInWidgetStyles"
  , test (qtVersion >= [5, 7]) "AA_DontUseNativeDialogs"
  , test (qtVersion >= [5, 7]) "AA_SynthesizeMouseForUnhandledTabletEvents"
  , test (qtVersion >= [5, 7]) "AA_CompressHighFrequencyEvents"
  , test (qtVersion >= [5, 10]) "AA_CompressTabletEvents"
  , test (qtVersion >= [5, 8]) "AA_DontCheckOpenGLContextThreadAffinity"
  , test (qtVersion >= [5, 10]) "AA_DisableShaderDiskCache"
  , test (qtVersion >= [5, 10]) "AA_DisableWindowContextHelpButton"
  ]

e_ArrowType =
  makeQtEnum (ident1 "Qt" "ArrowType") qtInclude
  [ "NoArrow"
  , "UpArrow"
  , "DownArrow"
  , "LeftArrow"
  , "RightArrow"
  ]

e_AspectRatioMode =
  makeQtEnum (ident1 "Qt" "AspectRatioMode") qtInclude
  [ "IgnoreAspectRatio"
  , "KeepAspectRatio"
  , "KeepAspectRatioByExpanding"
  ]

e_Axis =
  makeQtEnum (ident1 "Qt" "Axis") qtInclude
  [ "XAxis"
  , "YAxis"
  , "ZAxis"
  ]

e_BGMode =
  makeQtEnum (ident1 "Qt" "BGMode") qtInclude
  [ "TransparentMode"
  , "OpaqueMode"
  ]

e_BrushStyle =
  makeQtEnum (ident1 "Qt" "BrushStyle") qtInclude
  [ "NoBrush"
  , "SolidPattern"
  , "Dense1Pattern"
  , "Dense2Pattern"
  , "Dense3Pattern"
  , "Dense4Pattern"
  , "Dense5Pattern"
  , "Dense6Pattern"
  , "Dense7Pattern"
  , "HorPattern"
  , "VerPattern"
  , "CrossPattern"
  , "BDiagPattern"
  , "FDiagPattern"
  , "DiagCrossPattern"
  , "LinearGradientPattern"
  , "RadialGradientPattern"
  , "ConicalGradientPattern"
  , "TexturePattern"
  ]

e_CaseSensitivity =
  makeQtEnum (ident1 "Qt" "CaseSensitivity") qtInclude
  [ "CaseInsensitive"
  , "CaseSensitive"
  ]

e_CheckState =
  makeQtEnum (ident1 "Qt" "CheckState") qtInclude
  [ "Unchecked"
  , "PartiallyChecked"
  , "Checked"
  ]

e_ChecksumType =
  makeQtEnum (ident1 "Qt" "ChecksumType") qtInclude
  [ "ChecksumIso3309"
  , "ChecksumItuV41"
  ]

e_ClipOperation =
  makeQtEnum (ident1 "Qt" "ClipOperation") qtInclude
  [ "NoClip"
  , "ReplaceClip"
  , "IntersectClip"
  ]

e_ConnectionType =
  makeQtEnum (ident1 "Qt" "ConnectionType") qtInclude $
  collect
  [ just "AutoConnection"
  , just "DirectConnection"
  , just "QueuedConnection"
  , just "BlockingQueuedConnection"
  , test (qtVersion >= [4, 6]) "UniqueConnection"
  ]

e_ContextMenuPolicy =
  makeQtEnum (ident1 "Qt" "ContextMenuPolicy") qtInclude
  [ "NoContextMenu"
  , "PreventContextMenu"
  , "DefaultContextMenu"
  , "ActionsContextMenu"
  , "CustomContextMenu"
  ]

e_CoordinateSystem =
  makeQtEnum (ident1 "Qt" "CoordinateSystem") qtInclude
  [ "DeviceCoordinates"
  , "LogicalCoordinates"
  ]

e_Corner =
  makeQtEnum (ident1 "Qt" "Corner") qtInclude
  [ "TopLeftCorner"
  , "TopRightCorner"
  , "BottomLeftCorner"
  , "BottomRightCorner"
  ]

e_CursorMoveStyle =
  makeQtEnum (ident1 "Qt" "CursorMoveStyle") qtInclude
  [ "LogicalMoveStyle"
  , "VisualMoveStyle"
  ]

e_CursorShape =
  makeQtEnum (ident1 "Qt" "CursorShape") qtInclude
  [ "ArrowCursor"
  , "UpArrowCursor"
  , "CrossCursor"
  , "WaitCursor"
  , "IBeamCursor"
  , "SizeVerCursor"
  , "SizeHorCursor"
  , "SizeBDiagCursor"
  , "SizeFDiagCursor"
  , "SizeAllCursor"
  , "BlankCursor"
  , "SplitVCursor"
  , "SplitHCursor"
  , "PointingHandCursor"
  , "ForbiddenCursor"
  , "WhatsThisCursor"
  , "BusyCursor"
  , "OpenHandCursor"
  , "ClosedHandCursor"
  , "DragCopyCursor"
  , "DragMoveCursor"
  , "DragLinkCursor"
  , "BitmapCursor"
  ]

e_DateFormat =
  makeQtEnum (ident1 "Qt" "DateFormat") qtInclude $
  collect
  [ just "TextDate"
  , just "ISODate"
  , test (qtVersion >= [5, 8]) "ISODateWithMs"
  , just "SystemLocaleShortDate"
  , just "SystemLocaleLongDate"
  , just "DefaultLocaleShortDate"
  , just "DefaultLocaleLongDate"
  , just "SystemLocaleDate"
  , just "LocaleDate"
  , just "RFC2822Date"
  ]

e_DayOfWeek =
  makeQtEnum (ident1 "Qt" "DayOfWeek") qtInclude
  [ "Monday"
  , "Tuesday"
  , "Wednesday"
  , "Thursday"
  , "Friday"
  , "Saturday"
  , "Sunday"
  ]

(e_DockWidgetArea, fl_DockWidgetAreas) =
  makeQtEnumAndFlags (ident1 "Qt" "DockWidgetArea") "DockWidgetAreas" qtInclude
  [ "NoDockWidgetArea"
  , "LeftDockWidgetArea"
  , "RightDockWidgetArea"
  , "TopDockWidgetArea"
  , "BottomDockWidgetArea"
  , "AllDockWidgetAreas"
  ]

(e_DropAction, fl_DropActions) =
  makeQtEnumAndFlags (ident1 "Qt" "DropAction") "DropActions" qtInclude
  [ "IgnoreAction"
  , "CopyAction"
  , "MoveAction"
  , "LinkAction"
  , "ActionMask"
  , "TargetMoveAction"
  ]

(e_Edge, fl_Edges) =
  makeQtEnumAndFlags (ident1 "Qt" "Edge") "Edges" qtInclude
  [ "TopEdge"
  , "LeftEdge"
  , "RightEdge"
  , "BottomEdge"
  ]

e_EnterKeyType =
  makeQtEnum (ident1 "Qt" "EnterKeyType") qtInclude
  [ "EnterKeyDefault"
  , "EnterKeyReturn"
  , "EnterKeyDone"
  , "EnterKeyGo"
  , "EnterKeySend"
  , "EnterKeySearch"
  , "EnterKeyNext"
  , "EnterKeyPrevious"
  ]

e_EventPriority =
  makeQtEnum (ident1 "Qt" "EventPriority") qtInclude
  [ "HighEventPriority"
  , "NormalEventPriority"
  , "LowEventPriority"
  ]

e_FillRule =
  makeQtEnum (ident1 "Qt" "FillRule") qtInclude
  [ "OddEvenFill"
  , "WindingFill"
  ]

(e_FindChildOption, fl_FindChildOptions) =
  makeQtEnumAndFlags (ident1 "Qt" "FindChildOption") "FindChildOptions" qtInclude
  [ "FindDirectChildrenOnly"
  , "FindChildrenRecursively"
  ]

e_FocusPolicy =
  makeQtEnum (ident1 "Qt" "FocusPolicy") qtInclude
  [ "TabFocus"
  , "ClickFocus"
  , "StrongFocus"
  , "WheelFocus"
  , "NoFocus"
  ]

e_FocusReason =
  makeQtEnum (ident1 "Qt" "FocusReason") qtInclude
  [ "MouseFocusReason"
  , "TabFocusReason"
  , "BacktabFocusReason"
  , "ActiveWindowFocusReason"
  , "PopupFocusReason"
  , "ShortcutFocusReason"
  , "MenuBarFocusReason"
  , "OtherFocusReason"
  ]

(e_GestureFlag, fl_GestureFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "GestureFlag") "GestureFlags" qtInclude $
  collect
  [ just "DontStartGestureOnChildren"
  , just "ReceivePartialGestures"
  , test (qtVersion >= [4,7]) "IgnoredGesturesPropagateToParent"
  ]

(e_GestureState, fl_GestureStates) =
  makeQtEnumAndFlags (ident1 "Qt" "GestureState") "GestureStates" qtInclude
  [ "NoGesture"
  , "GestureStarted"
  , "GestureUpdated"
  , "GestureFinished"
  , "GestureCanceled"
  ]

e_GestureType =
  makeQtEnum (ident1 "Qt" "GestureType") qtInclude
  [ "TapGesture"
  , "TapAndHoldGesture"
  , "PanGesture"
  , "PinchGesture"
  , "SwipeGesture"
  , "CustomGesture"
  ]

e_HitTestAccuracy =
  makeQtEnum (ident1 "Qt" "HitTestAccuracy") qtInclude
  [ "ExactHit"
  , "FuzzyHit"
  ]

e_GlobalColor =
  makeQtEnum (ident1 "Qt" "GlobalColor") qtInclude
  [ "white"
  , "black"
  , "red"
  , "darkRed"
  , "green"
  , "darkGreen"
  , "blue"
  , "darkBlue"
  , "cyan"
  , "darkCyan"
  , "magenta"
  , "darkMagenta"
  , "yellow"
  , "darkYellow"
  , "gray"
  , "darkGray"
  , "lightGray"
  , "transparent"
  , "color0"
  , "color1"
  ]

(e_ImageConversionFlag, fl_ImageConversionFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "ImageConversionFlag") "ImageConversionFlags" qtInclude
  [ -- Color/mono preference:
    "AutoColor"
  , "ColorOnly"
  , "MonoOnly"
    -- Dithering mode preference for RGB channels:
  , "DiffuseDither"
  , "OrderedDither"
  , "ThresholdDither"
    -- Dithering mode preference for alpha channel:
  , "ThresholdAlphaDither"
  , "OrderedAlphaDither"
  , "DiffuseAlphaDither"
    -- Color matching versus dithering preference:
  , "PreferDither"
  , "AvoidDither"
  , "NoOpaqueDetection"
  , "NoFormatConversion"
  ]

(e_InputMethodHint, fl_InputMethodHints) =
  makeQtEnumAndFlags (ident1 "Qt" "InputMethodHint") "InputMethodHints" qtInclude
  [ "ImhNone"
  , "ImhHiddenText"
  , "ImhSensitiveData"
  , "ImhNoAutoUppercase"
  , "ImhPreferNumbers"
  , "ImhPreferUppercase"
  , "ImhPreferLowercase"
  , "ImhNoPredictiveText"
  , "ImhDate"
  , "ImhTime"
  , "ImhPreferLatin"
  , "ImhMultiLine"
  , "ImhDigitsOnly"
  , "ImhFormattedNumbersOnly"
  , "ImhUppercaseOnly"
  , "ImhLowercaseOnly"
  , "ImhDialableCharactersOnly"
  , "ImhEmailCharactersOnly"
  , "ImhUrlCharactersOnly"
  , "ImhLatinOnly"
  , "ImhExclusiveInputMask"
  ]

(e_InputMethodQuery, fl_InputMethodQueries) =
  makeQtEnumAndFlags (ident1 "Qt" "InputMethodQuery") "InputMethodQueries" qtInclude $
  collect
  [ just "ImEnabled"
  , just "ImMicroFocus"
  , just "ImCursorRectangle"
  , just "ImFont"
  , just "ImCursorPosition"
  , just "ImSurroundingText"
  , just "ImCurrentSelection"
  , just "ImMaximumTextLength"
  , just "ImAnchorPosition"
  , just "ImHints"
  , just "ImPreferredLanguage"
  , just "ImPlatformData"
  , just "ImAbsolutePosition"
  , just "ImTextBeforeCursor"
  , just "ImTextAfterCursor"
  , test (qtVersion >= [5, 6]) "ImEnterKeyType"
  , test (qtVersion >= [5, 7]) "ImAnchorRectangle"
  , test (qtVersion >= [5, 7]) "ImInputItemClipRectangle"
  ]

-- TODO Support for custom ItemDataRole values.
e_ItemDataRole =
  makeQtEnum (ident1 "Qt" "ItemDataRole") qtInclude $
  collect
  [ -- General-purpose roles:
    just "DisplayRole"
  , just "DecorationRole"
  , just "EditRole"
  , just "ToolTipRole"
  , just "StatusTipRole"
  , just "WhatsThisRole"
  , just "SizeHintRole"

    -- Roles describing appearance and metadata:
  , just "FontRole"
  , just "TextAlignmentRole"
  , just "BackgroundRole"
  , just "ForegroundRole"
  , just "CheckStateRole"
  , test (qtVersion >= [4, 8]) "InitialSortOrderRole"

    -- Accessibility roles:
  , just "AccessibleTextRole"
  , just "AccessibleDescriptionRole"

    -- User roles:
  , just "UserRole"
  ]

(e_ItemFlag, fl_ItemFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "ItemFlag") "ItemFlags" qtInclude $
  collect
  [ just "NoItemFlags"
  , just "ItemIsSelectable"
  , just "ItemIsEditable"
  , just "ItemIsDragEnabled"
  , just "ItemIsDropEnabled"
  , just "ItemIsUserCheckable"
  , just "ItemIsEnabled"
  , test (qtVersion >= [5, 6]) "ItemIsAutoTristate"
  , just "ItemNeverHasChildren"
  , test (qtVersion >= [5, 5]) "ItemIsUserTristate"
  ]

e_ItemSelectionMode =
  makeQtEnum (ident1 "Qt" "ItemSelectionMode") qtInclude
  [ "ContainsItemShape"
  , "IntersectsItemShape"
  , "ContainsItemBoundingRect"
  , "IntersectsItemBoundingRect"
  ]

e_ItemSelectionOperation =
  makeQtEnum (ident1 "Qt" "ItemSelectionOperation") qtInclude
  [ "ReplaceSelection"
  , "AddToSelection"
  ]

e_Key =
  enumAddEntryNameOverrides Haskell
    [ ("Key_Dead_a", "KeyDeadALower")
    , ("Key_Dead_A", "KeyDeadAUpper")
    , ("Key_Dead_e", "KeyDeadELower")
    , ("Key_Dead_E", "KeyDeadEUpper")
    , ("Key_Dead_i", "KeyDeadILower")
    , ("Key_Dead_I", "KeyDeadIUpper")
    , ("Key_Dead_o", "KeyDeadOLower")
    , ("Key_Dead_O", "KeyDeadOUpper")
    , ("Key_Dead_u", "KeyDeadULower")
    , ("Key_Dead_U", "KeyDeadUUpper")
    ] $
  makeQtEnum (ident1 "Qt" "Key") qtInclude $
  collect
  [ just "Key_Escape"
  , just "Key_Tab"
  , just "Key_Backtab"
  , just "Key_Backspace"
  , just "Key_Return"
  , just "Key_Enter"
  , just "Key_Insert"
  , just "Key_Delete"
  , just "Key_Pause"
  , just "Key_Print"
  , just "Key_SysReq"
  , just "Key_Clear"
  , just "Key_Home"
  , just "Key_End"
  , just "Key_Left"
  , just "Key_Up"
  , just "Key_Right"
  , just "Key_Down"
  , just "Key_PageUp"
  , just "Key_PageDown"
  , just "Key_Shift"
  , just "Key_Control"
  , just "Key_Meta"
  , just "Key_Alt"
  , just "Key_AltGr"
  , just "Key_CapsLock"
  , just "Key_NumLock"
  , just "Key_ScrollLock"
  , just "Key_F1"
  , just "Key_F2"
  , just "Key_F3"
  , just "Key_F4"
  , just "Key_F5"
  , just "Key_F6"
  , just "Key_F7"
  , just "Key_F8"
  , just "Key_F9"
  , just "Key_F10"
  , just "Key_F11"
  , just "Key_F12"
  , just "Key_F13"
  , just "Key_F14"
  , just "Key_F15"
  , just "Key_F16"
  , just "Key_F17"
  , just "Key_F18"
  , just "Key_F19"
  , just "Key_F20"
  , just "Key_F21"
  , just "Key_F22"
  , just "Key_F23"
  , just "Key_F24"
  , just "Key_F25"
  , just "Key_F26"
  , just "Key_F27"
  , just "Key_F28"
  , just "Key_F29"
  , just "Key_F30"
  , just "Key_F31"
  , just "Key_F32"
  , just "Key_F33"
  , just "Key_F34"
  , just "Key_F35"
  , just "Key_Super_L"
  , just "Key_Super_R"
  , just "Key_Menu"
  , just "Key_Hyper_L"
  , just "Key_Hyper_R"
  , just "Key_Help"
  , just "Key_Direction_L"
  , just "Key_Direction_R"
  , just "Key_Space"
  , just "Key_Any"  -- Alias for Key_Space.
  , just "Key_Exclam"
  , just "Key_QuoteDbl"
  , just "Key_NumberSign"
  , just "Key_Dollar"
  , just "Key_Percent"
  , just "Key_Ampersand"
  , just "Key_Apostrophe"
  , just "Key_ParenLeft"
  , just "Key_ParenRight"
  , just "Key_Asterisk"
  , just "Key_Plus"
  , just "Key_Comma"
  , just "Key_Minus"
  , just "Key_Period"
  , just "Key_Slash"
  , just "Key_0"
  , just "Key_1"
  , just "Key_2"
  , just "Key_3"
  , just "Key_4"
  , just "Key_5"
  , just "Key_6"
  , just "Key_7"
  , just "Key_8"
  , just "Key_9"
  , just "Key_Colon"
  , just "Key_Semicolon"
  , just "Key_Less"
  , just "Key_Equal"
  , just "Key_Greater"
  , just "Key_Question"
  , just "Key_At"
  , just "Key_A"
  , just "Key_B"
  , just "Key_C"
  , just "Key_D"
  , just "Key_E"
  , just "Key_F"
  , just "Key_G"
  , just "Key_H"
  , just "Key_I"
  , just "Key_J"
  , just "Key_K"
  , just "Key_L"
  , just "Key_M"
  , just "Key_N"
  , just "Key_O"
  , just "Key_P"
  , just "Key_Q"
  , just "Key_R"
  , just "Key_S"
  , just "Key_T"
  , just "Key_U"
  , just "Key_V"
  , just "Key_W"
  , just "Key_X"
  , just "Key_Y"
  , just "Key_Z"
  , just "Key_BracketLeft"
  , just "Key_Backslash"
  , just "Key_BracketRight"
  , just "Key_AsciiCircum"
  , just "Key_Underscore"
  , just "Key_QuoteLeft"
  , just "Key_BraceLeft"
  , just "Key_Bar"
  , just "Key_BraceRight"
  , just "Key_AsciiTilde"
  , just "Key_nobreakspace"
  , just "Key_exclamdown"
  , just "Key_cent"
  , just "Key_sterling"
  , just "Key_currency"
  , just "Key_yen"
  , just "Key_brokenbar"
  , just "Key_section"
  , just "Key_diaeresis"
  , just "Key_copyright"
  , just "Key_ordfeminine"
  , just "Key_guillemotleft"
  , just "Key_notsign"
  , just "Key_hyphen"
  , just "Key_registered"
  , just "Key_macron"
  , just "Key_degree"
  , just "Key_plusminus"
  , just "Key_twosuperior"
  , just "Key_threesuperior"
  , just "Key_acute"
  , just "Key_mu"
  , just "Key_paragraph"
  , just "Key_periodcentered"
  , just "Key_cedilla"
  , just "Key_onesuperior"
  , just "Key_masculine"
  , just "Key_guillemotright"
  , just "Key_onequarter"
  , just "Key_onehalf"
  , just "Key_threequarters"
  , just "Key_questiondown"
  , just "Key_Agrave"
  , just "Key_Aacute"
  , just "Key_Acircumflex"
  , just "Key_Atilde"
  , just "Key_Adiaeresis"
  , just "Key_Aring"
  , just "Key_AE"
  , just "Key_Ccedilla"
  , just "Key_Egrave"
  , just "Key_Eacute"
  , just "Key_Ecircumflex"
  , just "Key_Ediaeresis"
  , just "Key_Igrave"
  , just "Key_Iacute"
  , just "Key_Icircumflex"
  , just "Key_Idiaeresis"
  , just "Key_ETH"
  , just "Key_Ntilde"
  , just "Key_Ograve"
  , just "Key_Oacute"
  , just "Key_Ocircumflex"
  , just "Key_Otilde"
  , just "Key_Odiaeresis"
  , just "Key_multiply"
  , just "Key_Ooblique"
  , just "Key_Ugrave"
  , just "Key_Uacute"
  , just "Key_Ucircumflex"
  , just "Key_Udiaeresis"
  , just "Key_Yacute"
  , just "Key_THORN"
  , just "Key_ssharp"
  , just "Key_division"
  , just "Key_ydiaeresis"
  , just "Key_Multi_key"
  , just "Key_Codeinput"
  , just "Key_SingleCandidate"
  , just "Key_MultipleCandidate"
  , just "Key_PreviousCandidate"
  , just "Key_Mode_switch"
  , just "Key_Kanji"
  , just "Key_Muhenkan"
  , just "Key_Henkan"
  , just "Key_Romaji"
  , just "Key_Hiragana"
  , just "Key_Katakana"
  , just "Key_Hiragana_Katakana"
  , just "Key_Zenkaku"
  , just "Key_Hankaku"
  , just "Key_Zenkaku_Hankaku"
  , just "Key_Touroku"
  , just "Key_Massyo"
  , just "Key_Kana_Lock"
  , just "Key_Kana_Shift"
  , just "Key_Eisu_Shift"
  , just "Key_Eisu_toggle"
  , just "Key_Hangul"
  , just "Key_Hangul_Start"
  , just "Key_Hangul_End"
  , just "Key_Hangul_Hanja"
  , just "Key_Hangul_Jamo"
  , just "Key_Hangul_Romaja"
  , just "Key_Hangul_Jeonja"
  , just "Key_Hangul_Banja"
  , just "Key_Hangul_PreHanja"
  , just "Key_Hangul_PostHanja"
  , just "Key_Hangul_Special"
  , just "Key_Dead_Grave"
  , just "Key_Dead_Acute"
  , just "Key_Dead_Circumflex"
  , just "Key_Dead_Tilde"
  , just "Key_Dead_Macron"
  , just "Key_Dead_Breve"
  , just "Key_Dead_Abovedot"
  , just "Key_Dead_Diaeresis"
  , just "Key_Dead_Abovering"
  , just "Key_Dead_Doubleacute"
  , just "Key_Dead_Caron"
  , just "Key_Dead_Cedilla"
  , just "Key_Dead_Ogonek"
  , just "Key_Dead_Iota"
  , just "Key_Dead_Voiced_Sound"
  , just "Key_Dead_Semivoiced_Sound"
  , just "Key_Dead_Belowdot"
  , just "Key_Dead_Hook"
  , just "Key_Dead_Horn"
  , test (qtVersion >= [5, 11]) "Key_Dead_Stroke"
  , test (qtVersion >= [5, 11]) "Key_Dead_Abovecomma"
  , test (qtVersion >= [5, 11]) "Key_Dead_Abovereversedcomma"
  , test (qtVersion >= [5, 11]) "Key_Dead_Doublegrave"
  , test (qtVersion >= [5, 11]) "Key_Dead_Belowring"
  , test (qtVersion >= [5, 11]) "Key_Dead_Belowmacron"
  , test (qtVersion >= [5, 11]) "Key_Dead_Belowtilde"
  , test (qtVersion >= [5, 11]) "Key_Dead_Belowbreve"
  , test (qtVersion >= [5, 11]) "Key_Dead_Belowdiaeresis"
  , test (qtVersion >= [5, 11]) "Key_Dead_Invertedbreve"
  , test (qtVersion >= [5, 11]) "Key_Dead_Belowcomma"
  , test (qtVersion >= [5, 11]) "Key_Dead_Currency"
  , test (qtVersion >= [5, 11]) "Key_Dead_a"
  , test (qtVersion >= [5, 11]) "Key_Dead_A"
  , test (qtVersion >= [5, 11]) "Key_Dead_e"
  , test (qtVersion >= [5, 11]) "Key_Dead_E"
  , test (qtVersion >= [5, 11]) "Key_Dead_i"
  , test (qtVersion >= [5, 11]) "Key_Dead_I"
  , test (qtVersion >= [5, 11]) "Key_Dead_o"
  , test (qtVersion >= [5, 11]) "Key_Dead_O"
  , test (qtVersion >= [5, 11]) "Key_Dead_u"
  , test (qtVersion >= [5, 11]) "Key_Dead_U"
  , test (qtVersion >= [5, 11]) "Key_Dead_Small_Schwa"
  , test (qtVersion >= [5, 11]) "Key_Dead_Capital_Schwa"
  , test (qtVersion >= [5, 11]) "Key_Dead_Greek"
  , test (qtVersion >= [5, 11]) "Key_Dead_Lowline"
  , test (qtVersion >= [5, 11]) "Key_Dead_Aboveverticalline"
  , test (qtVersion >= [5, 11]) "Key_Dead_Belowverticalline"
  , test (qtVersion >= [5, 11]) "Key_Dead_Longsolidusoverlay"
  , just "Key_Back"
  , just "Key_Forward"
  , just "Key_Stop"
  , just "Key_Refresh"
  , just "Key_VolumeDown"
  , just "Key_VolumeMute"
  , just "Key_VolumeUp"
  , just "Key_BassBoost"
  , just "Key_BassUp"
  , just "Key_BassDown"
  , just "Key_TrebleUp"
  , just "Key_TrebleDown"
  , just "Key_MediaPlay"
  , just "Key_MediaStop"
  , just "Key_MediaPrevious"
  , just "Key_MediaNext"
  , just "Key_MediaRecord"
  , just "Key_MediaPause"
  , just "Key_MediaTogglePlayPause"
  , just "Key_HomePage"
  , just "Key_Favorites"
  , just "Key_Search"
  , just "Key_Standby"
  , just "Key_OpenUrl"
  , just "Key_LaunchMail"
  , just "Key_LaunchMedia"
  , just "Key_Launch0"
  , just "Key_Launch1"
  , just "Key_Launch2"
  , just "Key_Launch3"
  , just "Key_Launch4"
  , just "Key_Launch5"
  , just "Key_Launch6"
  , just "Key_Launch7"
  , just "Key_Launch8"
  , just "Key_Launch9"
  , just "Key_LaunchA"
  , just "Key_LaunchB"
  , just "Key_LaunchC"
  , just "Key_LaunchD"
  , just "Key_LaunchE"
  , just "Key_LaunchF"
  , just "Key_LaunchG"
  , just "Key_LaunchH"
    -- TODO Additional Qt::Key_* constants.
  ]

(e_KeyboardModifier, fl_KeyboardModifiers) =
  makeQtEnumAndFlags (ident1 "Qt" "KeyboardModifier") "KeyboardModifiers" qtInclude
  [ "NoModifier"
  , "ShiftModifier"
  , "ControlModifier"
  , "AltModifier"
  , "MetaModifier"
  , "KeypadModifier"
  , "GroupSwitchModifier"
  ]

e_LayoutDirection =
  makeQtEnum (ident1 "Qt" "LayoutDirection") qtInclude
  [ "LeftToRight"
  , "RightToLeft"
  , "LayoutDirectionAuto"
  ]

e_MaskMode =
  makeQtEnum (ident1 "Qt" "MaskMode") qtInclude
  [ "MaskInColor"
  , "MaskOutColor"
  ]

(e_MatchFlag, fl_MatchFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "MatchFlag") "MatchFlags" qtInclude
  [ "MatchExactly"
  , "MatchFixedString"
  , "MatchContains"
  , "MatchStartsWith"
  , "MatchEndsWith"
  , "MatchCaseSensitive"
  , "MatchRegExp"
  , "MatchWildcard"
  , "MatchWrap"
  , "MatchRecursive"
  ]

(e_MouseButton, fl_MouseButtons) =
  makeQtEnumAndFlags (ident1 "Qt" "MouseButton") "MouseButtons" qtInclude
  [ "NoButton"
  , "AllButtons"
  , "LeftButton"
  , "RightButton"
  , "MidButton"
  , "MiddleButton"
  , "BackButton"
  , "XButton1"
  , "ExtraButton1"
  , "ForwardButton"
  , "XButton2"
  , "ExtraButton2"
  , "TaskButton"
  , "ExtraButton3"
  , "ExtraButton4"
  , "ExtraButton5"
  , "ExtraButton6"
  , "ExtraButton7"
  , "ExtraButton8"
  , "ExtraButton9"
  , "ExtraButton10"
  , "ExtraButton11"
  , "ExtraButton12"
  , "ExtraButton13"
  , "ExtraButton14"
  , "ExtraButton15"
  , "ExtraButton16"
  , "ExtraButton17"
  , "ExtraButton18"
  , "ExtraButton19"
  , "ExtraButton20"
  , "ExtraButton21"
  , "ExtraButton22"
  , "ExtraButton23"
  , "ExtraButton24"
  ]

(e_MouseEventFlag, fl_MouseEventFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "MouseEventFlag") "MouseEventFlags" qtInclude
  [ "MouseEventCreatedDoubleClick"
  ]

e_MouseEventFlag_minVersion = [5, 3]

e_MouseEventSource =
  makeQtEnum (ident1 "Qt" "MouseEventSource") qtInclude
  [ "MouseEventNotSynthesized"
  , "MouseEventSynthesizedBySystem"
  , "MouseEventSynthesizedByQt"
  ]

e_MouseEventSource_minVersion = [5, 3]

e_NativeGestureType =
  makeQtEnum (ident1 "Qt" "NativeGestureType") qtInclude
  [ "BeginNativeGesture"
  , "EndNativeGesture"
  , "PanNativeGesture"
  , "ZoomNativeGesture"
  , "SmartZoomNativeGesture"
  , "RotateNativeGesture"
  , "SwipeNativeGesture"
  ]

e_NavigationMode =
  makeQtEnum (ident1 "Qt" "NavigationMode") qtInclude
  [ "NavigationModeNone"
  , "NavigationModeKeypadTabOrder"
  , "NavigationModeKeypadDirectional"
  , "NavigationModeCursorAuto"
  , "NavigationModeCursorForceVisible"
  ]

(e_Orientation, fl_Orientations) =
  makeQtEnumAndFlags (ident1 "Qt" "Orientation") "Orientations" qtInclude
  [ "Horizontal"
  , "Vertical"
  ]

e_PenCapStyle =
  makeQtEnum (ident1 "Qt" "PenCapStyle") qtInclude
  [ "FlatCap"
  , "SquareCap"
  , "RoundCap"
  ]

e_PenJoinStyle =
  makeQtEnum (ident1 "Qt" "PenJoinStyle") qtInclude
  [ "MiterJoin"
  , "BevelJoin"
  , "RoundJoin"
  , "SvgMiterJoin"
  ]

e_PenStyle =
  makeQtEnum (ident1 "Qt" "PenStyle") qtInclude
  [ "NoPen"
  , "SolidLine"
  , "DashLine"
  , "DotLine"
  , "DashDotLine"
  , "DashDotDotLine"
  , "CustomDashLine"
  ]

(e_ScreenOrientation, fl_ScreenOrientations) =
  makeQtEnumAndFlags (ident1 "Qt" "ScreenOrientation") "ScreenOrientations" qtInclude
  [ "PrimaryOrientation"
  , "PortraitOrientation"
  , "LandscapeOrientation"
  , "InvertedPortraitOrientation"
  , "InvertedLandscapeOrientation"
  ]

e_ScreenOrientation_minVersion = [5, 0]

e_ScrollBarPolicy =
  makeQtEnum (ident1 "Qt" "ScrollBarPolicy") qtInclude
  [ "ScrollBarAsNeeded"
  , "ScrollBarAlwaysOff"
  , "ScrollBarAlwaysOn"
  ]

e_ScrollPhase =
  makeQtEnum (ident1 "Qt" "ScrollPhase") qtInclude
  [ "ScrollBegin"
  , "ScrollUpdate"
  , "ScrollEnd"
  ]

e_ScrollPhase_minVersion = [5, 2]

e_ShortcutContext =
  makeQtEnum (ident1 "Qt" "ShortcutContext") qtInclude
  [ "WidgetShortcut"
  , "WidgetWithChildrenShortcut"
  , "WindowShortcut"
  , "ApplicationShortcut"
  ]

e_SizeHint =
  makeQtEnum (ident1 "Qt" "SizeHint") qtInclude
  [ "MinimumSize"
  , "PreferredSize"
  , "MaximumSize"
  , "MinimumDescent"
  ]

e_SizeMode =
  makeQtEnum (ident1 "Qt" "SizeMode") qtInclude
  [ "AbsoluteSize"
  , "RelativeSize"
  ]

e_SortOrder =
  makeQtEnum (ident1 "Qt" "SortOrder") qtInclude
  [ "AscendingOrder"
  , "DescendingOrder"
  ]

e_TabFocusBehavior =
  makeQtEnum (ident1 "Qt" "TabFocusBehavior") qtInclude
  [ "NoTabFocus"
  , "TabFocusTextControls"
  , "TabFocusListControls"
  , "TabFocusAllControls"
  ]

e_TextElideMode =
  makeQtEnum (ident1 "Qt" "TextElideMode") qtInclude
  [ "ElideLeft"
  , "ElideRight"
  , "ElideMiddle"
  , "ElideNone"
  ]

e_TextFlag =
  makeQtEnum (ident1 "Qt" "TextFlag") qtInclude
  [ "TextSingleLine"
  , "TextDontClip"
  , "TextExpandTabs"
  , "TextShowMnemonic"
  , "TextWordWrap"
  , "TextWrapAnywhere"
  , "TextHideMnemonic"
  , "TextDontPrint"
  , "TextIncludeTrailingSpaces"
  , "TextJustificationForced"
  ]

e_TextFormat =
  makeQtEnum (ident1 "Qt" "TextFormat") qtInclude $
  collect
  [ just "PlainText"
  , just "RichText"
  , just "AutoText"
  , test (qtVersion < [5, 0]) "LogText"
  ]

(e_TextInteractionFlag, fl_TextInteractionFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "TextInteractionFlag") "TextInteractionFlags" qtInclude
  [ "NoTextInteraction"
  , "TextSelectableByMouse"
  , "TextSelectableByKeyboard"
  , "LinksAccessibleByMouse"
  , "LinksAccessibleByKeyboard"
  , "TextEditable"
  , "TextEditorInteraction"
  , "TextBrowserInteraction"
  ]

e_TileRule =
  makeQtEnum (ident1 "Qt" "TileRule") qtInclude
  [ "StretchTile"
  , "RepeatTile"
  , "RoundTile"
  ]

e_TimeSpec =
  makeQtEnum (ident1 "Qt" "TimeSpec") qtInclude
  [ "LocalTime"
  , "UTC"
  , "OffsetFromUTC"
  , "TimeZone"
  ]

e_TimerType =
  makeQtEnum (ident1 "Qt" "TimerType") qtInclude
  [ "PreciseTimer"
  , "CoarseTimer"
  , "VeryCoarseTimer"
  ]

(e_ToolBarArea, fl_ToolBarAreas) =
  makeQtEnumAndFlags (ident1 "Qt" "ToolBarArea") "ToolBarAreas" qtInclude
  [ "NoToolBarArea"
  , "LeftToolBarArea"
  , "RightToolBarArea"
  , "TopToolBarArea"
  , "BottomToolBarArea"
  , "AllToolBarAreas"
  ]

e_ToolButtonStyle =
  makeQtEnum (ident1 "Qt" "ToolButtonStyle") qtInclude
  [ "ToolButtonIconOnly"
  , "ToolButtonTextOnly"
  , "ToolButtonTextBesideIcon"
  , "ToolButtonTextUnderIcon"
  , "ToolButtonFollowStyle"
  ]

(e_TouchPointState, fl_TouchPointStates) =
  makeQtEnumAndFlags (ident1 "Qt" "TouchPointState") "TouchPointStates" qtInclude
  [ "TouchPointPressed"
  , "TouchPointMoved"
  , "TouchPointStationary"
  , "TouchPointReleased"
  ]

e_TransformationMode =
  makeQtEnum (ident1 "Qt" "TransformationMode") qtInclude
  [ "FastTransformation"
  , "SmoothTransformation"
  ]

e_UIEffect =
  makeQtEnum (ident1 "Qt" "UIEffect") qtInclude
  [ "UI_AnimateMenu"
  , "UI_FadeMenu"
  , "UI_AnimateCombo"
  , "UI_AnimateTooltip"
  , "UI_FadeTooltip"
  , "UI_AnimateToolBox"
  ]

e_WhiteSpaceMode =
  makeQtEnum (ident1 "Qt" "WhiteSpaceMode") qtInclude
  [ "WhiteSpaceNormal"
  , "WhiteSpacePre"
  , "WhiteSpaceNoWrap"
  ]

e_WidgetAttribute =
  makeQtEnum (ident1 "Qt" "WidgetAttribute") qtInclude $
  collect
  [ just "WA_AcceptDrops"
  , just "WA_AlwaysShowToolTips"
  , just "WA_ContentsPropagated"
  , just "WA_CustomWhatsThis"
  , just "WA_DeleteOnClose"
  , just "WA_Disabled"
  , just "WA_DontShowOnScreen"
  , just "WA_ForceDisabled"
  , just "WA_ForceUpdatesDisabled"
  , just "WA_GroupLeader"
  , just "WA_Hover"
  , just "WA_InputMethodEnabled"
  , just "WA_KeyboardFocusChange"
  , just "WA_KeyCompression"
  , just "WA_LayoutOnEntireRect"
  , just "WA_LayoutUsesWidgetRect"
  , just "WA_MacNoClickThrough"
  , just "WA_MacOpaqueSizeGrip"
  , just "WA_MacShowFocusRect"
  , just "WA_MacNormalSize"
  , just "WA_MacSmallSize"
  , just "WA_MacMiniSize"
  , just "WA_MacVariableSize"
  , just "WA_MacBrushedMetal"
  , just "WA_Mapped"
  , just "WA_MouseNoMask"
  , just "WA_MouseTracking"
  , just "WA_Moved"
  , just "WA_MSWindowsUseDirect3D"
  , just "WA_NoChildEventsForParent"
  , just "WA_NoChildEventsFromChildren"
  , just "WA_NoMouseReplay"
  , just "WA_NoMousePropagation"
  , just "WA_TransparentForMouseEvents"
  , just "WA_NoSystemBackground"
  , just "WA_OpaquePaintEvent"
  , just "WA_OutsideWSRange"
  , just "WA_PaintOnScreen"
  , just "WA_PaintUnclipped"
  , just "WA_PendingMoveEvent"
  , just "WA_PendingResizeEvent"
  , just "WA_QuitOnClose"
  , just "WA_Resized"
  , just "WA_RightToLeft"
  , just "WA_SetCursor"
  , just "WA_SetFont"
  , just "WA_SetPalette"
  , just "WA_SetStyle"
  , just "WA_ShowModal"
  , just "WA_StaticContents"
  , just "WA_StyleSheet"
  , test (qtVersion >= [5, 12]) "WA_StyleSheetTarget"
  , test (qtVersion >= [5, 10]) "WA_TabletTracking"
  , just "WA_TranslucentBackground"
  , just "WA_UnderMouse"
  , just "WA_UpdatesDisabled"
  , just "WA_WindowModified"
  , just "WA_WindowPropagation"
  , just "WA_MacAlwaysShowToolWindow"
  , just "WA_SetLocale"
  , just "WA_StyledBackground"
  , just "WA_ShowWithoutActivating"
  , just "WA_NativeWindow"
  , just "WA_DontCreateNativeAncestors"
  , just "WA_X11NetWmWindowTypeDesktop"
  , just "WA_X11NetWmWindowTypeDock"
  , just "WA_X11NetWmWindowTypeToolBar"
  , just "WA_X11NetWmWindowTypeMenu"
  , just "WA_X11NetWmWindowTypeUtility"
  , just "WA_X11NetWmWindowTypeSplash"
  , just "WA_X11NetWmWindowTypeDialog"
  , just "WA_X11NetWmWindowTypeDropDownMenu"
  , just "WA_X11NetWmWindowTypePopupMenu"
  , just "WA_X11NetWmWindowTypeToolTip"
  , just "WA_X11NetWmWindowTypeNotification"
  , just "WA_X11NetWmWindowTypeCombo"
  , just "WA_X11NetWmWindowTypeDND"
  , just "WA_MacFrameworkScaled"
  , just "WA_AcceptTouchEvents"
  , just "WA_TouchPadAcceptSingleTouchEvents"
  , just "WA_X11DoNotAcceptFocus"
  , just "WA_AlwaysStackOnTop"
  , test (qtVersion >= [5, 11]) "WA_ContentsMarginsRespectsSafeArea"
  ]

e_WindowFrameSection =
  makeQtEnum (ident1 "Qt" "WindowFrameSection") qtInclude
  [ "NoSection"
  , "LeftSection"
  , "TopLeftSection"
  , "TopSection"
  , "TopRightSection"
  , "RightSection"
  , "BottomRightSection"
  , "BottomSection"
  , "BottomLeftSection"
  , "TitleBarArea"
  ]

e_WindowModality =
  makeQtEnum (ident1 "Qt" "WindowModality") qtInclude
  [ "NonModal"
  , "WindowModal"
  , "ApplicationModal"
  ]

(e_WindowState, fl_WindowStates) =
  makeQtEnumAndFlags (ident1 "Qt" "WindowState") "WindowStates" qtInclude
  [ "WindowNoState"
  , "WindowMinimized"
  , "WindowMaximized"
  , "WindowFullScreen"
  , "WindowActive"
  ]

(e_WindowType, fl_WindowFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "WindowType") "WindowFlags" qtInclude $
  [ "Widget"
  , "Window"
  , "Dialog"
  , "Sheet"
  , "Drawer"
  , "Popup"
  , "Tool"
  , "ToolTip"
  , "SplashScreen"
  , "Desktop"
  , "SubWindow"
  , "ForeignWindow"
  , "CoverWindow"
  ]

f_escape =
  addReqIncludes [includeStd "QTextDocument"] $
  makeFn (ident1 "Qt" "escape") Nothing Nonpure [objT c_QString] $ objT c_QString
