{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ----------
--  Object hierarchy for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Copyright (C) 2001-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This file reflects the Gtk+ object hierarchy in terms of Haskell classes.
--
-- Note: the mk... functions were originally meant to simply be an alias
-- for the constructor. However, in order to communicate the destructor
-- of an object to objectNew, the mk... functions are now a tuple containing
-- Haskell constructor and the destructor function pointer. This hack avoids
-- changing all modules that simply pass mk... to objectNew.
--
module Graphics.UI.Gtk.Types (

  module System.Glib.GObject,
  module Graphics.UI.Gtk.General.Threading,
  AtkObject(AtkObject), AtkObjectClass,
  toAtkObject, 
  mkAtkObject, unAtkObject,
  castToAtkObject, gTypeAtkObject,
  Accessible(Accessible), AccessibleClass,
  toAccessible, 
  mkAccessible, unAccessible,
  castToAccessible, gTypeAccessible,
  Keymap(Keymap), KeymapClass,
  toKeymap, 
  mkKeymap, unKeymap,
  castToKeymap, gTypeKeymap,
  DisplayManager(DisplayManager), DisplayManagerClass,
  toDisplayManager, 
  mkDisplayManager, unDisplayManager,
  castToDisplayManager, gTypeDisplayManager,
  AppLaunchContext(AppLaunchContext), AppLaunchContextClass,
  toAppLaunchContext, 
  mkAppLaunchContext, unAppLaunchContext,
  castToAppLaunchContext, gTypeAppLaunchContext,
  PrintSettings(PrintSettings), PrintSettingsClass,
  toPrintSettings, 
  mkPrintSettings, unPrintSettings,
  castToPrintSettings, gTypePrintSettings,
  PrintOperation(PrintOperation), PrintOperationClass,
  toPrintOperation, 
  mkPrintOperation, unPrintOperation,
  castToPrintOperation, gTypePrintOperation,
  PrintOperationPreview(PrintOperationPreview), PrintOperationPreviewClass,
  toPrintOperationPreview, 
  mkPrintOperationPreview, unPrintOperationPreview,
  castToPrintOperationPreview, gTypePrintOperationPreview,
  PageSetup(PageSetup), PageSetupClass,
  toPageSetup, 
  mkPageSetup, unPageSetup,
  castToPageSetup, gTypePageSetup,
  PrintContext(PrintContext), PrintContextClass,
  toPrintContext, 
  mkPrintContext, unPrintContext,
  castToPrintContext, gTypePrintContext,
  RecentChooser(RecentChooser), RecentChooserClass,
  toRecentChooser, 
  mkRecentChooser, unRecentChooser,
  castToRecentChooser, gTypeRecentChooser,
  RecentManager(RecentManager), RecentManagerClass,
  toRecentManager, 
  mkRecentManager, unRecentManager,
  castToRecentManager, gTypeRecentManager,
  DrawWindow(DrawWindow), DrawWindowClass,
  toDrawWindow, 
  mkDrawWindow, unDrawWindow,
  castToDrawWindow, gTypeDrawWindow,
  GLContext(GLContext), GLContextClass,
  toGLContext, 
  mkGLContext, unGLContext,
  castToGLContext, gTypeGLContext,
  Screen(Screen), ScreenClass,
  toScreen, 
  mkScreen, unScreen,
  castToScreen, gTypeScreen,
  Display(Display), DisplayClass,
  toDisplay, 
  mkDisplay, unDisplay,
  castToDisplay, gTypeDisplay,
  Visual(Visual), VisualClass,
  toVisual, 
  mkVisual, unVisual,
  castToVisual, gTypeVisual,
  Device(Device), DeviceClass,
  toDevice, 
  mkDevice, unDevice,
  castToDevice, gTypeDevice,
  FrameClock(FrameClock), FrameClockClass,
  toFrameClock, 
  mkFrameClock, unFrameClock,
  castToFrameClock, gTypeFrameClock,
  Settings(Settings), SettingsClass,
  toSettings, 
  mkSettings, unSettings,
  castToSettings, gTypeSettings,
  TextBuffer(TextBuffer), TextBufferClass,
  toTextBuffer, 
  mkTextBuffer, unTextBuffer,
  castToTextBuffer, gTypeTextBuffer,
  TextTag(TextTag), TextTagClass,
  toTextTag, 
  mkTextTag, unTextTag,
  castToTextTag, gTypeTextTag,
  TextTagTable(TextTagTable), TextTagTableClass,
  toTextTagTable, 
  mkTextTagTable, unTextTagTable,
  castToTextTagTable, gTypeTextTagTable,
  Style(Style), StyleClass,
  toStyle, 
  mkStyle, unStyle,
  castToStyle, gTypeStyle,
  RcStyle(RcStyle), RcStyleClass,
  toRcStyle, 
  mkRcStyle, unRcStyle,
  castToRcStyle, gTypeRcStyle,
  DragContext(DragContext), DragContextClass,
  toDragContext, 
  mkDragContext, unDragContext,
  castToDragContext, gTypeDragContext,
  Pixbuf(Pixbuf), PixbufClass,
  toPixbuf, 
  mkPixbuf, unPixbuf,
  castToPixbuf, gTypePixbuf,
  PixbufAnimation(PixbufAnimation), PixbufAnimationClass,
  toPixbufAnimation, 
  mkPixbufAnimation, unPixbufAnimation,
  castToPixbufAnimation, gTypePixbufAnimation,
  PixbufSimpleAnim(PixbufSimpleAnim), PixbufSimpleAnimClass,
  toPixbufSimpleAnim, 
  mkPixbufSimpleAnim, unPixbufSimpleAnim,
  castToPixbufSimpleAnim, gTypePixbufSimpleAnim,
  PixbufAnimationIter(PixbufAnimationIter), PixbufAnimationIterClass,
  toPixbufAnimationIter, 
  mkPixbufAnimationIter, unPixbufAnimationIter,
  castToPixbufAnimationIter, gTypePixbufAnimationIter,
  TextChildAnchor(TextChildAnchor), TextChildAnchorClass,
  toTextChildAnchor, 
  mkTextChildAnchor, unTextChildAnchor,
  castToTextChildAnchor, gTypeTextChildAnchor,
  TextMark(TextMark), TextMarkClass,
  toTextMark, 
  mkTextMark, unTextMark,
  castToTextMark, gTypeTextMark,
  RecentFilter(RecentFilter), RecentFilterClass,
  toRecentFilter, 
  mkRecentFilter, unRecentFilter,
  castToRecentFilter, gTypeRecentFilter,
  Widget(Widget), WidgetClass,
  toWidget, 
  mkWidget, unWidget,
  castToWidget, gTypeWidget,
  HSV(HSV), HSVClass,
  toHSV, 
  mkHSV, unHSV,
  castToHSV, gTypeHSV,
  Misc(Misc), MiscClass,
  toMisc, 
  mkMisc, unMisc,
  castToMisc, gTypeMisc,
  Label(Label), LabelClass,
  toLabel, 
  mkLabel, unLabel,
  castToLabel, gTypeLabel,
  AccelLabel(AccelLabel), AccelLabelClass,
  toAccelLabel, 
  mkAccelLabel, unAccelLabel,
  castToAccelLabel, gTypeAccelLabel,
  Arrow(Arrow), ArrowClass,
  toArrow, 
  mkArrow, unArrow,
  castToArrow, gTypeArrow,
  Image(Image), ImageClass,
  toImage, 
  mkImage, unImage,
  castToImage, gTypeImage,
  Switch(Switch), SwitchClass,
  toSwitch, 
  mkSwitch, unSwitch,
  castToSwitch, gTypeSwitch,
  Container(Container), ContainerClass,
  toContainer, 
  mkContainer, unContainer,
  castToContainer, gTypeContainer,
  ToolPalette(ToolPalette), ToolPaletteClass,
  toToolPalette, 
  mkToolPalette, unToolPalette,
  castToToolPalette, gTypeToolPalette,
  ToolItemGroup(ToolItemGroup), ToolItemGroupClass,
  toToolItemGroup, 
  mkToolItemGroup, unToolItemGroup,
  castToToolItemGroup, gTypeToolItemGroup,
  Stack(Stack), StackClass,
  toStack, 
  mkStack, unStack,
  castToStack, gTypeStack,
  Bin(Bin), BinClass,
  toBin, 
  mkBin, unBin,
  castToBin, gTypeBin,
  Alignment(Alignment), AlignmentClass,
  toAlignment, 
  mkAlignment, unAlignment,
  castToAlignment, gTypeAlignment,
  Frame(Frame), FrameClass,
  toFrame, 
  mkFrame, unFrame,
  castToFrame, gTypeFrame,
  AspectFrame(AspectFrame), AspectFrameClass,
  toAspectFrame, 
  mkAspectFrame, unAspectFrame,
  castToAspectFrame, gTypeAspectFrame,
  Button(Button), ButtonClass,
  toButton, 
  mkButton, unButton,
  castToButton, gTypeButton,
  ScaleButton(ScaleButton), ScaleButtonClass,
  toScaleButton, 
  mkScaleButton, unScaleButton,
  castToScaleButton, gTypeScaleButton,
  VolumeButton(VolumeButton), VolumeButtonClass,
  toVolumeButton, 
  mkVolumeButton, unVolumeButton,
  castToVolumeButton, gTypeVolumeButton,
  LinkButton(LinkButton), LinkButtonClass,
  toLinkButton, 
  mkLinkButton, unLinkButton,
  castToLinkButton, gTypeLinkButton,
  ToggleButton(ToggleButton), ToggleButtonClass,
  toToggleButton, 
  mkToggleButton, unToggleButton,
  castToToggleButton, gTypeToggleButton,
  CheckButton(CheckButton), CheckButtonClass,
  toCheckButton, 
  mkCheckButton, unCheckButton,
  castToCheckButton, gTypeCheckButton,
  RadioButton(RadioButton), RadioButtonClass,
  toRadioButton, 
  mkRadioButton, unRadioButton,
  castToRadioButton, gTypeRadioButton,
  ColorButton(ColorButton), ColorButtonClass,
  toColorButton, 
  mkColorButton, unColorButton,
  castToColorButton, gTypeColorButton,
  FontButton(FontButton), FontButtonClass,
  toFontButton, 
  mkFontButton, unFontButton,
  castToFontButton, gTypeFontButton,
  MenuItem(MenuItem), MenuItemClass,
  toMenuItem, 
  mkMenuItem, unMenuItem,
  castToMenuItem, gTypeMenuItem,
  CheckMenuItem(CheckMenuItem), CheckMenuItemClass,
  toCheckMenuItem, 
  mkCheckMenuItem, unCheckMenuItem,
  castToCheckMenuItem, gTypeCheckMenuItem,
  RadioMenuItem(RadioMenuItem), RadioMenuItemClass,
  toRadioMenuItem, 
  mkRadioMenuItem, unRadioMenuItem,
  castToRadioMenuItem, gTypeRadioMenuItem,
  TearoffMenuItem(TearoffMenuItem), TearoffMenuItemClass,
  toTearoffMenuItem, 
  mkTearoffMenuItem, unTearoffMenuItem,
  castToTearoffMenuItem, gTypeTearoffMenuItem,
  ImageMenuItem(ImageMenuItem), ImageMenuItemClass,
  toImageMenuItem, 
  mkImageMenuItem, unImageMenuItem,
  castToImageMenuItem, gTypeImageMenuItem,
  SeparatorMenuItem(SeparatorMenuItem), SeparatorMenuItemClass,
  toSeparatorMenuItem, 
  mkSeparatorMenuItem, unSeparatorMenuItem,
  castToSeparatorMenuItem, gTypeSeparatorMenuItem,
  Overlay(Overlay), OverlayClass,
  toOverlay, 
  mkOverlay, unOverlay,
  castToOverlay, gTypeOverlay,
  Window(Window), WindowClass,
  toWindow, 
  mkWindow, unWindow,
  castToWindow, gTypeWindow,
  Assistant(Assistant), AssistantClass,
  toAssistant, 
  mkAssistant, unAssistant,
  castToAssistant, gTypeAssistant,
  OffscreenWindow(OffscreenWindow), OffscreenWindowClass,
  toOffscreenWindow, 
  mkOffscreenWindow, unOffscreenWindow,
  castToOffscreenWindow, gTypeOffscreenWindow,
  Dialog(Dialog), DialogClass,
  toDialog, 
  mkDialog, unDialog,
  castToDialog, gTypeDialog,
  AboutDialog(AboutDialog), AboutDialogClass,
  toAboutDialog, 
  mkAboutDialog, unAboutDialog,
  castToAboutDialog, gTypeAboutDialog,
  ColorSelectionDialog(ColorSelectionDialog), ColorSelectionDialogClass,
  toColorSelectionDialog, 
  mkColorSelectionDialog, unColorSelectionDialog,
  castToColorSelectionDialog, gTypeColorSelectionDialog,
  FileChooserDialog(FileChooserDialog), FileChooserDialogClass,
  toFileChooserDialog, 
  mkFileChooserDialog, unFileChooserDialog,
  castToFileChooserDialog, gTypeFileChooserDialog,
  FontSelectionDialog(FontSelectionDialog), FontSelectionDialogClass,
  toFontSelectionDialog, 
  mkFontSelectionDialog, unFontSelectionDialog,
  castToFontSelectionDialog, gTypeFontSelectionDialog,
  MessageDialog(MessageDialog), MessageDialogClass,
  toMessageDialog, 
  mkMessageDialog, unMessageDialog,
  castToMessageDialog, gTypeMessageDialog,
  EventBox(EventBox), EventBoxClass,
  toEventBox, 
  mkEventBox, unEventBox,
  castToEventBox, gTypeEventBox,
  HandleBox(HandleBox), HandleBoxClass,
  toHandleBox, 
  mkHandleBox, unHandleBox,
  castToHandleBox, gTypeHandleBox,
  ScrolledWindow(ScrolledWindow), ScrolledWindowClass,
  toScrolledWindow, 
  mkScrolledWindow, unScrolledWindow,
  castToScrolledWindow, gTypeScrolledWindow,
  Viewport(Viewport), ViewportClass,
  toViewport, 
  mkViewport, unViewport,
  castToViewport, gTypeViewport,
  Expander(Expander), ExpanderClass,
  toExpander, 
  mkExpander, unExpander,
  castToExpander, gTypeExpander,
  ComboBox(ComboBox), ComboBoxClass,
  toComboBox, 
  mkComboBox, unComboBox,
  castToComboBox, gTypeComboBox,
  ToolItem(ToolItem), ToolItemClass,
  toToolItem, 
  mkToolItem, unToolItem,
  castToToolItem, gTypeToolItem,
  ToolButton(ToolButton), ToolButtonClass,
  toToolButton, 
  mkToolButton, unToolButton,
  castToToolButton, gTypeToolButton,
  MenuToolButton(MenuToolButton), MenuToolButtonClass,
  toMenuToolButton, 
  mkMenuToolButton, unMenuToolButton,
  castToMenuToolButton, gTypeMenuToolButton,
  ToggleToolButton(ToggleToolButton), ToggleToolButtonClass,
  toToggleToolButton, 
  mkToggleToolButton, unToggleToolButton,
  castToToggleToolButton, gTypeToggleToolButton,
  RadioToolButton(RadioToolButton), RadioToolButtonClass,
  toRadioToolButton, 
  mkRadioToolButton, unRadioToolButton,
  castToRadioToolButton, gTypeRadioToolButton,
  SeparatorToolItem(SeparatorToolItem), SeparatorToolItemClass,
  toSeparatorToolItem, 
  mkSeparatorToolItem, unSeparatorToolItem,
  castToSeparatorToolItem, gTypeSeparatorToolItem,
  StackSwitcher(StackSwitcher), StackSwitcherClass,
  toStackSwitcher, 
  mkStackSwitcher, unStackSwitcher,
  castToStackSwitcher, gTypeStackSwitcher,
  Box(Box), BoxClass,
  toBox, 
  mkBox, unBox,
  castToBox, gTypeBox,
  ButtonBox(ButtonBox), ButtonBoxClass,
  toButtonBox, 
  mkButtonBox, unButtonBox,
  castToButtonBox, gTypeButtonBox,
  HButtonBox(HButtonBox), HButtonBoxClass,
  toHButtonBox, 
  mkHButtonBox, unHButtonBox,
  castToHButtonBox, gTypeHButtonBox,
  VButtonBox(VButtonBox), VButtonBoxClass,
  toVButtonBox, 
  mkVButtonBox, unVButtonBox,
  castToVButtonBox, gTypeVButtonBox,
  VBox(VBox), VBoxClass,
  toVBox, 
  mkVBox, unVBox,
  castToVBox, gTypeVBox,
  RecentChooserWidget(RecentChooserWidget), RecentChooserWidgetClass,
  toRecentChooserWidget, 
  mkRecentChooserWidget, unRecentChooserWidget,
  castToRecentChooserWidget, gTypeRecentChooserWidget,
  ColorSelection(ColorSelection), ColorSelectionClass,
  toColorSelection, 
  mkColorSelection, unColorSelection,
  castToColorSelection, gTypeColorSelection,
  FontSelection(FontSelection), FontSelectionClass,
  toFontSelection, 
  mkFontSelection, unFontSelection,
  castToFontSelection, gTypeFontSelection,
  FileChooserWidget(FileChooserWidget), FileChooserWidgetClass,
  toFileChooserWidget, 
  mkFileChooserWidget, unFileChooserWidget,
  castToFileChooserWidget, gTypeFileChooserWidget,
  HBox(HBox), HBoxClass,
  toHBox, 
  mkHBox, unHBox,
  castToHBox, gTypeHBox,
  InfoBar(InfoBar), InfoBarClass,
  toInfoBar, 
  mkInfoBar, unInfoBar,
  castToInfoBar, gTypeInfoBar,
  FileChooserButton(FileChooserButton), FileChooserButtonClass,
  toFileChooserButton, 
  mkFileChooserButton, unFileChooserButton,
  castToFileChooserButton, gTypeFileChooserButton,
  Statusbar(Statusbar), StatusbarClass,
  toStatusbar, 
  mkStatusbar, unStatusbar,
  castToStatusbar, gTypeStatusbar,
  Grid(Grid), GridClass,
  toGrid, 
  mkGrid, unGrid,
  castToGrid, gTypeGrid,
  Fixed(Fixed), FixedClass,
  toFixed, 
  mkFixed, unFixed,
  castToFixed, gTypeFixed,
  Paned(Paned), PanedClass,
  toPaned, 
  mkPaned, unPaned,
  castToPaned, gTypePaned,
  HPaned(HPaned), HPanedClass,
  toHPaned, 
  mkHPaned, unHPaned,
  castToHPaned, gTypeHPaned,
  VPaned(VPaned), VPanedClass,
  toVPaned, 
  mkVPaned, unVPaned,
  castToVPaned, gTypeVPaned,
  IconView(IconView), IconViewClass,
  toIconView, 
  mkIconView, unIconView,
  castToIconView, gTypeIconView,
  Layout(Layout), LayoutClass,
  toLayout, 
  mkLayout, unLayout,
  castToLayout, gTypeLayout,
  MenuShell(MenuShell), MenuShellClass,
  toMenuShell, 
  mkMenuShell, unMenuShell,
  castToMenuShell, gTypeMenuShell,
  Menu(Menu), MenuClass,
  toMenu, 
  mkMenu, unMenu,
  castToMenu, gTypeMenu,
  RecentChooserMenu(RecentChooserMenu), RecentChooserMenuClass,
  toRecentChooserMenu, 
  mkRecentChooserMenu, unRecentChooserMenu,
  castToRecentChooserMenu, gTypeRecentChooserMenu,
  MenuBar(MenuBar), MenuBarClass,
  toMenuBar, 
  mkMenuBar, unMenuBar,
  castToMenuBar, gTypeMenuBar,
  Notebook(Notebook), NotebookClass,
  toNotebook, 
  mkNotebook, unNotebook,
  castToNotebook, gTypeNotebook,
  Table(Table), TableClass,
  toTable, 
  mkTable, unTable,
  castToTable, gTypeTable,
  TextView(TextView), TextViewClass,
  toTextView, 
  mkTextView, unTextView,
  castToTextView, gTypeTextView,
  Toolbar(Toolbar), ToolbarClass,
  toToolbar, 
  mkToolbar, unToolbar,
  castToToolbar, gTypeToolbar,
  TreeView(TreeView), TreeViewClass,
  toTreeView, 
  mkTreeView, unTreeView,
  castToTreeView, gTypeTreeView,
  Calendar(Calendar), CalendarClass,
  toCalendar, 
  mkCalendar, unCalendar,
  castToCalendar, gTypeCalendar,
  CellView(CellView), CellViewClass,
  toCellView, 
  mkCellView, unCellView,
  castToCellView, gTypeCellView,
  GLArea(GLArea), GLAreaClass,
  toGLArea, 
  mkGLArea, unGLArea,
  castToGLArea, gTypeGLArea,
  DrawingArea(DrawingArea), DrawingAreaClass,
  toDrawingArea, 
  mkDrawingArea, unDrawingArea,
  castToDrawingArea, gTypeDrawingArea,
  Spinner(Spinner), SpinnerClass,
  toSpinner, 
  mkSpinner, unSpinner,
  castToSpinner, gTypeSpinner,
  Entry(Entry), EntryClass,
  toEntry, 
  mkEntry, unEntry,
  castToEntry, gTypeEntry,
  SpinButton(SpinButton), SpinButtonClass,
  toSpinButton, 
  mkSpinButton, unSpinButton,
  castToSpinButton, gTypeSpinButton,
  Range(Range), RangeClass,
  toRange, 
  mkRange, unRange,
  castToRange, gTypeRange,
  Scale(Scale), ScaleClass,
  toScale, 
  mkScale, unScale,
  castToScale, gTypeScale,
  HScale(HScale), HScaleClass,
  toHScale, 
  mkHScale, unHScale,
  castToHScale, gTypeHScale,
  VScale(VScale), VScaleClass,
  toVScale, 
  mkVScale, unVScale,
  castToVScale, gTypeVScale,
  Scrollbar(Scrollbar), ScrollbarClass,
  toScrollbar, 
  mkScrollbar, unScrollbar,
  castToScrollbar, gTypeScrollbar,
  HScrollbar(HScrollbar), HScrollbarClass,
  toHScrollbar, 
  mkHScrollbar, unHScrollbar,
  castToHScrollbar, gTypeHScrollbar,
  VScrollbar(VScrollbar), VScrollbarClass,
  toVScrollbar, 
  mkVScrollbar, unVScrollbar,
  castToVScrollbar, gTypeVScrollbar,
  Separator(Separator), SeparatorClass,
  toSeparator, 
  mkSeparator, unSeparator,
  castToSeparator, gTypeSeparator,
  HSeparator(HSeparator), HSeparatorClass,
  toHSeparator, 
  mkHSeparator, unHSeparator,
  castToHSeparator, gTypeHSeparator,
  VSeparator(VSeparator), VSeparatorClass,
  toVSeparator, 
  mkVSeparator, unVSeparator,
  castToVSeparator, gTypeVSeparator,
  Invisible(Invisible), InvisibleClass,
  toInvisible, 
  mkInvisible, unInvisible,
  castToInvisible, gTypeInvisible,
  ProgressBar(ProgressBar), ProgressBarClass,
  toProgressBar, 
  mkProgressBar, unProgressBar,
  castToProgressBar, gTypeProgressBar,
  LevelBar(LevelBar), LevelBarClass,
  toLevelBar, 
  mkLevelBar, unLevelBar,
  castToLevelBar, gTypeLevelBar,
  Adjustment(Adjustment), AdjustmentClass,
  toAdjustment, 
  mkAdjustment, unAdjustment,
  castToAdjustment, gTypeAdjustment,
  IMContext(IMContext), IMContextClass,
  toIMContext, 
  mkIMContext, unIMContext,
  castToIMContext, gTypeIMContext,
  IMMulticontext(IMMulticontext), IMMulticontextClass,
  toIMMulticontext, 
  mkIMMulticontext, unIMMulticontext,
  castToIMMulticontext, gTypeIMMulticontext,
  IMContextSimple(IMContextSimple), IMContextSimpleClass,
  toIMContextSimple, 
  mkIMContextSimple, unIMContextSimple,
  castToIMContextSimple, gTypeIMContextSimple,
  TreeViewColumn(TreeViewColumn), TreeViewColumnClass,
  toTreeViewColumn, 
  mkTreeViewColumn, unTreeViewColumn,
  castToTreeViewColumn, gTypeTreeViewColumn,
  CellRenderer(CellRenderer), CellRendererClass,
  toCellRenderer, 
  mkCellRenderer, unCellRenderer,
  castToCellRenderer, gTypeCellRenderer,
  CellRendererSpinner(CellRendererSpinner), CellRendererSpinnerClass,
  toCellRendererSpinner, 
  mkCellRendererSpinner, unCellRendererSpinner,
  castToCellRendererSpinner, gTypeCellRendererSpinner,
  CellRendererPixbuf(CellRendererPixbuf), CellRendererPixbufClass,
  toCellRendererPixbuf, 
  mkCellRendererPixbuf, unCellRendererPixbuf,
  castToCellRendererPixbuf, gTypeCellRendererPixbuf,
  CellRendererText(CellRendererText), CellRendererTextClass,
  toCellRendererText, 
  mkCellRendererText, unCellRendererText,
  castToCellRendererText, gTypeCellRendererText,
  CellRendererAccel(CellRendererAccel), CellRendererAccelClass,
  toCellRendererAccel, 
  mkCellRendererAccel, unCellRendererAccel,
  castToCellRendererAccel, gTypeCellRendererAccel,
  CellRendererSpin(CellRendererSpin), CellRendererSpinClass,
  toCellRendererSpin, 
  mkCellRendererSpin, unCellRendererSpin,
  castToCellRendererSpin, gTypeCellRendererSpin,
  CellRendererCombo(CellRendererCombo), CellRendererComboClass,
  toCellRendererCombo, 
  mkCellRendererCombo, unCellRendererCombo,
  castToCellRendererCombo, gTypeCellRendererCombo,
  CellRendererToggle(CellRendererToggle), CellRendererToggleClass,
  toCellRendererToggle, 
  mkCellRendererToggle, unCellRendererToggle,
  castToCellRendererToggle, gTypeCellRendererToggle,
  CellRendererProgress(CellRendererProgress), CellRendererProgressClass,
  toCellRendererProgress, 
  mkCellRendererProgress, unCellRendererProgress,
  castToCellRendererProgress, gTypeCellRendererProgress,
  FileFilter(FileFilter), FileFilterClass,
  toFileFilter, 
  mkFileFilter, unFileFilter,
  castToFileFilter, gTypeFileFilter,
  Builder(Builder), BuilderClass,
  toBuilder, 
  mkBuilder, unBuilder,
  castToBuilder, gTypeBuilder,
  StyleContext(StyleContext), StyleContextClass,
  toStyleContext, 
  mkStyleContext, unStyleContext,
  castToStyleContext, gTypeStyleContext,
  StyleProvider(StyleProvider), StyleProviderClass,
  toStyleProvider, 
  mkStyleProvider, unStyleProvider,
  castToStyleProvider, gTypeStyleProvider,
  CssProvider(CssProvider), CssProviderClass,
  toCssProvider, 
  mkCssProvider, unCssProvider,
  castToCssProvider, gTypeCssProvider,
  CellLayout(CellLayout), CellLayoutClass,
  toCellLayout, 
  mkCellLayout, unCellLayout,
  castToCellLayout, gTypeCellLayout,
  TreeSortable(TreeSortable), TreeSortableClass,
  toTreeSortable, 
  mkTreeSortable, unTreeSortable,
  castToTreeSortable, gTypeTreeSortable,
  Tooltip(Tooltip), TooltipClass,
  toTooltip, 
  mkTooltip, unTooltip,
  castToTooltip, gTypeTooltip,
  StatusIcon(StatusIcon), StatusIconClass,
  toStatusIcon, 
  mkStatusIcon, unStatusIcon,
  castToStatusIcon, gTypeStatusIcon,
  TreeSelection(TreeSelection), TreeSelectionClass,
  toTreeSelection, 
  mkTreeSelection, unTreeSelection,
  castToTreeSelection, gTypeTreeSelection,
  TreeModel(TreeModel), TreeModelClass,
  toTreeModel, 
  mkTreeModel, unTreeModel,
  castToTreeModel, gTypeTreeModel,
  TreeStore(TreeStore), TreeStoreClass,
  toTreeStore, 
  mkTreeStore, unTreeStore,
  castToTreeStore, gTypeTreeStore,
  ListStore(ListStore), ListStoreClass,
  toListStore, 
  mkListStore, unListStore,
  castToListStore, gTypeListStore,
  TreeModelSort(TreeModelSort), TreeModelSortClass,
  toTreeModelSort, 
  mkTreeModelSort, unTreeModelSort,
  castToTreeModelSort, gTypeTreeModelSort,
  TreeModelFilter(TreeModelFilter), TreeModelFilterClass,
  toTreeModelFilter, 
  mkTreeModelFilter, unTreeModelFilter,
  castToTreeModelFilter, gTypeTreeModelFilter,
  IconFactory(IconFactory), IconFactoryClass,
  toIconFactory, 
  mkIconFactory, unIconFactory,
  castToIconFactory, gTypeIconFactory,
  IconTheme(IconTheme), IconThemeClass,
  toIconTheme, 
  mkIconTheme, unIconTheme,
  castToIconTheme, gTypeIconTheme,
  SizeGroup(SizeGroup), SizeGroupClass,
  toSizeGroup, 
  mkSizeGroup, unSizeGroup,
  castToSizeGroup, gTypeSizeGroup,
  Clipboard(Clipboard), ClipboardClass,
  toClipboard, 
  mkClipboard, unClipboard,
  castToClipboard, gTypeClipboard,
  AccelGroup(AccelGroup), AccelGroupClass,
  toAccelGroup, 
  mkAccelGroup, unAccelGroup,
  castToAccelGroup, gTypeAccelGroup,
  AccelMap(AccelMap), AccelMapClass,
  toAccelMap, 
  mkAccelMap, unAccelMap,
  castToAccelMap, gTypeAccelMap,
  EntryCompletion(EntryCompletion), EntryCompletionClass,
  toEntryCompletion, 
  mkEntryCompletion, unEntryCompletion,
  castToEntryCompletion, gTypeEntryCompletion,
  EntryBuffer(EntryBuffer), EntryBufferClass,
  toEntryBuffer, 
  mkEntryBuffer, unEntryBuffer,
  castToEntryBuffer, gTypeEntryBuffer,
  Action(Action), ActionClass,
  toAction, 
  mkAction, unAction,
  castToAction, gTypeAction,
  RecentAction(RecentAction), RecentActionClass,
  toRecentAction, 
  mkRecentAction, unRecentAction,
  castToRecentAction, gTypeRecentAction,
  ToggleAction(ToggleAction), ToggleActionClass,
  toToggleAction, 
  mkToggleAction, unToggleAction,
  castToToggleAction, gTypeToggleAction,
  RadioAction(RadioAction), RadioActionClass,
  toRadioAction, 
  mkRadioAction, unRadioAction,
  castToRadioAction, gTypeRadioAction,
  ActionGroup(ActionGroup), ActionGroupClass,
  toActionGroup, 
  mkActionGroup, unActionGroup,
  castToActionGroup, gTypeActionGroup,
  UIManager(UIManager), UIManagerClass,
  toUIManager, 
  mkUIManager, unUIManager,
  castToUIManager, gTypeUIManager,
  WindowGroup(WindowGroup), WindowGroupClass,
  toWindowGroup, 
  mkWindowGroup, unWindowGroup,
  castToWindowGroup, gTypeWindowGroup,
  CellEditable(CellEditable), CellEditableClass,
  toCellEditable, 
  mkCellEditable, unCellEditable,
  castToCellEditable, gTypeCellEditable,
  Editable(Editable), EditableClass,
  toEditable, 
  mkEditable, unEditable,
  castToEditable, gTypeEditable,
  FileChooser(FileChooser), FileChooserClass,
  toFileChooser, 
  mkFileChooser, unFileChooser,
  castToFileChooser, gTypeFileChooser
  ) where

import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
-- TODO work around cpphs https://ghc.haskell.org/trac/ghc/ticket/13553
#if __GLASGOW_HASKELL__ >= 707 || __GLASGOW_HASKELL__ == 0
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#else
import Foreign.ForeignPtr (unsafeForeignPtrToPtr)
#endif

import Foreign.C.Types    (CULong(..), CUInt(..), CULLong(..))
import System.Glib.GType  (GType, typeInstanceIsA)
{#import System.Glib.GObject#}
import Graphics.UI.Gtk.General.Threading

{# context lib="gtk" prefix="gtk" #}

-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
--
castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName


-- ****************************************************************** AtkObject

{#pointer *AtkObject as AtkObject foreign newtype #} deriving (Eq,Ord)

mkAtkObject = (AtkObject, objectUnrefFromMainloop)
unAtkObject (AtkObject o) = o

class GObjectClass o => AtkObjectClass o
toAtkObject :: AtkObjectClass o => o -> AtkObject
toAtkObject = unsafeCastGObject . toGObject

instance AtkObjectClass AtkObject
instance GObjectClass AtkObject where
  toGObject = GObject . castForeignPtr . unAtkObject
  unsafeCastGObject = AtkObject . castForeignPtr . unGObject

castToAtkObject :: GObjectClass obj => obj -> AtkObject
castToAtkObject = castTo gTypeAtkObject "AtkObject"

gTypeAtkObject :: GType
gTypeAtkObject =
  {# call fun unsafe atk_object_get_type #}

-- ***************************************************************** Accessible

{#pointer *GtkAccessible as Accessible foreign newtype #} deriving (Eq,Ord)

mkAccessible = (Accessible, objectUnrefFromMainloop)
unAccessible (Accessible o) = o

class AtkObjectClass o => AccessibleClass o
toAccessible :: AccessibleClass o => o -> Accessible
toAccessible = unsafeCastGObject . toGObject

instance AccessibleClass Accessible
instance AtkObjectClass Accessible
instance GObjectClass Accessible where
  toGObject = GObject . castForeignPtr . unAccessible
  unsafeCastGObject = Accessible . castForeignPtr . unGObject

castToAccessible :: GObjectClass obj => obj -> Accessible
castToAccessible = castTo gTypeAccessible "Accessible"

gTypeAccessible :: GType
gTypeAccessible =
  {# call fun unsafe gtk_accessible_get_type #}

-- ********************************************************************* Keymap

{#pointer *GdkKeymap as Keymap foreign newtype #} deriving (Eq,Ord)

mkKeymap = (Keymap, objectUnrefFromMainloop)
unKeymap (Keymap o) = o

class GObjectClass o => KeymapClass o
toKeymap :: KeymapClass o => o -> Keymap
toKeymap = unsafeCastGObject . toGObject

instance KeymapClass Keymap
instance GObjectClass Keymap where
  toGObject = GObject . castForeignPtr . unKeymap
  unsafeCastGObject = Keymap . castForeignPtr . unGObject

castToKeymap :: GObjectClass obj => obj -> Keymap
castToKeymap = castTo gTypeKeymap "Keymap"

gTypeKeymap :: GType
gTypeKeymap =
  {# call fun unsafe gdk_keymap_get_type #}

-- ************************************************************* DisplayManager

{#pointer *GdkDisplayManager as DisplayManager foreign newtype #} deriving (Eq,Ord)

mkDisplayManager = (DisplayManager, objectUnrefFromMainloop)
unDisplayManager (DisplayManager o) = o

class GObjectClass o => DisplayManagerClass o
toDisplayManager :: DisplayManagerClass o => o -> DisplayManager
toDisplayManager = unsafeCastGObject . toGObject

instance DisplayManagerClass DisplayManager
instance GObjectClass DisplayManager where
  toGObject = GObject . castForeignPtr . unDisplayManager
  unsafeCastGObject = DisplayManager . castForeignPtr . unGObject

castToDisplayManager :: GObjectClass obj => obj -> DisplayManager
castToDisplayManager = castTo gTypeDisplayManager "DisplayManager"

gTypeDisplayManager :: GType
gTypeDisplayManager =
  {# call fun unsafe gdk_display_manager_get_type #}

-- *********************************************************** AppLaunchContext

{#pointer *GdkAppLaunchContext as AppLaunchContext foreign newtype #} deriving (Eq,Ord)

mkAppLaunchContext = (AppLaunchContext, objectUnrefFromMainloop)
unAppLaunchContext (AppLaunchContext o) = o

class GObjectClass o => AppLaunchContextClass o
toAppLaunchContext :: AppLaunchContextClass o => o -> AppLaunchContext
toAppLaunchContext = unsafeCastGObject . toGObject

instance AppLaunchContextClass AppLaunchContext
instance GObjectClass AppLaunchContext where
  toGObject = GObject . castForeignPtr . unAppLaunchContext
  unsafeCastGObject = AppLaunchContext . castForeignPtr . unGObject

castToAppLaunchContext :: GObjectClass obj => obj -> AppLaunchContext
castToAppLaunchContext = castTo gTypeAppLaunchContext "AppLaunchContext"

gTypeAppLaunchContext :: GType
gTypeAppLaunchContext =
  {# call fun unsafe gdk_app_launch_context_get_type #}

-- ************************************************************** PrintSettings

{#pointer *GtkPrintSettings as PrintSettings foreign newtype #} deriving (Eq,Ord)

mkPrintSettings = (PrintSettings, objectUnrefFromMainloop)
unPrintSettings (PrintSettings o) = o

class GObjectClass o => PrintSettingsClass o
toPrintSettings :: PrintSettingsClass o => o -> PrintSettings
toPrintSettings = unsafeCastGObject . toGObject

instance PrintSettingsClass PrintSettings
instance GObjectClass PrintSettings where
  toGObject = GObject . castForeignPtr . unPrintSettings
  unsafeCastGObject = PrintSettings . castForeignPtr . unGObject

castToPrintSettings :: GObjectClass obj => obj -> PrintSettings
castToPrintSettings = castTo gTypePrintSettings "PrintSettings"

gTypePrintSettings :: GType
gTypePrintSettings =
  {# call fun unsafe gtk_print_settings_get_type #}

-- ************************************************************* PrintOperation

{#pointer *GtkPrintOperation as PrintOperation foreign newtype #} deriving (Eq,Ord)

mkPrintOperation = (PrintOperation, objectUnrefFromMainloop)
unPrintOperation (PrintOperation o) = o

class GObjectClass o => PrintOperationClass o
toPrintOperation :: PrintOperationClass o => o -> PrintOperation
toPrintOperation = unsafeCastGObject . toGObject

instance PrintOperationClass PrintOperation
instance GObjectClass PrintOperation where
  toGObject = GObject . castForeignPtr . unPrintOperation
  unsafeCastGObject = PrintOperation . castForeignPtr . unGObject

castToPrintOperation :: GObjectClass obj => obj -> PrintOperation
castToPrintOperation = castTo gTypePrintOperation "PrintOperation"

gTypePrintOperation :: GType
gTypePrintOperation =
  {# call fun unsafe gtk_print_operation_get_type #}

-- ****************************************************** PrintOperationPreview

{#pointer *GtkPrintOperationPreview as PrintOperationPreview foreign newtype #} deriving (Eq,Ord)

mkPrintOperationPreview = (PrintOperationPreview, objectUnrefFromMainloop)
unPrintOperationPreview (PrintOperationPreview o) = o

class GObjectClass o => PrintOperationPreviewClass o
toPrintOperationPreview :: PrintOperationPreviewClass o => o -> PrintOperationPreview
toPrintOperationPreview = unsafeCastGObject . toGObject

instance PrintOperationPreviewClass PrintOperationPreview
instance GObjectClass PrintOperationPreview where
  toGObject = GObject . castForeignPtr . unPrintOperationPreview
  unsafeCastGObject = PrintOperationPreview . castForeignPtr . unGObject

castToPrintOperationPreview :: GObjectClass obj => obj -> PrintOperationPreview
castToPrintOperationPreview = castTo gTypePrintOperationPreview "PrintOperationPreview"

gTypePrintOperationPreview :: GType
gTypePrintOperationPreview =
  {# call fun unsafe gtk_print_operation_preview_get_type #}

-- ****************************************************************** PageSetup

{#pointer *GtkPageSetup as PageSetup foreign newtype #} deriving (Eq,Ord)

mkPageSetup = (PageSetup, objectUnrefFromMainloop)
unPageSetup (PageSetup o) = o

class GObjectClass o => PageSetupClass o
toPageSetup :: PageSetupClass o => o -> PageSetup
toPageSetup = unsafeCastGObject . toGObject

instance PageSetupClass PageSetup
instance GObjectClass PageSetup where
  toGObject = GObject . castForeignPtr . unPageSetup
  unsafeCastGObject = PageSetup . castForeignPtr . unGObject

castToPageSetup :: GObjectClass obj => obj -> PageSetup
castToPageSetup = castTo gTypePageSetup "PageSetup"

gTypePageSetup :: GType
gTypePageSetup =
  {# call fun unsafe gtk_page_setup_get_type #}

-- *************************************************************** PrintContext

{#pointer *GtkPrintContext as PrintContext foreign newtype #} deriving (Eq,Ord)

mkPrintContext = (PrintContext, objectUnrefFromMainloop)
unPrintContext (PrintContext o) = o

class GObjectClass o => PrintContextClass o
toPrintContext :: PrintContextClass o => o -> PrintContext
toPrintContext = unsafeCastGObject . toGObject

instance PrintContextClass PrintContext
instance GObjectClass PrintContext where
  toGObject = GObject . castForeignPtr . unPrintContext
  unsafeCastGObject = PrintContext . castForeignPtr . unGObject

castToPrintContext :: GObjectClass obj => obj -> PrintContext
castToPrintContext = castTo gTypePrintContext "PrintContext"

gTypePrintContext :: GType
gTypePrintContext =
  {# call fun unsafe gtk_print_context_get_type #}

-- ************************************************************** RecentChooser

{#pointer *GtkRecentChooser as RecentChooser foreign newtype #} deriving (Eq,Ord)

mkRecentChooser = (RecentChooser, objectUnrefFromMainloop)
unRecentChooser (RecentChooser o) = o

class GObjectClass o => RecentChooserClass o
toRecentChooser :: RecentChooserClass o => o -> RecentChooser
toRecentChooser = unsafeCastGObject . toGObject

instance RecentChooserClass RecentChooser
instance GObjectClass RecentChooser where
  toGObject = GObject . castForeignPtr . unRecentChooser
  unsafeCastGObject = RecentChooser . castForeignPtr . unGObject

castToRecentChooser :: GObjectClass obj => obj -> RecentChooser
castToRecentChooser = castTo gTypeRecentChooser "RecentChooser"

gTypeRecentChooser :: GType
gTypeRecentChooser =
  {# call fun unsafe gtk_recent_chooser_get_type #}

-- ************************************************************** RecentManager

{#pointer *GtkRecentManager as RecentManager foreign newtype #} deriving (Eq,Ord)

mkRecentManager = (RecentManager, objectUnrefFromMainloop)
unRecentManager (RecentManager o) = o

class GObjectClass o => RecentManagerClass o
toRecentManager :: RecentManagerClass o => o -> RecentManager
toRecentManager = unsafeCastGObject . toGObject

instance RecentManagerClass RecentManager
instance GObjectClass RecentManager where
  toGObject = GObject . castForeignPtr . unRecentManager
  unsafeCastGObject = RecentManager . castForeignPtr . unGObject

castToRecentManager :: GObjectClass obj => obj -> RecentManager
castToRecentManager = castTo gTypeRecentManager "RecentManager"

gTypeRecentManager :: GType
gTypeRecentManager =
  {# call fun unsafe gtk_recent_manager_get_type #}

-- ***************************************************************** DrawWindow

{#pointer *GdkWindow as DrawWindow foreign newtype #} deriving (Eq,Ord)

mkDrawWindow = (DrawWindow, objectUnrefFromMainloop)
unDrawWindow (DrawWindow o) = o

class GObjectClass o => DrawWindowClass o
toDrawWindow :: DrawWindowClass o => o -> DrawWindow
toDrawWindow = unsafeCastGObject . toGObject

instance DrawWindowClass DrawWindow
instance GObjectClass DrawWindow where
  toGObject = GObject . castForeignPtr . unDrawWindow
  unsafeCastGObject = DrawWindow . castForeignPtr . unGObject

castToDrawWindow :: GObjectClass obj => obj -> DrawWindow
castToDrawWindow = castTo gTypeDrawWindow "DrawWindow"

gTypeDrawWindow :: GType
gTypeDrawWindow =
  {# call fun unsafe gdk_window_get_type #}

-- ****************************************************************** GLContext

{#pointer *GdkGLContext as GLContext foreign newtype #} deriving (Eq,Ord)

mkGLContext = (GLContext, objectUnrefFromMainloop)
unGLContext (GLContext o) = o

class GObjectClass o => GLContextClass o
toGLContext :: GLContextClass o => o -> GLContext
toGLContext = unsafeCastGObject . toGObject

instance GLContextClass GLContext
instance GObjectClass GLContext where
  toGObject = GObject . castForeignPtr . unGLContext
  unsafeCastGObject = GLContext . castForeignPtr . unGObject

castToGLContext :: GObjectClass obj => obj -> GLContext
castToGLContext = castTo gTypeGLContext "GLContext"

gTypeGLContext :: GType
gTypeGLContext =
  {# call fun unsafe gdk_gl_context_get_type #}

-- ********************************************************************* Screen

{#pointer *GdkScreen as Screen foreign newtype #} deriving (Eq,Ord)

mkScreen = (Screen, objectUnrefFromMainloop)
unScreen (Screen o) = o

class GObjectClass o => ScreenClass o
toScreen :: ScreenClass o => o -> Screen
toScreen = unsafeCastGObject . toGObject

instance ScreenClass Screen
instance GObjectClass Screen where
  toGObject = GObject . castForeignPtr . unScreen
  unsafeCastGObject = Screen . castForeignPtr . unGObject

castToScreen :: GObjectClass obj => obj -> Screen
castToScreen = castTo gTypeScreen "Screen"

gTypeScreen :: GType
gTypeScreen =
  {# call fun unsafe gdk_screen_get_type #}

-- ******************************************************************** Display

{#pointer *GdkDisplay as Display foreign newtype #} deriving (Eq,Ord)

mkDisplay = (Display, objectUnrefFromMainloop)
unDisplay (Display o) = o

class GObjectClass o => DisplayClass o
toDisplay :: DisplayClass o => o -> Display
toDisplay = unsafeCastGObject . toGObject

instance DisplayClass Display
instance GObjectClass Display where
  toGObject = GObject . castForeignPtr . unDisplay
  unsafeCastGObject = Display . castForeignPtr . unGObject

castToDisplay :: GObjectClass obj => obj -> Display
castToDisplay = castTo gTypeDisplay "Display"

gTypeDisplay :: GType
gTypeDisplay =
  {# call fun unsafe gdk_display_get_type #}

-- ********************************************************************* Visual

{#pointer *GdkVisual as Visual foreign newtype #} deriving (Eq,Ord)

mkVisual = (Visual, objectUnrefFromMainloop)
unVisual (Visual o) = o

class GObjectClass o => VisualClass o
toVisual :: VisualClass o => o -> Visual
toVisual = unsafeCastGObject . toGObject

instance VisualClass Visual
instance GObjectClass Visual where
  toGObject = GObject . castForeignPtr . unVisual
  unsafeCastGObject = Visual . castForeignPtr . unGObject

castToVisual :: GObjectClass obj => obj -> Visual
castToVisual = castTo gTypeVisual "Visual"

gTypeVisual :: GType
gTypeVisual =
  {# call fun unsafe gdk_visual_get_type #}

-- ********************************************************************* Device

{#pointer *GdkDevice as Device foreign newtype #} deriving (Eq,Ord)

mkDevice = (Device, objectUnrefFromMainloop)
unDevice (Device o) = o

class GObjectClass o => DeviceClass o
toDevice :: DeviceClass o => o -> Device
toDevice = unsafeCastGObject . toGObject

instance DeviceClass Device
instance GObjectClass Device where
  toGObject = GObject . castForeignPtr . unDevice
  unsafeCastGObject = Device . castForeignPtr . unGObject

castToDevice :: GObjectClass obj => obj -> Device
castToDevice = castTo gTypeDevice "Device"

gTypeDevice :: GType
gTypeDevice =
  {# call fun unsafe gdk_device_get_type #}

-- ***************************************************************** FrameClock

{#pointer *GdkFrameClock as FrameClock foreign newtype #} deriving (Eq,Ord)

mkFrameClock = (FrameClock, objectUnrefFromMainloop)
unFrameClock (FrameClock o) = o

class GObjectClass o => FrameClockClass o
toFrameClock :: FrameClockClass o => o -> FrameClock
toFrameClock = unsafeCastGObject . toGObject

instance FrameClockClass FrameClock
instance GObjectClass FrameClock where
  toGObject = GObject . castForeignPtr . unFrameClock
  unsafeCastGObject = FrameClock . castForeignPtr . unGObject

castToFrameClock :: GObjectClass obj => obj -> FrameClock
castToFrameClock = castTo gTypeFrameClock "FrameClock"

gTypeFrameClock :: GType
gTypeFrameClock =
  {# call fun unsafe gdk_frame_clock_get_type #}

-- ******************************************************************* Settings

{#pointer *GtkSettings as Settings foreign newtype #} deriving (Eq,Ord)

mkSettings = (Settings, objectUnrefFromMainloop)
unSettings (Settings o) = o

class GObjectClass o => SettingsClass o
toSettings :: SettingsClass o => o -> Settings
toSettings = unsafeCastGObject . toGObject

instance SettingsClass Settings
instance GObjectClass Settings where
  toGObject = GObject . castForeignPtr . unSettings
  unsafeCastGObject = Settings . castForeignPtr . unGObject

castToSettings :: GObjectClass obj => obj -> Settings
castToSettings = castTo gTypeSettings "Settings"

gTypeSettings :: GType
gTypeSettings =
  {# call fun unsafe gtk_settings_get_type #}

-- ***************************************************************** TextBuffer

{#pointer *GtkTextBuffer as TextBuffer foreign newtype #} deriving (Eq,Ord)

mkTextBuffer = (TextBuffer, objectUnrefFromMainloop)
unTextBuffer (TextBuffer o) = o

class GObjectClass o => TextBufferClass o
toTextBuffer :: TextBufferClass o => o -> TextBuffer
toTextBuffer = unsafeCastGObject . toGObject

instance TextBufferClass TextBuffer
instance GObjectClass TextBuffer where
  toGObject = GObject . castForeignPtr . unTextBuffer
  unsafeCastGObject = TextBuffer . castForeignPtr . unGObject

castToTextBuffer :: GObjectClass obj => obj -> TextBuffer
castToTextBuffer = castTo gTypeTextBuffer "TextBuffer"

gTypeTextBuffer :: GType
gTypeTextBuffer =
  {# call fun unsafe gtk_text_buffer_get_type #}

-- ******************************************************************** TextTag

{#pointer *GtkTextTag as TextTag foreign newtype #} deriving (Eq,Ord)

mkTextTag = (TextTag, objectUnrefFromMainloop)
unTextTag (TextTag o) = o

class GObjectClass o => TextTagClass o
toTextTag :: TextTagClass o => o -> TextTag
toTextTag = unsafeCastGObject . toGObject

instance TextTagClass TextTag
instance GObjectClass TextTag where
  toGObject = GObject . castForeignPtr . unTextTag
  unsafeCastGObject = TextTag . castForeignPtr . unGObject

castToTextTag :: GObjectClass obj => obj -> TextTag
castToTextTag = castTo gTypeTextTag "TextTag"

gTypeTextTag :: GType
gTypeTextTag =
  {# call fun unsafe gtk_text_tag_get_type #}

-- *************************************************************** TextTagTable

{#pointer *GtkTextTagTable as TextTagTable foreign newtype #} deriving (Eq,Ord)

mkTextTagTable = (TextTagTable, objectUnrefFromMainloop)
unTextTagTable (TextTagTable o) = o

class GObjectClass o => TextTagTableClass o
toTextTagTable :: TextTagTableClass o => o -> TextTagTable
toTextTagTable = unsafeCastGObject . toGObject

instance TextTagTableClass TextTagTable
instance GObjectClass TextTagTable where
  toGObject = GObject . castForeignPtr . unTextTagTable
  unsafeCastGObject = TextTagTable . castForeignPtr . unGObject

castToTextTagTable :: GObjectClass obj => obj -> TextTagTable
castToTextTagTable = castTo gTypeTextTagTable "TextTagTable"

gTypeTextTagTable :: GType
gTypeTextTagTable =
  {# call fun unsafe gtk_text_tag_table_get_type #}

-- ********************************************************************** Style

{#pointer *GtkStyle as Style foreign newtype #} deriving (Eq,Ord)

mkStyle = (Style, objectUnrefFromMainloop)
unStyle (Style o) = o

class GObjectClass o => StyleClass o
toStyle :: StyleClass o => o -> Style
toStyle = unsafeCastGObject . toGObject

instance StyleClass Style
instance GObjectClass Style where
  toGObject = GObject . castForeignPtr . unStyle
  unsafeCastGObject = Style . castForeignPtr . unGObject

castToStyle :: GObjectClass obj => obj -> Style
castToStyle = castTo gTypeStyle "Style"

gTypeStyle :: GType
gTypeStyle =
  {# call fun unsafe gtk_style_get_type #}

-- ******************************************************************** RcStyle

{#pointer *GtkRcStyle as RcStyle foreign newtype #} deriving (Eq,Ord)

mkRcStyle = (RcStyle, objectUnrefFromMainloop)
unRcStyle (RcStyle o) = o

class GObjectClass o => RcStyleClass o
toRcStyle :: RcStyleClass o => o -> RcStyle
toRcStyle = unsafeCastGObject . toGObject

instance RcStyleClass RcStyle
instance GObjectClass RcStyle where
  toGObject = GObject . castForeignPtr . unRcStyle
  unsafeCastGObject = RcStyle . castForeignPtr . unGObject

castToRcStyle :: GObjectClass obj => obj -> RcStyle
castToRcStyle = castTo gTypeRcStyle "RcStyle"

gTypeRcStyle :: GType
gTypeRcStyle =
  {# call fun unsafe gtk_rc_style_get_type #}

-- **************************************************************** DragContext

{#pointer *GdkDragContext as DragContext foreign newtype #} deriving (Eq,Ord)

mkDragContext = (DragContext, objectUnrefFromMainloop)
unDragContext (DragContext o) = o

class GObjectClass o => DragContextClass o
toDragContext :: DragContextClass o => o -> DragContext
toDragContext = unsafeCastGObject . toGObject

instance DragContextClass DragContext
instance GObjectClass DragContext where
  toGObject = GObject . castForeignPtr . unDragContext
  unsafeCastGObject = DragContext . castForeignPtr . unGObject

castToDragContext :: GObjectClass obj => obj -> DragContext
castToDragContext = castTo gTypeDragContext "DragContext"

gTypeDragContext :: GType
gTypeDragContext =
  {# call fun unsafe gdk_drag_context_get_type #}

-- ********************************************************************* Pixbuf

{#pointer *GdkPixbuf as Pixbuf foreign newtype #} deriving (Eq,Ord)

mkPixbuf = (Pixbuf, objectUnref)
unPixbuf (Pixbuf o) = o

class GObjectClass o => PixbufClass o
toPixbuf :: PixbufClass o => o -> Pixbuf
toPixbuf = unsafeCastGObject . toGObject

instance PixbufClass Pixbuf
instance GObjectClass Pixbuf where
  toGObject = GObject . castForeignPtr . unPixbuf
  unsafeCastGObject = Pixbuf . castForeignPtr . unGObject

castToPixbuf :: GObjectClass obj => obj -> Pixbuf
castToPixbuf = castTo gTypePixbuf "Pixbuf"

gTypePixbuf :: GType
gTypePixbuf =
  {# call fun unsafe gdk_pixbuf_get_type #}

-- ************************************************************ PixbufAnimation

{#pointer *GdkPixbufAnimation as PixbufAnimation foreign newtype #} deriving (Eq,Ord)

mkPixbufAnimation = (PixbufAnimation, objectUnref)
unPixbufAnimation (PixbufAnimation o) = o

class GObjectClass o => PixbufAnimationClass o
toPixbufAnimation :: PixbufAnimationClass o => o -> PixbufAnimation
toPixbufAnimation = unsafeCastGObject . toGObject

instance PixbufAnimationClass PixbufAnimation
instance GObjectClass PixbufAnimation where
  toGObject = GObject . castForeignPtr . unPixbufAnimation
  unsafeCastGObject = PixbufAnimation . castForeignPtr . unGObject

castToPixbufAnimation :: GObjectClass obj => obj -> PixbufAnimation
castToPixbufAnimation = castTo gTypePixbufAnimation "PixbufAnimation"

gTypePixbufAnimation :: GType
gTypePixbufAnimation =
  {# call fun unsafe gdk_pixbuf_animation_get_type #}

-- *********************************************************** PixbufSimpleAnim

{#pointer *GdkPixbufSimpleAnim as PixbufSimpleAnim foreign newtype #} deriving (Eq,Ord)

mkPixbufSimpleAnim = (PixbufSimpleAnim, objectUnref)
unPixbufSimpleAnim (PixbufSimpleAnim o) = o

class PixbufAnimationClass o => PixbufSimpleAnimClass o
toPixbufSimpleAnim :: PixbufSimpleAnimClass o => o -> PixbufSimpleAnim
toPixbufSimpleAnim = unsafeCastGObject . toGObject

instance PixbufSimpleAnimClass PixbufSimpleAnim
instance PixbufAnimationClass PixbufSimpleAnim
instance GObjectClass PixbufSimpleAnim where
  toGObject = GObject . castForeignPtr . unPixbufSimpleAnim
  unsafeCastGObject = PixbufSimpleAnim . castForeignPtr . unGObject

castToPixbufSimpleAnim :: GObjectClass obj => obj -> PixbufSimpleAnim
castToPixbufSimpleAnim = castTo gTypePixbufSimpleAnim "PixbufSimpleAnim"

gTypePixbufSimpleAnim :: GType
gTypePixbufSimpleAnim =
  {# call fun unsafe gdk_pixbuf_simple_anim_get_type #}

-- ******************************************************** PixbufAnimationIter

{#pointer *GdkPixbufAnimationIter as PixbufAnimationIter foreign newtype #} deriving (Eq,Ord)

mkPixbufAnimationIter = (PixbufAnimationIter, objectUnref)
unPixbufAnimationIter (PixbufAnimationIter o) = o

class GObjectClass o => PixbufAnimationIterClass o
toPixbufAnimationIter :: PixbufAnimationIterClass o => o -> PixbufAnimationIter
toPixbufAnimationIter = unsafeCastGObject . toGObject

instance PixbufAnimationIterClass PixbufAnimationIter
instance GObjectClass PixbufAnimationIter where
  toGObject = GObject . castForeignPtr . unPixbufAnimationIter
  unsafeCastGObject = PixbufAnimationIter . castForeignPtr . unGObject

castToPixbufAnimationIter :: GObjectClass obj => obj -> PixbufAnimationIter
castToPixbufAnimationIter = castTo gTypePixbufAnimationIter "PixbufAnimationIter"

gTypePixbufAnimationIter :: GType
gTypePixbufAnimationIter =
  {# call fun unsafe gdk_pixbuf_animation_iter_get_type #}

-- ************************************************************ TextChildAnchor

{#pointer *GtkTextChildAnchor as TextChildAnchor foreign newtype #} deriving (Eq,Ord)

mkTextChildAnchor = (TextChildAnchor, objectUnrefFromMainloop)
unTextChildAnchor (TextChildAnchor o) = o

class GObjectClass o => TextChildAnchorClass o
toTextChildAnchor :: TextChildAnchorClass o => o -> TextChildAnchor
toTextChildAnchor = unsafeCastGObject . toGObject

instance TextChildAnchorClass TextChildAnchor
instance GObjectClass TextChildAnchor where
  toGObject = GObject . castForeignPtr . unTextChildAnchor
  unsafeCastGObject = TextChildAnchor . castForeignPtr . unGObject

castToTextChildAnchor :: GObjectClass obj => obj -> TextChildAnchor
castToTextChildAnchor = castTo gTypeTextChildAnchor "TextChildAnchor"

gTypeTextChildAnchor :: GType
gTypeTextChildAnchor =
  {# call fun unsafe gtk_text_child_anchor_get_type #}

-- ******************************************************************* TextMark

{#pointer *GtkTextMark as TextMark foreign newtype #} deriving (Eq,Ord)

mkTextMark = (TextMark, objectUnrefFromMainloop)
unTextMark (TextMark o) = o

class GObjectClass o => TextMarkClass o
toTextMark :: TextMarkClass o => o -> TextMark
toTextMark = unsafeCastGObject . toGObject

instance TextMarkClass TextMark
instance GObjectClass TextMark where
  toGObject = GObject . castForeignPtr . unTextMark
  unsafeCastGObject = TextMark . castForeignPtr . unGObject

castToTextMark :: GObjectClass obj => obj -> TextMark
castToTextMark = castTo gTypeTextMark "TextMark"

gTypeTextMark :: GType
gTypeTextMark =
  {# call fun unsafe gtk_text_mark_get_type #}

-- *************************************************************** RecentFilter

{#pointer *GtkRecentFilter as RecentFilter foreign newtype #} deriving (Eq,Ord)

mkRecentFilter = (RecentFilter, objectUnrefFromMainloop)
unRecentFilter (RecentFilter o) = o

class GObjectClass o => RecentFilterClass o
toRecentFilter :: RecentFilterClass o => o -> RecentFilter
toRecentFilter = unsafeCastGObject . toGObject

instance RecentFilterClass RecentFilter
instance GObjectClass RecentFilter where
  toGObject = GObject . castForeignPtr . unRecentFilter
  unsafeCastGObject = RecentFilter . castForeignPtr . unGObject

castToRecentFilter :: GObjectClass obj => obj -> RecentFilter
castToRecentFilter = castTo gTypeRecentFilter "RecentFilter"

gTypeRecentFilter :: GType
gTypeRecentFilter =
  {# call fun unsafe gtk_recent_filter_get_type #}

-- ********************************************************************* Widget

{#pointer *GtkWidget as Widget foreign newtype #} deriving (Eq,Ord)

mkWidget = (Widget, objectUnrefFromMainloop)
unWidget (Widget o) = o

class GObjectClass o => WidgetClass o
toWidget :: WidgetClass o => o -> Widget
toWidget = unsafeCastGObject . toGObject

instance WidgetClass Widget
instance GObjectClass Widget where
  toGObject = GObject . castForeignPtr . unWidget
  unsafeCastGObject = Widget . castForeignPtr . unGObject

castToWidget :: GObjectClass obj => obj -> Widget
castToWidget = castTo gTypeWidget "Widget"

gTypeWidget :: GType
gTypeWidget =
  {# call fun unsafe gtk_widget_get_type #}

-- ************************************************************************ HSV

{#pointer *GtkHSV as HSV foreign newtype #} deriving (Eq,Ord)

mkHSV = (HSV, objectUnrefFromMainloop)
unHSV (HSV o) = o

class WidgetClass o => HSVClass o
toHSV :: HSVClass o => o -> HSV
toHSV = unsafeCastGObject . toGObject

instance HSVClass HSV
instance WidgetClass HSV
instance GObjectClass HSV where
  toGObject = GObject . castForeignPtr . unHSV
  unsafeCastGObject = HSV . castForeignPtr . unGObject

castToHSV :: GObjectClass obj => obj -> HSV
castToHSV = castTo gTypeHSV "HSV"

gTypeHSV :: GType
gTypeHSV =
  {# call fun unsafe gtk_hsv_get_type #}

-- *********************************************************************** Misc

{#pointer *GtkMisc as Misc foreign newtype #} deriving (Eq,Ord)

mkMisc = (Misc, objectUnrefFromMainloop)
unMisc (Misc o) = o

class WidgetClass o => MiscClass o
toMisc :: MiscClass o => o -> Misc
toMisc = unsafeCastGObject . toGObject

instance MiscClass Misc
instance WidgetClass Misc
instance GObjectClass Misc where
  toGObject = GObject . castForeignPtr . unMisc
  unsafeCastGObject = Misc . castForeignPtr . unGObject

castToMisc :: GObjectClass obj => obj -> Misc
castToMisc = castTo gTypeMisc "Misc"

gTypeMisc :: GType
gTypeMisc =
  {# call fun unsafe gtk_misc_get_type #}

-- ********************************************************************** Label

{#pointer *GtkLabel as Label foreign newtype #} deriving (Eq,Ord)

mkLabel = (Label, objectUnrefFromMainloop)
unLabel (Label o) = o

class MiscClass o => LabelClass o
toLabel :: LabelClass o => o -> Label
toLabel = unsafeCastGObject . toGObject

instance LabelClass Label
instance MiscClass Label
instance WidgetClass Label
instance GObjectClass Label where
  toGObject = GObject . castForeignPtr . unLabel
  unsafeCastGObject = Label . castForeignPtr . unGObject

castToLabel :: GObjectClass obj => obj -> Label
castToLabel = castTo gTypeLabel "Label"

gTypeLabel :: GType
gTypeLabel =
  {# call fun unsafe gtk_label_get_type #}

-- ***************************************************************** AccelLabel

{#pointer *GtkAccelLabel as AccelLabel foreign newtype #} deriving (Eq,Ord)

mkAccelLabel = (AccelLabel, objectUnrefFromMainloop)
unAccelLabel (AccelLabel o) = o

class LabelClass o => AccelLabelClass o
toAccelLabel :: AccelLabelClass o => o -> AccelLabel
toAccelLabel = unsafeCastGObject . toGObject

instance AccelLabelClass AccelLabel
instance LabelClass AccelLabel
instance MiscClass AccelLabel
instance WidgetClass AccelLabel
instance GObjectClass AccelLabel where
  toGObject = GObject . castForeignPtr . unAccelLabel
  unsafeCastGObject = AccelLabel . castForeignPtr . unGObject

castToAccelLabel :: GObjectClass obj => obj -> AccelLabel
castToAccelLabel = castTo gTypeAccelLabel "AccelLabel"

gTypeAccelLabel :: GType
gTypeAccelLabel =
  {# call fun unsafe gtk_accel_label_get_type #}

-- ********************************************************************** Arrow

{#pointer *GtkArrow as Arrow foreign newtype #} deriving (Eq,Ord)

mkArrow = (Arrow, objectUnrefFromMainloop)
unArrow (Arrow o) = o

class MiscClass o => ArrowClass o
toArrow :: ArrowClass o => o -> Arrow
toArrow = unsafeCastGObject . toGObject

instance ArrowClass Arrow
instance MiscClass Arrow
instance WidgetClass Arrow
instance GObjectClass Arrow where
  toGObject = GObject . castForeignPtr . unArrow
  unsafeCastGObject = Arrow . castForeignPtr . unGObject

castToArrow :: GObjectClass obj => obj -> Arrow
castToArrow = castTo gTypeArrow "Arrow"

gTypeArrow :: GType
gTypeArrow =
  {# call fun unsafe gtk_arrow_get_type #}

-- ********************************************************************** Image

{#pointer *GtkImage as Image foreign newtype #} deriving (Eq,Ord)

mkImage = (Image, objectUnrefFromMainloop)
unImage (Image o) = o

class MiscClass o => ImageClass o
toImage :: ImageClass o => o -> Image
toImage = unsafeCastGObject . toGObject

instance ImageClass Image
instance MiscClass Image
instance WidgetClass Image
instance GObjectClass Image where
  toGObject = GObject . castForeignPtr . unImage
  unsafeCastGObject = Image . castForeignPtr . unGObject

castToImage :: GObjectClass obj => obj -> Image
castToImage = castTo gTypeImage "Image"

gTypeImage :: GType
gTypeImage =
  {# call fun unsafe gtk_image_get_type #}

-- ********************************************************************* Switch

{#pointer *GtkSwitch as Switch foreign newtype #} deriving (Eq,Ord)

mkSwitch = (Switch, objectUnrefFromMainloop)
unSwitch (Switch o) = o

class WidgetClass o => SwitchClass o
toSwitch :: SwitchClass o => o -> Switch
toSwitch = unsafeCastGObject . toGObject

instance SwitchClass Switch
instance WidgetClass Switch
instance GObjectClass Switch where
  toGObject = GObject . castForeignPtr . unSwitch
  unsafeCastGObject = Switch . castForeignPtr . unGObject

castToSwitch :: GObjectClass obj => obj -> Switch
castToSwitch = castTo gTypeSwitch "Switch"

gTypeSwitch :: GType
gTypeSwitch =
  {# call fun unsafe gtk_switch_get_type #}

-- ****************************************************************** Container

{#pointer *GtkContainer as Container foreign newtype #} deriving (Eq,Ord)

mkContainer = (Container, objectUnrefFromMainloop)
unContainer (Container o) = o

class WidgetClass o => ContainerClass o
toContainer :: ContainerClass o => o -> Container
toContainer = unsafeCastGObject . toGObject

instance ContainerClass Container
instance WidgetClass Container
instance GObjectClass Container where
  toGObject = GObject . castForeignPtr . unContainer
  unsafeCastGObject = Container . castForeignPtr . unGObject

castToContainer :: GObjectClass obj => obj -> Container
castToContainer = castTo gTypeContainer "Container"

gTypeContainer :: GType
gTypeContainer =
  {# call fun unsafe gtk_container_get_type #}

-- **************************************************************** ToolPalette

{#pointer *GtkToolPalette as ToolPalette foreign newtype #} deriving (Eq,Ord)

mkToolPalette = (ToolPalette, objectUnrefFromMainloop)
unToolPalette (ToolPalette o) = o

class ContainerClass o => ToolPaletteClass o
toToolPalette :: ToolPaletteClass o => o -> ToolPalette
toToolPalette = unsafeCastGObject . toGObject

instance ToolPaletteClass ToolPalette
instance ContainerClass ToolPalette
instance WidgetClass ToolPalette
instance GObjectClass ToolPalette where
  toGObject = GObject . castForeignPtr . unToolPalette
  unsafeCastGObject = ToolPalette . castForeignPtr . unGObject

castToToolPalette :: GObjectClass obj => obj -> ToolPalette
castToToolPalette = castTo gTypeToolPalette "ToolPalette"

gTypeToolPalette :: GType
gTypeToolPalette =
  {# call fun unsafe gtk_tool_palette_get_type #}

-- ************************************************************** ToolItemGroup

{#pointer *GtkToolItemGroup as ToolItemGroup foreign newtype #} deriving (Eq,Ord)

mkToolItemGroup = (ToolItemGroup, objectUnrefFromMainloop)
unToolItemGroup (ToolItemGroup o) = o

class ContainerClass o => ToolItemGroupClass o
toToolItemGroup :: ToolItemGroupClass o => o -> ToolItemGroup
toToolItemGroup = unsafeCastGObject . toGObject

instance ToolItemGroupClass ToolItemGroup
instance ContainerClass ToolItemGroup
instance WidgetClass ToolItemGroup
instance GObjectClass ToolItemGroup where
  toGObject = GObject . castForeignPtr . unToolItemGroup
  unsafeCastGObject = ToolItemGroup . castForeignPtr . unGObject

castToToolItemGroup :: GObjectClass obj => obj -> ToolItemGroup
castToToolItemGroup = castTo gTypeToolItemGroup "ToolItemGroup"

gTypeToolItemGroup :: GType
gTypeToolItemGroup =
  {# call fun unsafe gtk_tool_item_group_get_type #}

-- ********************************************************************** Stack

{#pointer *GtkStack as Stack foreign newtype #} deriving (Eq,Ord)

mkStack = (Stack, objectUnrefFromMainloop)
unStack (Stack o) = o

class ContainerClass o => StackClass o
toStack :: StackClass o => o -> Stack
toStack = unsafeCastGObject . toGObject

instance StackClass Stack
instance ContainerClass Stack
instance WidgetClass Stack
instance GObjectClass Stack where
  toGObject = GObject . castForeignPtr . unStack
  unsafeCastGObject = Stack . castForeignPtr . unGObject

castToStack :: GObjectClass obj => obj -> Stack
castToStack = castTo gTypeStack "Stack"

gTypeStack :: GType
gTypeStack =
  {# call fun unsafe gtk_stack_get_type #}

-- ************************************************************************ Bin

{#pointer *GtkBin as Bin foreign newtype #} deriving (Eq,Ord)

mkBin = (Bin, objectUnrefFromMainloop)
unBin (Bin o) = o

class ContainerClass o => BinClass o
toBin :: BinClass o => o -> Bin
toBin = unsafeCastGObject . toGObject

instance BinClass Bin
instance ContainerClass Bin
instance WidgetClass Bin
instance GObjectClass Bin where
  toGObject = GObject . castForeignPtr . unBin
  unsafeCastGObject = Bin . castForeignPtr . unGObject

castToBin :: GObjectClass obj => obj -> Bin
castToBin = castTo gTypeBin "Bin"

gTypeBin :: GType
gTypeBin =
  {# call fun unsafe gtk_bin_get_type #}

-- ****************************************************************** Alignment

{#pointer *GtkAlignment as Alignment foreign newtype #} deriving (Eq,Ord)

mkAlignment = (Alignment, objectUnrefFromMainloop)
unAlignment (Alignment o) = o

class BinClass o => AlignmentClass o
toAlignment :: AlignmentClass o => o -> Alignment
toAlignment = unsafeCastGObject . toGObject

instance AlignmentClass Alignment
instance BinClass Alignment
instance ContainerClass Alignment
instance WidgetClass Alignment
instance GObjectClass Alignment where
  toGObject = GObject . castForeignPtr . unAlignment
  unsafeCastGObject = Alignment . castForeignPtr . unGObject

castToAlignment :: GObjectClass obj => obj -> Alignment
castToAlignment = castTo gTypeAlignment "Alignment"

gTypeAlignment :: GType
gTypeAlignment =
  {# call fun unsafe gtk_alignment_get_type #}

-- ********************************************************************** Frame

{#pointer *GtkFrame as Frame foreign newtype #} deriving (Eq,Ord)

mkFrame = (Frame, objectUnrefFromMainloop)
unFrame (Frame o) = o

class BinClass o => FrameClass o
toFrame :: FrameClass o => o -> Frame
toFrame = unsafeCastGObject . toGObject

instance FrameClass Frame
instance BinClass Frame
instance ContainerClass Frame
instance WidgetClass Frame
instance GObjectClass Frame where
  toGObject = GObject . castForeignPtr . unFrame
  unsafeCastGObject = Frame . castForeignPtr . unGObject

castToFrame :: GObjectClass obj => obj -> Frame
castToFrame = castTo gTypeFrame "Frame"

gTypeFrame :: GType
gTypeFrame =
  {# call fun unsafe gtk_frame_get_type #}

-- **************************************************************** AspectFrame

{#pointer *GtkAspectFrame as AspectFrame foreign newtype #} deriving (Eq,Ord)

mkAspectFrame = (AspectFrame, objectUnrefFromMainloop)
unAspectFrame (AspectFrame o) = o

class FrameClass o => AspectFrameClass o
toAspectFrame :: AspectFrameClass o => o -> AspectFrame
toAspectFrame = unsafeCastGObject . toGObject

instance AspectFrameClass AspectFrame
instance FrameClass AspectFrame
instance BinClass AspectFrame
instance ContainerClass AspectFrame
instance WidgetClass AspectFrame
instance GObjectClass AspectFrame where
  toGObject = GObject . castForeignPtr . unAspectFrame
  unsafeCastGObject = AspectFrame . castForeignPtr . unGObject

castToAspectFrame :: GObjectClass obj => obj -> AspectFrame
castToAspectFrame = castTo gTypeAspectFrame "AspectFrame"

gTypeAspectFrame :: GType
gTypeAspectFrame =
  {# call fun unsafe gtk_aspect_frame_get_type #}

-- ********************************************************************* Button

{#pointer *GtkButton as Button foreign newtype #} deriving (Eq,Ord)

mkButton = (Button, objectUnrefFromMainloop)
unButton (Button o) = o

class BinClass o => ButtonClass o
toButton :: ButtonClass o => o -> Button
toButton = unsafeCastGObject . toGObject

instance ButtonClass Button
instance BinClass Button
instance ContainerClass Button
instance WidgetClass Button
instance GObjectClass Button where
  toGObject = GObject . castForeignPtr . unButton
  unsafeCastGObject = Button . castForeignPtr . unGObject

castToButton :: GObjectClass obj => obj -> Button
castToButton = castTo gTypeButton "Button"

gTypeButton :: GType
gTypeButton =
  {# call fun unsafe gtk_button_get_type #}

-- **************************************************************** ScaleButton

{#pointer *GtkScaleButton as ScaleButton foreign newtype #} deriving (Eq,Ord)

mkScaleButton = (ScaleButton, objectUnrefFromMainloop)
unScaleButton (ScaleButton o) = o

class ButtonClass o => ScaleButtonClass o
toScaleButton :: ScaleButtonClass o => o -> ScaleButton
toScaleButton = unsafeCastGObject . toGObject

instance ScaleButtonClass ScaleButton
instance ButtonClass ScaleButton
instance BinClass ScaleButton
instance ContainerClass ScaleButton
instance WidgetClass ScaleButton
instance GObjectClass ScaleButton where
  toGObject = GObject . castForeignPtr . unScaleButton
  unsafeCastGObject = ScaleButton . castForeignPtr . unGObject

castToScaleButton :: GObjectClass obj => obj -> ScaleButton
castToScaleButton = castTo gTypeScaleButton "ScaleButton"

gTypeScaleButton :: GType
gTypeScaleButton =
  {# call fun unsafe gtk_scale_button_get_type #}

-- *************************************************************** VolumeButton

{#pointer *GtkVolumeButton as VolumeButton foreign newtype #} deriving (Eq,Ord)

mkVolumeButton = (VolumeButton, objectUnrefFromMainloop)
unVolumeButton (VolumeButton o) = o

class ScaleButtonClass o => VolumeButtonClass o
toVolumeButton :: VolumeButtonClass o => o -> VolumeButton
toVolumeButton = unsafeCastGObject . toGObject

instance VolumeButtonClass VolumeButton
instance ScaleButtonClass VolumeButton
instance ButtonClass VolumeButton
instance BinClass VolumeButton
instance ContainerClass VolumeButton
instance WidgetClass VolumeButton
instance GObjectClass VolumeButton where
  toGObject = GObject . castForeignPtr . unVolumeButton
  unsafeCastGObject = VolumeButton . castForeignPtr . unGObject

castToVolumeButton :: GObjectClass obj => obj -> VolumeButton
castToVolumeButton = castTo gTypeVolumeButton "VolumeButton"

gTypeVolumeButton :: GType
gTypeVolumeButton =
  {# call fun unsafe gtk_volume_button_get_type #}

-- ***************************************************************** LinkButton

{#pointer *GtkLinkButton as LinkButton foreign newtype #} deriving (Eq,Ord)

mkLinkButton = (LinkButton, objectUnrefFromMainloop)
unLinkButton (LinkButton o) = o

class ButtonClass o => LinkButtonClass o
toLinkButton :: LinkButtonClass o => o -> LinkButton
toLinkButton = unsafeCastGObject . toGObject

instance LinkButtonClass LinkButton
instance ButtonClass LinkButton
instance BinClass LinkButton
instance ContainerClass LinkButton
instance WidgetClass LinkButton
instance GObjectClass LinkButton where
  toGObject = GObject . castForeignPtr . unLinkButton
  unsafeCastGObject = LinkButton . castForeignPtr . unGObject

castToLinkButton :: GObjectClass obj => obj -> LinkButton
castToLinkButton = castTo gTypeLinkButton "LinkButton"

gTypeLinkButton :: GType
gTypeLinkButton =
  {# call fun unsafe gtk_link_button_get_type #}

-- *************************************************************** ToggleButton

{#pointer *GtkToggleButton as ToggleButton foreign newtype #} deriving (Eq,Ord)

mkToggleButton = (ToggleButton, objectUnrefFromMainloop)
unToggleButton (ToggleButton o) = o

class ButtonClass o => ToggleButtonClass o
toToggleButton :: ToggleButtonClass o => o -> ToggleButton
toToggleButton = unsafeCastGObject . toGObject

instance ToggleButtonClass ToggleButton
instance ButtonClass ToggleButton
instance BinClass ToggleButton
instance ContainerClass ToggleButton
instance WidgetClass ToggleButton
instance GObjectClass ToggleButton where
  toGObject = GObject . castForeignPtr . unToggleButton
  unsafeCastGObject = ToggleButton . castForeignPtr . unGObject

castToToggleButton :: GObjectClass obj => obj -> ToggleButton
castToToggleButton = castTo gTypeToggleButton "ToggleButton"

gTypeToggleButton :: GType
gTypeToggleButton =
  {# call fun unsafe gtk_toggle_button_get_type #}

-- **************************************************************** CheckButton

{#pointer *GtkCheckButton as CheckButton foreign newtype #} deriving (Eq,Ord)

mkCheckButton = (CheckButton, objectUnrefFromMainloop)
unCheckButton (CheckButton o) = o

class ToggleButtonClass o => CheckButtonClass o
toCheckButton :: CheckButtonClass o => o -> CheckButton
toCheckButton = unsafeCastGObject . toGObject

instance CheckButtonClass CheckButton
instance ToggleButtonClass CheckButton
instance ButtonClass CheckButton
instance BinClass CheckButton
instance ContainerClass CheckButton
instance WidgetClass CheckButton
instance GObjectClass CheckButton where
  toGObject = GObject . castForeignPtr . unCheckButton
  unsafeCastGObject = CheckButton . castForeignPtr . unGObject

castToCheckButton :: GObjectClass obj => obj -> CheckButton
castToCheckButton = castTo gTypeCheckButton "CheckButton"

gTypeCheckButton :: GType
gTypeCheckButton =
  {# call fun unsafe gtk_check_button_get_type #}

-- **************************************************************** RadioButton

{#pointer *GtkRadioButton as RadioButton foreign newtype #} deriving (Eq,Ord)

mkRadioButton = (RadioButton, objectUnrefFromMainloop)
unRadioButton (RadioButton o) = o

class CheckButtonClass o => RadioButtonClass o
toRadioButton :: RadioButtonClass o => o -> RadioButton
toRadioButton = unsafeCastGObject . toGObject

instance RadioButtonClass RadioButton
instance CheckButtonClass RadioButton
instance ToggleButtonClass RadioButton
instance ButtonClass RadioButton
instance BinClass RadioButton
instance ContainerClass RadioButton
instance WidgetClass RadioButton
instance GObjectClass RadioButton where
  toGObject = GObject . castForeignPtr . unRadioButton
  unsafeCastGObject = RadioButton . castForeignPtr . unGObject

castToRadioButton :: GObjectClass obj => obj -> RadioButton
castToRadioButton = castTo gTypeRadioButton "RadioButton"

gTypeRadioButton :: GType
gTypeRadioButton =
  {# call fun unsafe gtk_radio_button_get_type #}

-- **************************************************************** ColorButton

{#pointer *GtkColorButton as ColorButton foreign newtype #} deriving (Eq,Ord)

mkColorButton = (ColorButton, objectUnrefFromMainloop)
unColorButton (ColorButton o) = o

class ButtonClass o => ColorButtonClass o
toColorButton :: ColorButtonClass o => o -> ColorButton
toColorButton = unsafeCastGObject . toGObject

instance ColorButtonClass ColorButton
instance ButtonClass ColorButton
instance BinClass ColorButton
instance ContainerClass ColorButton
instance WidgetClass ColorButton
instance GObjectClass ColorButton where
  toGObject = GObject . castForeignPtr . unColorButton
  unsafeCastGObject = ColorButton . castForeignPtr . unGObject

castToColorButton :: GObjectClass obj => obj -> ColorButton
castToColorButton = castTo gTypeColorButton "ColorButton"

gTypeColorButton :: GType
gTypeColorButton =
  {# call fun unsafe gtk_color_button_get_type #}

-- ***************************************************************** FontButton

{#pointer *GtkFontButton as FontButton foreign newtype #} deriving (Eq,Ord)

mkFontButton = (FontButton, objectUnrefFromMainloop)
unFontButton (FontButton o) = o

class ButtonClass o => FontButtonClass o
toFontButton :: FontButtonClass o => o -> FontButton
toFontButton = unsafeCastGObject . toGObject

instance FontButtonClass FontButton
instance ButtonClass FontButton
instance BinClass FontButton
instance ContainerClass FontButton
instance WidgetClass FontButton
instance GObjectClass FontButton where
  toGObject = GObject . castForeignPtr . unFontButton
  unsafeCastGObject = FontButton . castForeignPtr . unGObject

castToFontButton :: GObjectClass obj => obj -> FontButton
castToFontButton = castTo gTypeFontButton "FontButton"

gTypeFontButton :: GType
gTypeFontButton =
  {# call fun unsafe gtk_font_button_get_type #}

-- ******************************************************************* MenuItem

{#pointer *GtkMenuItem as MenuItem foreign newtype #} deriving (Eq,Ord)

mkMenuItem = (MenuItem, objectUnrefFromMainloop)
unMenuItem (MenuItem o) = o

class BinClass o => MenuItemClass o
toMenuItem :: MenuItemClass o => o -> MenuItem
toMenuItem = unsafeCastGObject . toGObject

instance MenuItemClass MenuItem
instance BinClass MenuItem
instance ContainerClass MenuItem
instance WidgetClass MenuItem
instance GObjectClass MenuItem where
  toGObject = GObject . castForeignPtr . unMenuItem
  unsafeCastGObject = MenuItem . castForeignPtr . unGObject

castToMenuItem :: GObjectClass obj => obj -> MenuItem
castToMenuItem = castTo gTypeMenuItem "MenuItem"

gTypeMenuItem :: GType
gTypeMenuItem =
  {# call fun unsafe gtk_menu_item_get_type #}

-- ************************************************************** CheckMenuItem

{#pointer *GtkCheckMenuItem as CheckMenuItem foreign newtype #} deriving (Eq,Ord)

mkCheckMenuItem = (CheckMenuItem, objectUnrefFromMainloop)
unCheckMenuItem (CheckMenuItem o) = o

class MenuItemClass o => CheckMenuItemClass o
toCheckMenuItem :: CheckMenuItemClass o => o -> CheckMenuItem
toCheckMenuItem = unsafeCastGObject . toGObject

instance CheckMenuItemClass CheckMenuItem
instance MenuItemClass CheckMenuItem
instance BinClass CheckMenuItem
instance ContainerClass CheckMenuItem
instance WidgetClass CheckMenuItem
instance GObjectClass CheckMenuItem where
  toGObject = GObject . castForeignPtr . unCheckMenuItem
  unsafeCastGObject = CheckMenuItem . castForeignPtr . unGObject

castToCheckMenuItem :: GObjectClass obj => obj -> CheckMenuItem
castToCheckMenuItem = castTo gTypeCheckMenuItem "CheckMenuItem"

gTypeCheckMenuItem :: GType
gTypeCheckMenuItem =
  {# call fun unsafe gtk_check_menu_item_get_type #}

-- ************************************************************** RadioMenuItem

{#pointer *GtkRadioMenuItem as RadioMenuItem foreign newtype #} deriving (Eq,Ord)

mkRadioMenuItem = (RadioMenuItem, objectUnrefFromMainloop)
unRadioMenuItem (RadioMenuItem o) = o

class CheckMenuItemClass o => RadioMenuItemClass o
toRadioMenuItem :: RadioMenuItemClass o => o -> RadioMenuItem
toRadioMenuItem = unsafeCastGObject . toGObject

instance RadioMenuItemClass RadioMenuItem
instance CheckMenuItemClass RadioMenuItem
instance MenuItemClass RadioMenuItem
instance BinClass RadioMenuItem
instance ContainerClass RadioMenuItem
instance WidgetClass RadioMenuItem
instance GObjectClass RadioMenuItem where
  toGObject = GObject . castForeignPtr . unRadioMenuItem
  unsafeCastGObject = RadioMenuItem . castForeignPtr . unGObject

castToRadioMenuItem :: GObjectClass obj => obj -> RadioMenuItem
castToRadioMenuItem = castTo gTypeRadioMenuItem "RadioMenuItem"

gTypeRadioMenuItem :: GType
gTypeRadioMenuItem =
  {# call fun unsafe gtk_radio_menu_item_get_type #}

-- ************************************************************ TearoffMenuItem

{#pointer *GtkTearoffMenuItem as TearoffMenuItem foreign newtype #} deriving (Eq,Ord)

mkTearoffMenuItem = (TearoffMenuItem, objectUnrefFromMainloop)
unTearoffMenuItem (TearoffMenuItem o) = o

class MenuItemClass o => TearoffMenuItemClass o
toTearoffMenuItem :: TearoffMenuItemClass o => o -> TearoffMenuItem
toTearoffMenuItem = unsafeCastGObject . toGObject

instance TearoffMenuItemClass TearoffMenuItem
instance MenuItemClass TearoffMenuItem
instance BinClass TearoffMenuItem
instance ContainerClass TearoffMenuItem
instance WidgetClass TearoffMenuItem
instance GObjectClass TearoffMenuItem where
  toGObject = GObject . castForeignPtr . unTearoffMenuItem
  unsafeCastGObject = TearoffMenuItem . castForeignPtr . unGObject

castToTearoffMenuItem :: GObjectClass obj => obj -> TearoffMenuItem
castToTearoffMenuItem = castTo gTypeTearoffMenuItem "TearoffMenuItem"

gTypeTearoffMenuItem :: GType
gTypeTearoffMenuItem =
  {# call fun unsafe gtk_tearoff_menu_item_get_type #}

-- ************************************************************** ImageMenuItem

{#pointer *GtkImageMenuItem as ImageMenuItem foreign newtype #} deriving (Eq,Ord)

mkImageMenuItem = (ImageMenuItem, objectUnrefFromMainloop)
unImageMenuItem (ImageMenuItem o) = o

class MenuItemClass o => ImageMenuItemClass o
toImageMenuItem :: ImageMenuItemClass o => o -> ImageMenuItem
toImageMenuItem = unsafeCastGObject . toGObject

instance ImageMenuItemClass ImageMenuItem
instance MenuItemClass ImageMenuItem
instance BinClass ImageMenuItem
instance ContainerClass ImageMenuItem
instance WidgetClass ImageMenuItem
instance GObjectClass ImageMenuItem where
  toGObject = GObject . castForeignPtr . unImageMenuItem
  unsafeCastGObject = ImageMenuItem . castForeignPtr . unGObject

castToImageMenuItem :: GObjectClass obj => obj -> ImageMenuItem
castToImageMenuItem = castTo gTypeImageMenuItem "ImageMenuItem"

gTypeImageMenuItem :: GType
gTypeImageMenuItem =
  {# call fun unsafe gtk_image_menu_item_get_type #}

-- ********************************************************** SeparatorMenuItem

{#pointer *GtkSeparatorMenuItem as SeparatorMenuItem foreign newtype #} deriving (Eq,Ord)

mkSeparatorMenuItem = (SeparatorMenuItem, objectUnrefFromMainloop)
unSeparatorMenuItem (SeparatorMenuItem o) = o

class MenuItemClass o => SeparatorMenuItemClass o
toSeparatorMenuItem :: SeparatorMenuItemClass o => o -> SeparatorMenuItem
toSeparatorMenuItem = unsafeCastGObject . toGObject

instance SeparatorMenuItemClass SeparatorMenuItem
instance MenuItemClass SeparatorMenuItem
instance BinClass SeparatorMenuItem
instance ContainerClass SeparatorMenuItem
instance WidgetClass SeparatorMenuItem
instance GObjectClass SeparatorMenuItem where
  toGObject = GObject . castForeignPtr . unSeparatorMenuItem
  unsafeCastGObject = SeparatorMenuItem . castForeignPtr . unGObject

castToSeparatorMenuItem :: GObjectClass obj => obj -> SeparatorMenuItem
castToSeparatorMenuItem = castTo gTypeSeparatorMenuItem "SeparatorMenuItem"

gTypeSeparatorMenuItem :: GType
gTypeSeparatorMenuItem =
  {# call fun unsafe gtk_separator_menu_item_get_type #}

-- ******************************************************************** Overlay

{#pointer *GtkOverlay as Overlay foreign newtype #} deriving (Eq,Ord)

mkOverlay = (Overlay, objectUnrefFromMainloop)
unOverlay (Overlay o) = o

class BinClass o => OverlayClass o
toOverlay :: OverlayClass o => o -> Overlay
toOverlay = unsafeCastGObject . toGObject

instance OverlayClass Overlay
instance BinClass Overlay
instance ContainerClass Overlay
instance WidgetClass Overlay
instance GObjectClass Overlay where
  toGObject = GObject . castForeignPtr . unOverlay
  unsafeCastGObject = Overlay . castForeignPtr . unGObject

castToOverlay :: GObjectClass obj => obj -> Overlay
castToOverlay = castTo gTypeOverlay "Overlay"

gTypeOverlay :: GType
gTypeOverlay =
  {# call fun unsafe gtk_overlay_get_type #}

-- ********************************************************************* Window

{#pointer *GtkWindow as Window foreign newtype #} deriving (Eq,Ord)

mkWindow = (Window, objectUnrefFromMainloop)
unWindow (Window o) = o

class BinClass o => WindowClass o
toWindow :: WindowClass o => o -> Window
toWindow = unsafeCastGObject . toGObject

instance WindowClass Window
instance BinClass Window
instance ContainerClass Window
instance WidgetClass Window
instance GObjectClass Window where
  toGObject = GObject . castForeignPtr . unWindow
  unsafeCastGObject = Window . castForeignPtr . unGObject

castToWindow :: GObjectClass obj => obj -> Window
castToWindow = castTo gTypeWindow "Window"

gTypeWindow :: GType
gTypeWindow =
  {# call fun unsafe gtk_window_get_type #}

-- ****************************************************************** Assistant

{#pointer *GtkAssistant as Assistant foreign newtype #} deriving (Eq,Ord)

mkAssistant = (Assistant, objectUnrefFromMainloop)
unAssistant (Assistant o) = o

class WindowClass o => AssistantClass o
toAssistant :: AssistantClass o => o -> Assistant
toAssistant = unsafeCastGObject . toGObject

instance AssistantClass Assistant
instance WindowClass Assistant
instance BinClass Assistant
instance ContainerClass Assistant
instance WidgetClass Assistant
instance GObjectClass Assistant where
  toGObject = GObject . castForeignPtr . unAssistant
  unsafeCastGObject = Assistant . castForeignPtr . unGObject

castToAssistant :: GObjectClass obj => obj -> Assistant
castToAssistant = castTo gTypeAssistant "Assistant"

gTypeAssistant :: GType
gTypeAssistant =
  {# call fun unsafe gtk_assistant_get_type #}

-- ************************************************************ OffscreenWindow

{#pointer *GtkOffscreenWindow as OffscreenWindow foreign newtype #} deriving (Eq,Ord)

mkOffscreenWindow = (OffscreenWindow, objectUnrefFromMainloop)
unOffscreenWindow (OffscreenWindow o) = o

class WindowClass o => OffscreenWindowClass o
toOffscreenWindow :: OffscreenWindowClass o => o -> OffscreenWindow
toOffscreenWindow = unsafeCastGObject . toGObject

instance OffscreenWindowClass OffscreenWindow
instance WindowClass OffscreenWindow
instance BinClass OffscreenWindow
instance ContainerClass OffscreenWindow
instance WidgetClass OffscreenWindow
instance GObjectClass OffscreenWindow where
  toGObject = GObject . castForeignPtr . unOffscreenWindow
  unsafeCastGObject = OffscreenWindow . castForeignPtr . unGObject

castToOffscreenWindow :: GObjectClass obj => obj -> OffscreenWindow
castToOffscreenWindow = castTo gTypeOffscreenWindow "OffscreenWindow"

gTypeOffscreenWindow :: GType
gTypeOffscreenWindow =
  {# call fun unsafe gtk_offscreen_window_get_type #}

-- ********************************************************************* Dialog

{#pointer *GtkDialog as Dialog foreign newtype #} deriving (Eq,Ord)

mkDialog = (Dialog, objectUnrefFromMainloop)
unDialog (Dialog o) = o

class WindowClass o => DialogClass o
toDialog :: DialogClass o => o -> Dialog
toDialog = unsafeCastGObject . toGObject

instance DialogClass Dialog
instance WindowClass Dialog
instance BinClass Dialog
instance ContainerClass Dialog
instance WidgetClass Dialog
instance GObjectClass Dialog where
  toGObject = GObject . castForeignPtr . unDialog
  unsafeCastGObject = Dialog . castForeignPtr . unGObject

castToDialog :: GObjectClass obj => obj -> Dialog
castToDialog = castTo gTypeDialog "Dialog"

gTypeDialog :: GType
gTypeDialog =
  {# call fun unsafe gtk_dialog_get_type #}

-- **************************************************************** AboutDialog

{#pointer *GtkAboutDialog as AboutDialog foreign newtype #} deriving (Eq,Ord)

mkAboutDialog = (AboutDialog, objectUnrefFromMainloop)
unAboutDialog (AboutDialog o) = o

class DialogClass o => AboutDialogClass o
toAboutDialog :: AboutDialogClass o => o -> AboutDialog
toAboutDialog = unsafeCastGObject . toGObject

instance AboutDialogClass AboutDialog
instance DialogClass AboutDialog
instance WindowClass AboutDialog
instance BinClass AboutDialog
instance ContainerClass AboutDialog
instance WidgetClass AboutDialog
instance GObjectClass AboutDialog where
  toGObject = GObject . castForeignPtr . unAboutDialog
  unsafeCastGObject = AboutDialog . castForeignPtr . unGObject

castToAboutDialog :: GObjectClass obj => obj -> AboutDialog
castToAboutDialog = castTo gTypeAboutDialog "AboutDialog"

gTypeAboutDialog :: GType
gTypeAboutDialog =
  {# call fun unsafe gtk_about_dialog_get_type #}

-- ******************************************************* ColorSelectionDialog

{#pointer *GtkColorSelectionDialog as ColorSelectionDialog foreign newtype #} deriving (Eq,Ord)

mkColorSelectionDialog = (ColorSelectionDialog, objectUnrefFromMainloop)
unColorSelectionDialog (ColorSelectionDialog o) = o

class DialogClass o => ColorSelectionDialogClass o
toColorSelectionDialog :: ColorSelectionDialogClass o => o -> ColorSelectionDialog
toColorSelectionDialog = unsafeCastGObject . toGObject

instance ColorSelectionDialogClass ColorSelectionDialog
instance DialogClass ColorSelectionDialog
instance WindowClass ColorSelectionDialog
instance BinClass ColorSelectionDialog
instance ContainerClass ColorSelectionDialog
instance WidgetClass ColorSelectionDialog
instance GObjectClass ColorSelectionDialog where
  toGObject = GObject . castForeignPtr . unColorSelectionDialog
  unsafeCastGObject = ColorSelectionDialog . castForeignPtr . unGObject

castToColorSelectionDialog :: GObjectClass obj => obj -> ColorSelectionDialog
castToColorSelectionDialog = castTo gTypeColorSelectionDialog "ColorSelectionDialog"

gTypeColorSelectionDialog :: GType
gTypeColorSelectionDialog =
  {# call fun unsafe gtk_color_selection_dialog_get_type #}

-- ********************************************************** FileChooserDialog

{#pointer *GtkFileChooserDialog as FileChooserDialog foreign newtype #} deriving (Eq,Ord)

mkFileChooserDialog = (FileChooserDialog, objectUnrefFromMainloop)
unFileChooserDialog (FileChooserDialog o) = o

class DialogClass o => FileChooserDialogClass o
toFileChooserDialog :: FileChooserDialogClass o => o -> FileChooserDialog
toFileChooserDialog = unsafeCastGObject . toGObject

instance FileChooserDialogClass FileChooserDialog
instance DialogClass FileChooserDialog
instance WindowClass FileChooserDialog
instance BinClass FileChooserDialog
instance ContainerClass FileChooserDialog
instance WidgetClass FileChooserDialog
instance GObjectClass FileChooserDialog where
  toGObject = GObject . castForeignPtr . unFileChooserDialog
  unsafeCastGObject = FileChooserDialog . castForeignPtr . unGObject

castToFileChooserDialog :: GObjectClass obj => obj -> FileChooserDialog
castToFileChooserDialog = castTo gTypeFileChooserDialog "FileChooserDialog"

gTypeFileChooserDialog :: GType
gTypeFileChooserDialog =
  {# call fun unsafe gtk_file_chooser_dialog_get_type #}

-- ******************************************************** FontSelectionDialog

{#pointer *GtkFontSelectionDialog as FontSelectionDialog foreign newtype #} deriving (Eq,Ord)

mkFontSelectionDialog = (FontSelectionDialog, objectUnrefFromMainloop)
unFontSelectionDialog (FontSelectionDialog o) = o

class DialogClass o => FontSelectionDialogClass o
toFontSelectionDialog :: FontSelectionDialogClass o => o -> FontSelectionDialog
toFontSelectionDialog = unsafeCastGObject . toGObject

instance FontSelectionDialogClass FontSelectionDialog
instance DialogClass FontSelectionDialog
instance WindowClass FontSelectionDialog
instance BinClass FontSelectionDialog
instance ContainerClass FontSelectionDialog
instance WidgetClass FontSelectionDialog
instance GObjectClass FontSelectionDialog where
  toGObject = GObject . castForeignPtr . unFontSelectionDialog
  unsafeCastGObject = FontSelectionDialog . castForeignPtr . unGObject

castToFontSelectionDialog :: GObjectClass obj => obj -> FontSelectionDialog
castToFontSelectionDialog = castTo gTypeFontSelectionDialog "FontSelectionDialog"

gTypeFontSelectionDialog :: GType
gTypeFontSelectionDialog =
  {# call fun unsafe gtk_font_selection_dialog_get_type #}

-- ************************************************************** MessageDialog

{#pointer *GtkMessageDialog as MessageDialog foreign newtype #} deriving (Eq,Ord)

mkMessageDialog = (MessageDialog, objectUnrefFromMainloop)
unMessageDialog (MessageDialog o) = o

class DialogClass o => MessageDialogClass o
toMessageDialog :: MessageDialogClass o => o -> MessageDialog
toMessageDialog = unsafeCastGObject . toGObject

instance MessageDialogClass MessageDialog
instance DialogClass MessageDialog
instance WindowClass MessageDialog
instance BinClass MessageDialog
instance ContainerClass MessageDialog
instance WidgetClass MessageDialog
instance GObjectClass MessageDialog where
  toGObject = GObject . castForeignPtr . unMessageDialog
  unsafeCastGObject = MessageDialog . castForeignPtr . unGObject

castToMessageDialog :: GObjectClass obj => obj -> MessageDialog
castToMessageDialog = castTo gTypeMessageDialog "MessageDialog"

gTypeMessageDialog :: GType
gTypeMessageDialog =
  {# call fun unsafe gtk_message_dialog_get_type #}

-- ******************************************************************* EventBox

{#pointer *GtkEventBox as EventBox foreign newtype #} deriving (Eq,Ord)

mkEventBox = (EventBox, objectUnrefFromMainloop)
unEventBox (EventBox o) = o

class BinClass o => EventBoxClass o
toEventBox :: EventBoxClass o => o -> EventBox
toEventBox = unsafeCastGObject . toGObject

instance EventBoxClass EventBox
instance BinClass EventBox
instance ContainerClass EventBox
instance WidgetClass EventBox
instance GObjectClass EventBox where
  toGObject = GObject . castForeignPtr . unEventBox
  unsafeCastGObject = EventBox . castForeignPtr . unGObject

castToEventBox :: GObjectClass obj => obj -> EventBox
castToEventBox = castTo gTypeEventBox "EventBox"

gTypeEventBox :: GType
gTypeEventBox =
  {# call fun unsafe gtk_event_box_get_type #}

-- ****************************************************************** HandleBox

{#pointer *GtkHandleBox as HandleBox foreign newtype #} deriving (Eq,Ord)

mkHandleBox = (HandleBox, objectUnrefFromMainloop)
unHandleBox (HandleBox o) = o

class BinClass o => HandleBoxClass o
toHandleBox :: HandleBoxClass o => o -> HandleBox
toHandleBox = unsafeCastGObject . toGObject

instance HandleBoxClass HandleBox
instance BinClass HandleBox
instance ContainerClass HandleBox
instance WidgetClass HandleBox
instance GObjectClass HandleBox where
  toGObject = GObject . castForeignPtr . unHandleBox
  unsafeCastGObject = HandleBox . castForeignPtr . unGObject

castToHandleBox :: GObjectClass obj => obj -> HandleBox
castToHandleBox = castTo gTypeHandleBox "HandleBox"

gTypeHandleBox :: GType
gTypeHandleBox =
  {# call fun unsafe gtk_handle_box_get_type #}

-- ************************************************************* ScrolledWindow

{#pointer *GtkScrolledWindow as ScrolledWindow foreign newtype #} deriving (Eq,Ord)

mkScrolledWindow = (ScrolledWindow, objectUnrefFromMainloop)
unScrolledWindow (ScrolledWindow o) = o

class BinClass o => ScrolledWindowClass o
toScrolledWindow :: ScrolledWindowClass o => o -> ScrolledWindow
toScrolledWindow = unsafeCastGObject . toGObject

instance ScrolledWindowClass ScrolledWindow
instance BinClass ScrolledWindow
instance ContainerClass ScrolledWindow
instance WidgetClass ScrolledWindow
instance GObjectClass ScrolledWindow where
  toGObject = GObject . castForeignPtr . unScrolledWindow
  unsafeCastGObject = ScrolledWindow . castForeignPtr . unGObject

castToScrolledWindow :: GObjectClass obj => obj -> ScrolledWindow
castToScrolledWindow = castTo gTypeScrolledWindow "ScrolledWindow"

gTypeScrolledWindow :: GType
gTypeScrolledWindow =
  {# call fun unsafe gtk_scrolled_window_get_type #}

-- ******************************************************************* Viewport

{#pointer *GtkViewport as Viewport foreign newtype #} deriving (Eq,Ord)

mkViewport = (Viewport, objectUnrefFromMainloop)
unViewport (Viewport o) = o

class BinClass o => ViewportClass o
toViewport :: ViewportClass o => o -> Viewport
toViewport = unsafeCastGObject . toGObject

instance ViewportClass Viewport
instance BinClass Viewport
instance ContainerClass Viewport
instance WidgetClass Viewport
instance GObjectClass Viewport where
  toGObject = GObject . castForeignPtr . unViewport
  unsafeCastGObject = Viewport . castForeignPtr . unGObject

castToViewport :: GObjectClass obj => obj -> Viewport
castToViewport = castTo gTypeViewport "Viewport"

gTypeViewport :: GType
gTypeViewport =
  {# call fun unsafe gtk_viewport_get_type #}

-- ******************************************************************* Expander

{#pointer *GtkExpander as Expander foreign newtype #} deriving (Eq,Ord)

mkExpander = (Expander, objectUnrefFromMainloop)
unExpander (Expander o) = o

class BinClass o => ExpanderClass o
toExpander :: ExpanderClass o => o -> Expander
toExpander = unsafeCastGObject . toGObject

instance ExpanderClass Expander
instance BinClass Expander
instance ContainerClass Expander
instance WidgetClass Expander
instance GObjectClass Expander where
  toGObject = GObject . castForeignPtr . unExpander
  unsafeCastGObject = Expander . castForeignPtr . unGObject

castToExpander :: GObjectClass obj => obj -> Expander
castToExpander = castTo gTypeExpander "Expander"

gTypeExpander :: GType
gTypeExpander =
  {# call fun unsafe gtk_expander_get_type #}

-- ******************************************************************* ComboBox

{#pointer *GtkComboBox as ComboBox foreign newtype #} deriving (Eq,Ord)

mkComboBox = (ComboBox, objectUnrefFromMainloop)
unComboBox (ComboBox o) = o

class BinClass o => ComboBoxClass o
toComboBox :: ComboBoxClass o => o -> ComboBox
toComboBox = unsafeCastGObject . toGObject

instance ComboBoxClass ComboBox
instance BinClass ComboBox
instance ContainerClass ComboBox
instance WidgetClass ComboBox
instance GObjectClass ComboBox where
  toGObject = GObject . castForeignPtr . unComboBox
  unsafeCastGObject = ComboBox . castForeignPtr . unGObject

castToComboBox :: GObjectClass obj => obj -> ComboBox
castToComboBox = castTo gTypeComboBox "ComboBox"

gTypeComboBox :: GType
gTypeComboBox =
  {# call fun unsafe gtk_combo_box_get_type #}

-- ******************************************************************* ToolItem

{#pointer *GtkToolItem as ToolItem foreign newtype #} deriving (Eq,Ord)

mkToolItem = (ToolItem, objectUnrefFromMainloop)
unToolItem (ToolItem o) = o

class BinClass o => ToolItemClass o
toToolItem :: ToolItemClass o => o -> ToolItem
toToolItem = unsafeCastGObject . toGObject

instance ToolItemClass ToolItem
instance BinClass ToolItem
instance ContainerClass ToolItem
instance WidgetClass ToolItem
instance GObjectClass ToolItem where
  toGObject = GObject . castForeignPtr . unToolItem
  unsafeCastGObject = ToolItem . castForeignPtr . unGObject

castToToolItem :: GObjectClass obj => obj -> ToolItem
castToToolItem = castTo gTypeToolItem "ToolItem"

gTypeToolItem :: GType
gTypeToolItem =
  {# call fun unsafe gtk_tool_item_get_type #}

-- ***************************************************************** ToolButton

{#pointer *GtkToolButton as ToolButton foreign newtype #} deriving (Eq,Ord)

mkToolButton = (ToolButton, objectUnrefFromMainloop)
unToolButton (ToolButton o) = o

class ToolItemClass o => ToolButtonClass o
toToolButton :: ToolButtonClass o => o -> ToolButton
toToolButton = unsafeCastGObject . toGObject

instance ToolButtonClass ToolButton
instance ToolItemClass ToolButton
instance BinClass ToolButton
instance ContainerClass ToolButton
instance WidgetClass ToolButton
instance GObjectClass ToolButton where
  toGObject = GObject . castForeignPtr . unToolButton
  unsafeCastGObject = ToolButton . castForeignPtr . unGObject

castToToolButton :: GObjectClass obj => obj -> ToolButton
castToToolButton = castTo gTypeToolButton "ToolButton"

gTypeToolButton :: GType
gTypeToolButton =
  {# call fun unsafe gtk_tool_button_get_type #}

-- ************************************************************* MenuToolButton

{#pointer *GtkMenuToolButton as MenuToolButton foreign newtype #} deriving (Eq,Ord)

mkMenuToolButton = (MenuToolButton, objectUnrefFromMainloop)
unMenuToolButton (MenuToolButton o) = o

class ToolButtonClass o => MenuToolButtonClass o
toMenuToolButton :: MenuToolButtonClass o => o -> MenuToolButton
toMenuToolButton = unsafeCastGObject . toGObject

instance MenuToolButtonClass MenuToolButton
instance ToolButtonClass MenuToolButton
instance ToolItemClass MenuToolButton
instance BinClass MenuToolButton
instance ContainerClass MenuToolButton
instance WidgetClass MenuToolButton
instance GObjectClass MenuToolButton where
  toGObject = GObject . castForeignPtr . unMenuToolButton
  unsafeCastGObject = MenuToolButton . castForeignPtr . unGObject

castToMenuToolButton :: GObjectClass obj => obj -> MenuToolButton
castToMenuToolButton = castTo gTypeMenuToolButton "MenuToolButton"

gTypeMenuToolButton :: GType
gTypeMenuToolButton =
  {# call fun unsafe gtk_menu_tool_button_get_type #}

-- *********************************************************** ToggleToolButton

{#pointer *GtkToggleToolButton as ToggleToolButton foreign newtype #} deriving (Eq,Ord)

mkToggleToolButton = (ToggleToolButton, objectUnrefFromMainloop)
unToggleToolButton (ToggleToolButton o) = o

class ToolButtonClass o => ToggleToolButtonClass o
toToggleToolButton :: ToggleToolButtonClass o => o -> ToggleToolButton
toToggleToolButton = unsafeCastGObject . toGObject

instance ToggleToolButtonClass ToggleToolButton
instance ToolButtonClass ToggleToolButton
instance ToolItemClass ToggleToolButton
instance BinClass ToggleToolButton
instance ContainerClass ToggleToolButton
instance WidgetClass ToggleToolButton
instance GObjectClass ToggleToolButton where
  toGObject = GObject . castForeignPtr . unToggleToolButton
  unsafeCastGObject = ToggleToolButton . castForeignPtr . unGObject

castToToggleToolButton :: GObjectClass obj => obj -> ToggleToolButton
castToToggleToolButton = castTo gTypeToggleToolButton "ToggleToolButton"

gTypeToggleToolButton :: GType
gTypeToggleToolButton =
  {# call fun unsafe gtk_toggle_tool_button_get_type #}

-- ************************************************************ RadioToolButton

{#pointer *GtkRadioToolButton as RadioToolButton foreign newtype #} deriving (Eq,Ord)

mkRadioToolButton = (RadioToolButton, objectUnrefFromMainloop)
unRadioToolButton (RadioToolButton o) = o

class ToggleToolButtonClass o => RadioToolButtonClass o
toRadioToolButton :: RadioToolButtonClass o => o -> RadioToolButton
toRadioToolButton = unsafeCastGObject . toGObject

instance RadioToolButtonClass RadioToolButton
instance ToggleToolButtonClass RadioToolButton
instance ToolButtonClass RadioToolButton
instance ToolItemClass RadioToolButton
instance BinClass RadioToolButton
instance ContainerClass RadioToolButton
instance WidgetClass RadioToolButton
instance GObjectClass RadioToolButton where
  toGObject = GObject . castForeignPtr . unRadioToolButton
  unsafeCastGObject = RadioToolButton . castForeignPtr . unGObject

castToRadioToolButton :: GObjectClass obj => obj -> RadioToolButton
castToRadioToolButton = castTo gTypeRadioToolButton "RadioToolButton"

gTypeRadioToolButton :: GType
gTypeRadioToolButton =
  {# call fun unsafe gtk_radio_tool_button_get_type #}

-- ********************************************************** SeparatorToolItem

{#pointer *GtkSeparatorToolItem as SeparatorToolItem foreign newtype #} deriving (Eq,Ord)

mkSeparatorToolItem = (SeparatorToolItem, objectUnrefFromMainloop)
unSeparatorToolItem (SeparatorToolItem o) = o

class ToolItemClass o => SeparatorToolItemClass o
toSeparatorToolItem :: SeparatorToolItemClass o => o -> SeparatorToolItem
toSeparatorToolItem = unsafeCastGObject . toGObject

instance SeparatorToolItemClass SeparatorToolItem
instance ToolItemClass SeparatorToolItem
instance BinClass SeparatorToolItem
instance ContainerClass SeparatorToolItem
instance WidgetClass SeparatorToolItem
instance GObjectClass SeparatorToolItem where
  toGObject = GObject . castForeignPtr . unSeparatorToolItem
  unsafeCastGObject = SeparatorToolItem . castForeignPtr . unGObject

castToSeparatorToolItem :: GObjectClass obj => obj -> SeparatorToolItem
castToSeparatorToolItem = castTo gTypeSeparatorToolItem "SeparatorToolItem"

gTypeSeparatorToolItem :: GType
gTypeSeparatorToolItem =
  {# call fun unsafe gtk_separator_tool_item_get_type #}

-- ************************************************************** StackSwitcher

{#pointer *GtkStackSwitcher as StackSwitcher foreign newtype #} deriving (Eq,Ord)

mkStackSwitcher = (StackSwitcher, objectUnrefFromMainloop)
unStackSwitcher (StackSwitcher o) = o

class BinClass o => StackSwitcherClass o
toStackSwitcher :: StackSwitcherClass o => o -> StackSwitcher
toStackSwitcher = unsafeCastGObject . toGObject

instance StackSwitcherClass StackSwitcher
instance BinClass StackSwitcher
instance ContainerClass StackSwitcher
instance WidgetClass StackSwitcher
instance GObjectClass StackSwitcher where
  toGObject = GObject . castForeignPtr . unStackSwitcher
  unsafeCastGObject = StackSwitcher . castForeignPtr . unGObject

castToStackSwitcher :: GObjectClass obj => obj -> StackSwitcher
castToStackSwitcher = castTo gTypeStackSwitcher "StackSwitcher"

gTypeStackSwitcher :: GType
gTypeStackSwitcher =
  {# call fun unsafe gtk_stack_switcher_get_type #}

-- ************************************************************************ Box

{#pointer *GtkBox as Box foreign newtype #} deriving (Eq,Ord)

mkBox = (Box, objectUnrefFromMainloop)
unBox (Box o) = o

class ContainerClass o => BoxClass o
toBox :: BoxClass o => o -> Box
toBox = unsafeCastGObject . toGObject

instance BoxClass Box
instance ContainerClass Box
instance WidgetClass Box
instance GObjectClass Box where
  toGObject = GObject . castForeignPtr . unBox
  unsafeCastGObject = Box . castForeignPtr . unGObject

castToBox :: GObjectClass obj => obj -> Box
castToBox = castTo gTypeBox "Box"

gTypeBox :: GType
gTypeBox =
  {# call fun unsafe gtk_box_get_type #}

-- ****************************************************************** ButtonBox

{#pointer *GtkButtonBox as ButtonBox foreign newtype #} deriving (Eq,Ord)

mkButtonBox = (ButtonBox, objectUnrefFromMainloop)
unButtonBox (ButtonBox o) = o

class BoxClass o => ButtonBoxClass o
toButtonBox :: ButtonBoxClass o => o -> ButtonBox
toButtonBox = unsafeCastGObject . toGObject

instance ButtonBoxClass ButtonBox
instance BoxClass ButtonBox
instance ContainerClass ButtonBox
instance WidgetClass ButtonBox
instance GObjectClass ButtonBox where
  toGObject = GObject . castForeignPtr . unButtonBox
  unsafeCastGObject = ButtonBox . castForeignPtr . unGObject

castToButtonBox :: GObjectClass obj => obj -> ButtonBox
castToButtonBox = castTo gTypeButtonBox "ButtonBox"

gTypeButtonBox :: GType
gTypeButtonBox =
  {# call fun unsafe gtk_button_box_get_type #}

-- ***************************************************************** HButtonBox

{#pointer *GtkHButtonBox as HButtonBox foreign newtype #} deriving (Eq,Ord)

mkHButtonBox = (HButtonBox, objectUnrefFromMainloop)
unHButtonBox (HButtonBox o) = o

class ButtonBoxClass o => HButtonBoxClass o
toHButtonBox :: HButtonBoxClass o => o -> HButtonBox
toHButtonBox = unsafeCastGObject . toGObject

instance HButtonBoxClass HButtonBox
instance ButtonBoxClass HButtonBox
instance BoxClass HButtonBox
instance ContainerClass HButtonBox
instance WidgetClass HButtonBox
instance GObjectClass HButtonBox where
  toGObject = GObject . castForeignPtr . unHButtonBox
  unsafeCastGObject = HButtonBox . castForeignPtr . unGObject

castToHButtonBox :: GObjectClass obj => obj -> HButtonBox
castToHButtonBox = castTo gTypeHButtonBox "HButtonBox"

gTypeHButtonBox :: GType
gTypeHButtonBox =
  {# call fun unsafe gtk_hbutton_box_get_type #}

-- ***************************************************************** VButtonBox

{#pointer *GtkVButtonBox as VButtonBox foreign newtype #} deriving (Eq,Ord)

mkVButtonBox = (VButtonBox, objectUnrefFromMainloop)
unVButtonBox (VButtonBox o) = o

class ButtonBoxClass o => VButtonBoxClass o
toVButtonBox :: VButtonBoxClass o => o -> VButtonBox
toVButtonBox = unsafeCastGObject . toGObject

instance VButtonBoxClass VButtonBox
instance ButtonBoxClass VButtonBox
instance BoxClass VButtonBox
instance ContainerClass VButtonBox
instance WidgetClass VButtonBox
instance GObjectClass VButtonBox where
  toGObject = GObject . castForeignPtr . unVButtonBox
  unsafeCastGObject = VButtonBox . castForeignPtr . unGObject

castToVButtonBox :: GObjectClass obj => obj -> VButtonBox
castToVButtonBox = castTo gTypeVButtonBox "VButtonBox"

gTypeVButtonBox :: GType
gTypeVButtonBox =
  {# call fun unsafe gtk_vbutton_box_get_type #}

-- *********************************************************************** VBox

{#pointer *GtkVBox as VBox foreign newtype #} deriving (Eq,Ord)

mkVBox = (VBox, objectUnrefFromMainloop)
unVBox (VBox o) = o

class BoxClass o => VBoxClass o
toVBox :: VBoxClass o => o -> VBox
toVBox = unsafeCastGObject . toGObject

instance VBoxClass VBox
instance BoxClass VBox
instance ContainerClass VBox
instance WidgetClass VBox
instance GObjectClass VBox where
  toGObject = GObject . castForeignPtr . unVBox
  unsafeCastGObject = VBox . castForeignPtr . unGObject

castToVBox :: GObjectClass obj => obj -> VBox
castToVBox = castTo gTypeVBox "VBox"

gTypeVBox :: GType
gTypeVBox =
  {# call fun unsafe gtk_vbox_get_type #}

-- ******************************************************** RecentChooserWidget

{#pointer *GtkRecentChooserWidget as RecentChooserWidget foreign newtype #} deriving (Eq,Ord)

mkRecentChooserWidget = (RecentChooserWidget, objectUnrefFromMainloop)
unRecentChooserWidget (RecentChooserWidget o) = o

class VBoxClass o => RecentChooserWidgetClass o
toRecentChooserWidget :: RecentChooserWidgetClass o => o -> RecentChooserWidget
toRecentChooserWidget = unsafeCastGObject . toGObject

instance RecentChooserWidgetClass RecentChooserWidget
instance VBoxClass RecentChooserWidget
instance BoxClass RecentChooserWidget
instance ContainerClass RecentChooserWidget
instance WidgetClass RecentChooserWidget
instance GObjectClass RecentChooserWidget where
  toGObject = GObject . castForeignPtr . unRecentChooserWidget
  unsafeCastGObject = RecentChooserWidget . castForeignPtr . unGObject

castToRecentChooserWidget :: GObjectClass obj => obj -> RecentChooserWidget
castToRecentChooserWidget = castTo gTypeRecentChooserWidget "RecentChooserWidget"

gTypeRecentChooserWidget :: GType
gTypeRecentChooserWidget =
  {# call fun unsafe gtk_recent_chooser_widget_get_type #}

-- ************************************************************* ColorSelection

{#pointer *GtkColorSelection as ColorSelection foreign newtype #} deriving (Eq,Ord)

mkColorSelection = (ColorSelection, objectUnrefFromMainloop)
unColorSelection (ColorSelection o) = o

class VBoxClass o => ColorSelectionClass o
toColorSelection :: ColorSelectionClass o => o -> ColorSelection
toColorSelection = unsafeCastGObject . toGObject

instance ColorSelectionClass ColorSelection
instance VBoxClass ColorSelection
instance BoxClass ColorSelection
instance ContainerClass ColorSelection
instance WidgetClass ColorSelection
instance GObjectClass ColorSelection where
  toGObject = GObject . castForeignPtr . unColorSelection
  unsafeCastGObject = ColorSelection . castForeignPtr . unGObject

castToColorSelection :: GObjectClass obj => obj -> ColorSelection
castToColorSelection = castTo gTypeColorSelection "ColorSelection"

gTypeColorSelection :: GType
gTypeColorSelection =
  {# call fun unsafe gtk_color_selection_get_type #}

-- ************************************************************** FontSelection

{#pointer *GtkFontSelection as FontSelection foreign newtype #} deriving (Eq,Ord)

mkFontSelection = (FontSelection, objectUnrefFromMainloop)
unFontSelection (FontSelection o) = o

class VBoxClass o => FontSelectionClass o
toFontSelection :: FontSelectionClass o => o -> FontSelection
toFontSelection = unsafeCastGObject . toGObject

instance FontSelectionClass FontSelection
instance VBoxClass FontSelection
instance BoxClass FontSelection
instance ContainerClass FontSelection
instance WidgetClass FontSelection
instance GObjectClass FontSelection where
  toGObject = GObject . castForeignPtr . unFontSelection
  unsafeCastGObject = FontSelection . castForeignPtr . unGObject

castToFontSelection :: GObjectClass obj => obj -> FontSelection
castToFontSelection = castTo gTypeFontSelection "FontSelection"

gTypeFontSelection :: GType
gTypeFontSelection =
  {# call fun unsafe gtk_font_selection_get_type #}

-- ********************************************************** FileChooserWidget

{#pointer *GtkFileChooserWidget as FileChooserWidget foreign newtype #} deriving (Eq,Ord)

mkFileChooserWidget = (FileChooserWidget, objectUnrefFromMainloop)
unFileChooserWidget (FileChooserWidget o) = o

class VBoxClass o => FileChooserWidgetClass o
toFileChooserWidget :: FileChooserWidgetClass o => o -> FileChooserWidget
toFileChooserWidget = unsafeCastGObject . toGObject

instance FileChooserWidgetClass FileChooserWidget
instance VBoxClass FileChooserWidget
instance BoxClass FileChooserWidget
instance ContainerClass FileChooserWidget
instance WidgetClass FileChooserWidget
instance GObjectClass FileChooserWidget where
  toGObject = GObject . castForeignPtr . unFileChooserWidget
  unsafeCastGObject = FileChooserWidget . castForeignPtr . unGObject

castToFileChooserWidget :: GObjectClass obj => obj -> FileChooserWidget
castToFileChooserWidget = castTo gTypeFileChooserWidget "FileChooserWidget"

gTypeFileChooserWidget :: GType
gTypeFileChooserWidget =
  {# call fun unsafe gtk_file_chooser_widget_get_type #}

-- *********************************************************************** HBox

{#pointer *GtkHBox as HBox foreign newtype #} deriving (Eq,Ord)

mkHBox = (HBox, objectUnrefFromMainloop)
unHBox (HBox o) = o

class BoxClass o => HBoxClass o
toHBox :: HBoxClass o => o -> HBox
toHBox = unsafeCastGObject . toGObject

instance HBoxClass HBox
instance BoxClass HBox
instance ContainerClass HBox
instance WidgetClass HBox
instance GObjectClass HBox where
  toGObject = GObject . castForeignPtr . unHBox
  unsafeCastGObject = HBox . castForeignPtr . unGObject

castToHBox :: GObjectClass obj => obj -> HBox
castToHBox = castTo gTypeHBox "HBox"

gTypeHBox :: GType
gTypeHBox =
  {# call fun unsafe gtk_hbox_get_type #}

-- ******************************************************************** InfoBar

{#pointer *GtkInfoBar as InfoBar foreign newtype #} deriving (Eq,Ord)

mkInfoBar = (InfoBar, objectUnrefFromMainloop)
unInfoBar (InfoBar o) = o

class HBoxClass o => InfoBarClass o
toInfoBar :: InfoBarClass o => o -> InfoBar
toInfoBar = unsafeCastGObject . toGObject

instance InfoBarClass InfoBar
instance HBoxClass InfoBar
instance BoxClass InfoBar
instance ContainerClass InfoBar
instance WidgetClass InfoBar
instance GObjectClass InfoBar where
  toGObject = GObject . castForeignPtr . unInfoBar
  unsafeCastGObject = InfoBar . castForeignPtr . unGObject

castToInfoBar :: GObjectClass obj => obj -> InfoBar
castToInfoBar = castTo gTypeInfoBar "InfoBar"

gTypeInfoBar :: GType
gTypeInfoBar =
  {# call fun unsafe gtk_info_bar_get_type #}

-- ********************************************************** FileChooserButton

{#pointer *GtkFileChooserButton as FileChooserButton foreign newtype #} deriving (Eq,Ord)

mkFileChooserButton = (FileChooserButton, objectUnrefFromMainloop)
unFileChooserButton (FileChooserButton o) = o

class HBoxClass o => FileChooserButtonClass o
toFileChooserButton :: FileChooserButtonClass o => o -> FileChooserButton
toFileChooserButton = unsafeCastGObject . toGObject

instance FileChooserButtonClass FileChooserButton
instance HBoxClass FileChooserButton
instance BoxClass FileChooserButton
instance ContainerClass FileChooserButton
instance WidgetClass FileChooserButton
instance GObjectClass FileChooserButton where
  toGObject = GObject . castForeignPtr . unFileChooserButton
  unsafeCastGObject = FileChooserButton . castForeignPtr . unGObject

castToFileChooserButton :: GObjectClass obj => obj -> FileChooserButton
castToFileChooserButton = castTo gTypeFileChooserButton "FileChooserButton"

gTypeFileChooserButton :: GType
gTypeFileChooserButton =
  {# call fun unsafe gtk_file_chooser_button_get_type #}

-- ****************************************************************** Statusbar

{#pointer *GtkStatusbar as Statusbar foreign newtype #} deriving (Eq,Ord)

mkStatusbar = (Statusbar, objectUnrefFromMainloop)
unStatusbar (Statusbar o) = o

class HBoxClass o => StatusbarClass o
toStatusbar :: StatusbarClass o => o -> Statusbar
toStatusbar = unsafeCastGObject . toGObject

instance StatusbarClass Statusbar
instance HBoxClass Statusbar
instance BoxClass Statusbar
instance ContainerClass Statusbar
instance WidgetClass Statusbar
instance GObjectClass Statusbar where
  toGObject = GObject . castForeignPtr . unStatusbar
  unsafeCastGObject = Statusbar . castForeignPtr . unGObject

castToStatusbar :: GObjectClass obj => obj -> Statusbar
castToStatusbar = castTo gTypeStatusbar "Statusbar"

gTypeStatusbar :: GType
gTypeStatusbar =
  {# call fun unsafe gtk_statusbar_get_type #}

-- *********************************************************************** Grid

{#pointer *GtkGrid as Grid foreign newtype #} deriving (Eq,Ord)

mkGrid = (Grid, objectUnrefFromMainloop)
unGrid (Grid o) = o

class ContainerClass o => GridClass o
toGrid :: GridClass o => o -> Grid
toGrid = unsafeCastGObject . toGObject

instance GridClass Grid
instance ContainerClass Grid
instance WidgetClass Grid
instance GObjectClass Grid where
  toGObject = GObject . castForeignPtr . unGrid
  unsafeCastGObject = Grid . castForeignPtr . unGObject

castToGrid :: GObjectClass obj => obj -> Grid
castToGrid = castTo gTypeGrid "Grid"

gTypeGrid :: GType
gTypeGrid =
  {# call fun unsafe gtk_grid_get_type #}

-- ********************************************************************** Fixed

{#pointer *GtkFixed as Fixed foreign newtype #} deriving (Eq,Ord)

mkFixed = (Fixed, objectUnrefFromMainloop)
unFixed (Fixed o) = o

class ContainerClass o => FixedClass o
toFixed :: FixedClass o => o -> Fixed
toFixed = unsafeCastGObject . toGObject

instance FixedClass Fixed
instance ContainerClass Fixed
instance WidgetClass Fixed
instance GObjectClass Fixed where
  toGObject = GObject . castForeignPtr . unFixed
  unsafeCastGObject = Fixed . castForeignPtr . unGObject

castToFixed :: GObjectClass obj => obj -> Fixed
castToFixed = castTo gTypeFixed "Fixed"

gTypeFixed :: GType
gTypeFixed =
  {# call fun unsafe gtk_fixed_get_type #}

-- ********************************************************************** Paned

{#pointer *GtkPaned as Paned foreign newtype #} deriving (Eq,Ord)

mkPaned = (Paned, objectUnrefFromMainloop)
unPaned (Paned o) = o

class ContainerClass o => PanedClass o
toPaned :: PanedClass o => o -> Paned
toPaned = unsafeCastGObject . toGObject

instance PanedClass Paned
instance ContainerClass Paned
instance WidgetClass Paned
instance GObjectClass Paned where
  toGObject = GObject . castForeignPtr . unPaned
  unsafeCastGObject = Paned . castForeignPtr . unGObject

castToPaned :: GObjectClass obj => obj -> Paned
castToPaned = castTo gTypePaned "Paned"

gTypePaned :: GType
gTypePaned =
  {# call fun unsafe gtk_paned_get_type #}

-- ********************************************************************* HPaned

{#pointer *GtkHPaned as HPaned foreign newtype #} deriving (Eq,Ord)

mkHPaned = (HPaned, objectUnrefFromMainloop)
unHPaned (HPaned o) = o

class PanedClass o => HPanedClass o
toHPaned :: HPanedClass o => o -> HPaned
toHPaned = unsafeCastGObject . toGObject

instance HPanedClass HPaned
instance PanedClass HPaned
instance ContainerClass HPaned
instance WidgetClass HPaned
instance GObjectClass HPaned where
  toGObject = GObject . castForeignPtr . unHPaned
  unsafeCastGObject = HPaned . castForeignPtr . unGObject

castToHPaned :: GObjectClass obj => obj -> HPaned
castToHPaned = castTo gTypeHPaned "HPaned"

gTypeHPaned :: GType
gTypeHPaned =
  {# call fun unsafe gtk_hpaned_get_type #}

-- ********************************************************************* VPaned

{#pointer *GtkVPaned as VPaned foreign newtype #} deriving (Eq,Ord)

mkVPaned = (VPaned, objectUnrefFromMainloop)
unVPaned (VPaned o) = o

class PanedClass o => VPanedClass o
toVPaned :: VPanedClass o => o -> VPaned
toVPaned = unsafeCastGObject . toGObject

instance VPanedClass VPaned
instance PanedClass VPaned
instance ContainerClass VPaned
instance WidgetClass VPaned
instance GObjectClass VPaned where
  toGObject = GObject . castForeignPtr . unVPaned
  unsafeCastGObject = VPaned . castForeignPtr . unGObject

castToVPaned :: GObjectClass obj => obj -> VPaned
castToVPaned = castTo gTypeVPaned "VPaned"

gTypeVPaned :: GType
gTypeVPaned =
  {# call fun unsafe gtk_vpaned_get_type #}

-- ******************************************************************* IconView

{#pointer *GtkIconView as IconView foreign newtype #} deriving (Eq,Ord)

mkIconView = (IconView, objectUnrefFromMainloop)
unIconView (IconView o) = o

class ContainerClass o => IconViewClass o
toIconView :: IconViewClass o => o -> IconView
toIconView = unsafeCastGObject . toGObject

instance IconViewClass IconView
instance ContainerClass IconView
instance WidgetClass IconView
instance GObjectClass IconView where
  toGObject = GObject . castForeignPtr . unIconView
  unsafeCastGObject = IconView . castForeignPtr . unGObject

castToIconView :: GObjectClass obj => obj -> IconView
castToIconView = castTo gTypeIconView "IconView"

gTypeIconView :: GType
gTypeIconView =
  {# call fun unsafe gtk_icon_view_get_type #}

-- ********************************************************************* Layout

{#pointer *GtkLayout as Layout foreign newtype #} deriving (Eq,Ord)

mkLayout = (Layout, objectUnrefFromMainloop)
unLayout (Layout o) = o

class ContainerClass o => LayoutClass o
toLayout :: LayoutClass o => o -> Layout
toLayout = unsafeCastGObject . toGObject

instance LayoutClass Layout
instance ContainerClass Layout
instance WidgetClass Layout
instance GObjectClass Layout where
  toGObject = GObject . castForeignPtr . unLayout
  unsafeCastGObject = Layout . castForeignPtr . unGObject

castToLayout :: GObjectClass obj => obj -> Layout
castToLayout = castTo gTypeLayout "Layout"

gTypeLayout :: GType
gTypeLayout =
  {# call fun unsafe gtk_layout_get_type #}

-- ****************************************************************** MenuShell

{#pointer *GtkMenuShell as MenuShell foreign newtype #} deriving (Eq,Ord)

mkMenuShell = (MenuShell, objectUnrefFromMainloop)
unMenuShell (MenuShell o) = o

class ContainerClass o => MenuShellClass o
toMenuShell :: MenuShellClass o => o -> MenuShell
toMenuShell = unsafeCastGObject . toGObject

instance MenuShellClass MenuShell
instance ContainerClass MenuShell
instance WidgetClass MenuShell
instance GObjectClass MenuShell where
  toGObject = GObject . castForeignPtr . unMenuShell
  unsafeCastGObject = MenuShell . castForeignPtr . unGObject

castToMenuShell :: GObjectClass obj => obj -> MenuShell
castToMenuShell = castTo gTypeMenuShell "MenuShell"

gTypeMenuShell :: GType
gTypeMenuShell =
  {# call fun unsafe gtk_menu_shell_get_type #}

-- *********************************************************************** Menu

{#pointer *GtkMenu as Menu foreign newtype #} deriving (Eq,Ord)

mkMenu = (Menu, objectUnrefFromMainloop)
unMenu (Menu o) = o

class MenuShellClass o => MenuClass o
toMenu :: MenuClass o => o -> Menu
toMenu = unsafeCastGObject . toGObject

instance MenuClass Menu
instance MenuShellClass Menu
instance ContainerClass Menu
instance WidgetClass Menu
instance GObjectClass Menu where
  toGObject = GObject . castForeignPtr . unMenu
  unsafeCastGObject = Menu . castForeignPtr . unGObject

castToMenu :: GObjectClass obj => obj -> Menu
castToMenu = castTo gTypeMenu "Menu"

gTypeMenu :: GType
gTypeMenu =
  {# call fun unsafe gtk_menu_get_type #}

-- ********************************************************** RecentChooserMenu

{#pointer *GtkRecentChooserMenu as RecentChooserMenu foreign newtype #} deriving (Eq,Ord)

mkRecentChooserMenu = (RecentChooserMenu, objectUnrefFromMainloop)
unRecentChooserMenu (RecentChooserMenu o) = o

class MenuClass o => RecentChooserMenuClass o
toRecentChooserMenu :: RecentChooserMenuClass o => o -> RecentChooserMenu
toRecentChooserMenu = unsafeCastGObject . toGObject

instance RecentChooserMenuClass RecentChooserMenu
instance MenuClass RecentChooserMenu
instance MenuShellClass RecentChooserMenu
instance ContainerClass RecentChooserMenu
instance WidgetClass RecentChooserMenu
instance GObjectClass RecentChooserMenu where
  toGObject = GObject . castForeignPtr . unRecentChooserMenu
  unsafeCastGObject = RecentChooserMenu . castForeignPtr . unGObject

castToRecentChooserMenu :: GObjectClass obj => obj -> RecentChooserMenu
castToRecentChooserMenu = castTo gTypeRecentChooserMenu "RecentChooserMenu"

gTypeRecentChooserMenu :: GType
gTypeRecentChooserMenu =
  {# call fun unsafe gtk_recent_chooser_menu_get_type #}

-- ******************************************************************** MenuBar

{#pointer *GtkMenuBar as MenuBar foreign newtype #} deriving (Eq,Ord)

mkMenuBar = (MenuBar, objectUnrefFromMainloop)
unMenuBar (MenuBar o) = o

class MenuShellClass o => MenuBarClass o
toMenuBar :: MenuBarClass o => o -> MenuBar
toMenuBar = unsafeCastGObject . toGObject

instance MenuBarClass MenuBar
instance MenuShellClass MenuBar
instance ContainerClass MenuBar
instance WidgetClass MenuBar
instance GObjectClass MenuBar where
  toGObject = GObject . castForeignPtr . unMenuBar
  unsafeCastGObject = MenuBar . castForeignPtr . unGObject

castToMenuBar :: GObjectClass obj => obj -> MenuBar
castToMenuBar = castTo gTypeMenuBar "MenuBar"

gTypeMenuBar :: GType
gTypeMenuBar =
  {# call fun unsafe gtk_menu_bar_get_type #}

-- ******************************************************************* Notebook

{#pointer *GtkNotebook as Notebook foreign newtype #} deriving (Eq,Ord)

mkNotebook = (Notebook, objectUnrefFromMainloop)
unNotebook (Notebook o) = o

class ContainerClass o => NotebookClass o
toNotebook :: NotebookClass o => o -> Notebook
toNotebook = unsafeCastGObject . toGObject

instance NotebookClass Notebook
instance ContainerClass Notebook
instance WidgetClass Notebook
instance GObjectClass Notebook where
  toGObject = GObject . castForeignPtr . unNotebook
  unsafeCastGObject = Notebook . castForeignPtr . unGObject

castToNotebook :: GObjectClass obj => obj -> Notebook
castToNotebook = castTo gTypeNotebook "Notebook"

gTypeNotebook :: GType
gTypeNotebook =
  {# call fun unsafe gtk_notebook_get_type #}

-- ********************************************************************** Table

{#pointer *GtkTable as Table foreign newtype #} deriving (Eq,Ord)

mkTable = (Table, objectUnrefFromMainloop)
unTable (Table o) = o

class ContainerClass o => TableClass o
toTable :: TableClass o => o -> Table
toTable = unsafeCastGObject . toGObject

instance TableClass Table
instance ContainerClass Table
instance WidgetClass Table
instance GObjectClass Table where
  toGObject = GObject . castForeignPtr . unTable
  unsafeCastGObject = Table . castForeignPtr . unGObject

castToTable :: GObjectClass obj => obj -> Table
castToTable = castTo gTypeTable "Table"

gTypeTable :: GType
gTypeTable =
  {# call fun unsafe gtk_table_get_type #}

-- ******************************************************************* TextView

{#pointer *GtkTextView as TextView foreign newtype #} deriving (Eq,Ord)

mkTextView = (TextView, objectUnrefFromMainloop)
unTextView (TextView o) = o

class ContainerClass o => TextViewClass o
toTextView :: TextViewClass o => o -> TextView
toTextView = unsafeCastGObject . toGObject

instance TextViewClass TextView
instance ContainerClass TextView
instance WidgetClass TextView
instance GObjectClass TextView where
  toGObject = GObject . castForeignPtr . unTextView
  unsafeCastGObject = TextView . castForeignPtr . unGObject

castToTextView :: GObjectClass obj => obj -> TextView
castToTextView = castTo gTypeTextView "TextView"

gTypeTextView :: GType
gTypeTextView =
  {# call fun unsafe gtk_text_view_get_type #}

-- ******************************************************************** Toolbar

{#pointer *GtkToolbar as Toolbar foreign newtype #} deriving (Eq,Ord)

mkToolbar = (Toolbar, objectUnrefFromMainloop)
unToolbar (Toolbar o) = o

class ContainerClass o => ToolbarClass o
toToolbar :: ToolbarClass o => o -> Toolbar
toToolbar = unsafeCastGObject . toGObject

instance ToolbarClass Toolbar
instance ContainerClass Toolbar
instance WidgetClass Toolbar
instance GObjectClass Toolbar where
  toGObject = GObject . castForeignPtr . unToolbar
  unsafeCastGObject = Toolbar . castForeignPtr . unGObject

castToToolbar :: GObjectClass obj => obj -> Toolbar
castToToolbar = castTo gTypeToolbar "Toolbar"

gTypeToolbar :: GType
gTypeToolbar =
  {# call fun unsafe gtk_toolbar_get_type #}

-- ******************************************************************* TreeView

{#pointer *GtkTreeView as TreeView foreign newtype #} deriving (Eq,Ord)

mkTreeView = (TreeView, objectUnrefFromMainloop)
unTreeView (TreeView o) = o

class ContainerClass o => TreeViewClass o
toTreeView :: TreeViewClass o => o -> TreeView
toTreeView = unsafeCastGObject . toGObject

instance TreeViewClass TreeView
instance ContainerClass TreeView
instance WidgetClass TreeView
instance GObjectClass TreeView where
  toGObject = GObject . castForeignPtr . unTreeView
  unsafeCastGObject = TreeView . castForeignPtr . unGObject

castToTreeView :: GObjectClass obj => obj -> TreeView
castToTreeView = castTo gTypeTreeView "TreeView"

gTypeTreeView :: GType
gTypeTreeView =
  {# call fun unsafe gtk_tree_view_get_type #}

-- ******************************************************************* Calendar

{#pointer *GtkCalendar as Calendar foreign newtype #} deriving (Eq,Ord)

mkCalendar = (Calendar, objectUnrefFromMainloop)
unCalendar (Calendar o) = o

class WidgetClass o => CalendarClass o
toCalendar :: CalendarClass o => o -> Calendar
toCalendar = unsafeCastGObject . toGObject

instance CalendarClass Calendar
instance WidgetClass Calendar
instance GObjectClass Calendar where
  toGObject = GObject . castForeignPtr . unCalendar
  unsafeCastGObject = Calendar . castForeignPtr . unGObject

castToCalendar :: GObjectClass obj => obj -> Calendar
castToCalendar = castTo gTypeCalendar "Calendar"

gTypeCalendar :: GType
gTypeCalendar =
  {# call fun unsafe gtk_calendar_get_type #}

-- ******************************************************************* CellView

{#pointer *GtkCellView as CellView foreign newtype #} deriving (Eq,Ord)

mkCellView = (CellView, objectUnrefFromMainloop)
unCellView (CellView o) = o

class WidgetClass o => CellViewClass o
toCellView :: CellViewClass o => o -> CellView
toCellView = unsafeCastGObject . toGObject

instance CellViewClass CellView
instance WidgetClass CellView
instance GObjectClass CellView where
  toGObject = GObject . castForeignPtr . unCellView
  unsafeCastGObject = CellView . castForeignPtr . unGObject

castToCellView :: GObjectClass obj => obj -> CellView
castToCellView = castTo gTypeCellView "CellView"

gTypeCellView :: GType
gTypeCellView =
  {# call fun unsafe gtk_cell_view_get_type #}

-- ********************************************************************* GLArea

{#pointer *GtkGLArea as GLArea foreign newtype #} deriving (Eq,Ord)

mkGLArea = (GLArea, objectUnrefFromMainloop)
unGLArea (GLArea o) = o

class WidgetClass o => GLAreaClass o
toGLArea :: GLAreaClass o => o -> GLArea
toGLArea = unsafeCastGObject . toGObject

instance GLAreaClass GLArea
instance WidgetClass GLArea
instance GObjectClass GLArea where
  toGObject = GObject . castForeignPtr . unGLArea
  unsafeCastGObject = GLArea . castForeignPtr . unGObject

castToGLArea :: GObjectClass obj => obj -> GLArea
castToGLArea = castTo gTypeGLArea "GLArea"

gTypeGLArea :: GType
gTypeGLArea =
  {# call fun unsafe gtk_gl_area_get_type #}

-- **************************************************************** DrawingArea

{#pointer *GtkDrawingArea as DrawingArea foreign newtype #} deriving (Eq,Ord)

mkDrawingArea = (DrawingArea, objectUnrefFromMainloop)
unDrawingArea (DrawingArea o) = o

class WidgetClass o => DrawingAreaClass o
toDrawingArea :: DrawingAreaClass o => o -> DrawingArea
toDrawingArea = unsafeCastGObject . toGObject

instance DrawingAreaClass DrawingArea
instance WidgetClass DrawingArea
instance GObjectClass DrawingArea where
  toGObject = GObject . castForeignPtr . unDrawingArea
  unsafeCastGObject = DrawingArea . castForeignPtr . unGObject

castToDrawingArea :: GObjectClass obj => obj -> DrawingArea
castToDrawingArea = castTo gTypeDrawingArea "DrawingArea"

gTypeDrawingArea :: GType
gTypeDrawingArea =
  {# call fun unsafe gtk_drawing_area_get_type #}

-- ******************************************************************** Spinner

{#pointer *GtkSpinner as Spinner foreign newtype #} deriving (Eq,Ord)

mkSpinner = (Spinner, objectUnrefFromMainloop)
unSpinner (Spinner o) = o

class DrawingAreaClass o => SpinnerClass o
toSpinner :: SpinnerClass o => o -> Spinner
toSpinner = unsafeCastGObject . toGObject

instance SpinnerClass Spinner
instance DrawingAreaClass Spinner
instance WidgetClass Spinner
instance GObjectClass Spinner where
  toGObject = GObject . castForeignPtr . unSpinner
  unsafeCastGObject = Spinner . castForeignPtr . unGObject

castToSpinner :: GObjectClass obj => obj -> Spinner
castToSpinner = castTo gTypeSpinner "Spinner"

gTypeSpinner :: GType
gTypeSpinner =
  {# call fun unsafe gtk_spinner_get_type #}

-- ********************************************************************** Entry

{#pointer *GtkEntry as Entry foreign newtype #} deriving (Eq,Ord)

mkEntry = (Entry, objectUnrefFromMainloop)
unEntry (Entry o) = o

class WidgetClass o => EntryClass o
toEntry :: EntryClass o => o -> Entry
toEntry = unsafeCastGObject . toGObject

instance EntryClass Entry
instance WidgetClass Entry
instance GObjectClass Entry where
  toGObject = GObject . castForeignPtr . unEntry
  unsafeCastGObject = Entry . castForeignPtr . unGObject

castToEntry :: GObjectClass obj => obj -> Entry
castToEntry = castTo gTypeEntry "Entry"

gTypeEntry :: GType
gTypeEntry =
  {# call fun unsafe gtk_entry_get_type #}

-- ***************************************************************** SpinButton

{#pointer *GtkSpinButton as SpinButton foreign newtype #} deriving (Eq,Ord)

mkSpinButton = (SpinButton, objectUnrefFromMainloop)
unSpinButton (SpinButton o) = o

class EntryClass o => SpinButtonClass o
toSpinButton :: SpinButtonClass o => o -> SpinButton
toSpinButton = unsafeCastGObject . toGObject

instance SpinButtonClass SpinButton
instance EntryClass SpinButton
instance WidgetClass SpinButton
instance GObjectClass SpinButton where
  toGObject = GObject . castForeignPtr . unSpinButton
  unsafeCastGObject = SpinButton . castForeignPtr . unGObject

castToSpinButton :: GObjectClass obj => obj -> SpinButton
castToSpinButton = castTo gTypeSpinButton "SpinButton"

gTypeSpinButton :: GType
gTypeSpinButton =
  {# call fun unsafe gtk_spin_button_get_type #}

-- ********************************************************************** Range

{#pointer *GtkRange as Range foreign newtype #} deriving (Eq,Ord)

mkRange = (Range, objectUnrefFromMainloop)
unRange (Range o) = o

class WidgetClass o => RangeClass o
toRange :: RangeClass o => o -> Range
toRange = unsafeCastGObject . toGObject

instance RangeClass Range
instance WidgetClass Range
instance GObjectClass Range where
  toGObject = GObject . castForeignPtr . unRange
  unsafeCastGObject = Range . castForeignPtr . unGObject

castToRange :: GObjectClass obj => obj -> Range
castToRange = castTo gTypeRange "Range"

gTypeRange :: GType
gTypeRange =
  {# call fun unsafe gtk_range_get_type #}

-- ********************************************************************** Scale

{#pointer *GtkScale as Scale foreign newtype #} deriving (Eq,Ord)

mkScale = (Scale, objectUnrefFromMainloop)
unScale (Scale o) = o

class RangeClass o => ScaleClass o
toScale :: ScaleClass o => o -> Scale
toScale = unsafeCastGObject . toGObject

instance ScaleClass Scale
instance RangeClass Scale
instance WidgetClass Scale
instance GObjectClass Scale where
  toGObject = GObject . castForeignPtr . unScale
  unsafeCastGObject = Scale . castForeignPtr . unGObject

castToScale :: GObjectClass obj => obj -> Scale
castToScale = castTo gTypeScale "Scale"

gTypeScale :: GType
gTypeScale =
  {# call fun unsafe gtk_scale_get_type #}

-- ********************************************************************* HScale

{#pointer *GtkHScale as HScale foreign newtype #} deriving (Eq,Ord)

mkHScale = (HScale, objectUnrefFromMainloop)
unHScale (HScale o) = o

class ScaleClass o => HScaleClass o
toHScale :: HScaleClass o => o -> HScale
toHScale = unsafeCastGObject . toGObject

instance HScaleClass HScale
instance ScaleClass HScale
instance RangeClass HScale
instance WidgetClass HScale
instance GObjectClass HScale where
  toGObject = GObject . castForeignPtr . unHScale
  unsafeCastGObject = HScale . castForeignPtr . unGObject

castToHScale :: GObjectClass obj => obj -> HScale
castToHScale = castTo gTypeHScale "HScale"

gTypeHScale :: GType
gTypeHScale =
  {# call fun unsafe gtk_hscale_get_type #}

-- ********************************************************************* VScale

{#pointer *GtkVScale as VScale foreign newtype #} deriving (Eq,Ord)

mkVScale = (VScale, objectUnrefFromMainloop)
unVScale (VScale o) = o

class ScaleClass o => VScaleClass o
toVScale :: VScaleClass o => o -> VScale
toVScale = unsafeCastGObject . toGObject

instance VScaleClass VScale
instance ScaleClass VScale
instance RangeClass VScale
instance WidgetClass VScale
instance GObjectClass VScale where
  toGObject = GObject . castForeignPtr . unVScale
  unsafeCastGObject = VScale . castForeignPtr . unGObject

castToVScale :: GObjectClass obj => obj -> VScale
castToVScale = castTo gTypeVScale "VScale"

gTypeVScale :: GType
gTypeVScale =
  {# call fun unsafe gtk_vscale_get_type #}

-- ****************************************************************** Scrollbar

{#pointer *GtkScrollbar as Scrollbar foreign newtype #} deriving (Eq,Ord)

mkScrollbar = (Scrollbar, objectUnrefFromMainloop)
unScrollbar (Scrollbar o) = o

class RangeClass o => ScrollbarClass o
toScrollbar :: ScrollbarClass o => o -> Scrollbar
toScrollbar = unsafeCastGObject . toGObject

instance ScrollbarClass Scrollbar
instance RangeClass Scrollbar
instance WidgetClass Scrollbar
instance GObjectClass Scrollbar where
  toGObject = GObject . castForeignPtr . unScrollbar
  unsafeCastGObject = Scrollbar . castForeignPtr . unGObject

castToScrollbar :: GObjectClass obj => obj -> Scrollbar
castToScrollbar = castTo gTypeScrollbar "Scrollbar"

gTypeScrollbar :: GType
gTypeScrollbar =
  {# call fun unsafe gtk_scrollbar_get_type #}

-- ***************************************************************** HScrollbar

{#pointer *GtkHScrollbar as HScrollbar foreign newtype #} deriving (Eq,Ord)

mkHScrollbar = (HScrollbar, objectUnrefFromMainloop)
unHScrollbar (HScrollbar o) = o

class ScrollbarClass o => HScrollbarClass o
toHScrollbar :: HScrollbarClass o => o -> HScrollbar
toHScrollbar = unsafeCastGObject . toGObject

instance HScrollbarClass HScrollbar
instance ScrollbarClass HScrollbar
instance RangeClass HScrollbar
instance WidgetClass HScrollbar
instance GObjectClass HScrollbar where
  toGObject = GObject . castForeignPtr . unHScrollbar
  unsafeCastGObject = HScrollbar . castForeignPtr . unGObject

castToHScrollbar :: GObjectClass obj => obj -> HScrollbar
castToHScrollbar = castTo gTypeHScrollbar "HScrollbar"

gTypeHScrollbar :: GType
gTypeHScrollbar =
  {# call fun unsafe gtk_hscrollbar_get_type #}

-- ***************************************************************** VScrollbar

{#pointer *GtkVScrollbar as VScrollbar foreign newtype #} deriving (Eq,Ord)

mkVScrollbar = (VScrollbar, objectUnrefFromMainloop)
unVScrollbar (VScrollbar o) = o

class ScrollbarClass o => VScrollbarClass o
toVScrollbar :: VScrollbarClass o => o -> VScrollbar
toVScrollbar = unsafeCastGObject . toGObject

instance VScrollbarClass VScrollbar
instance ScrollbarClass VScrollbar
instance RangeClass VScrollbar
instance WidgetClass VScrollbar
instance GObjectClass VScrollbar where
  toGObject = GObject . castForeignPtr . unVScrollbar
  unsafeCastGObject = VScrollbar . castForeignPtr . unGObject

castToVScrollbar :: GObjectClass obj => obj -> VScrollbar
castToVScrollbar = castTo gTypeVScrollbar "VScrollbar"

gTypeVScrollbar :: GType
gTypeVScrollbar =
  {# call fun unsafe gtk_vscrollbar_get_type #}

-- ****************************************************************** Separator

{#pointer *GtkSeparator as Separator foreign newtype #} deriving (Eq,Ord)

mkSeparator = (Separator, objectUnrefFromMainloop)
unSeparator (Separator o) = o

class WidgetClass o => SeparatorClass o
toSeparator :: SeparatorClass o => o -> Separator
toSeparator = unsafeCastGObject . toGObject

instance SeparatorClass Separator
instance WidgetClass Separator
instance GObjectClass Separator where
  toGObject = GObject . castForeignPtr . unSeparator
  unsafeCastGObject = Separator . castForeignPtr . unGObject

castToSeparator :: GObjectClass obj => obj -> Separator
castToSeparator = castTo gTypeSeparator "Separator"

gTypeSeparator :: GType
gTypeSeparator =
  {# call fun unsafe gtk_separator_get_type #}

-- ***************************************************************** HSeparator

{#pointer *GtkHSeparator as HSeparator foreign newtype #} deriving (Eq,Ord)

mkHSeparator = (HSeparator, objectUnrefFromMainloop)
unHSeparator (HSeparator o) = o

class SeparatorClass o => HSeparatorClass o
toHSeparator :: HSeparatorClass o => o -> HSeparator
toHSeparator = unsafeCastGObject . toGObject

instance HSeparatorClass HSeparator
instance SeparatorClass HSeparator
instance WidgetClass HSeparator
instance GObjectClass HSeparator where
  toGObject = GObject . castForeignPtr . unHSeparator
  unsafeCastGObject = HSeparator . castForeignPtr . unGObject

castToHSeparator :: GObjectClass obj => obj -> HSeparator
castToHSeparator = castTo gTypeHSeparator "HSeparator"

gTypeHSeparator :: GType
gTypeHSeparator =
  {# call fun unsafe gtk_hseparator_get_type #}

-- ***************************************************************** VSeparator

{#pointer *GtkVSeparator as VSeparator foreign newtype #} deriving (Eq,Ord)

mkVSeparator = (VSeparator, objectUnrefFromMainloop)
unVSeparator (VSeparator o) = o

class SeparatorClass o => VSeparatorClass o
toVSeparator :: VSeparatorClass o => o -> VSeparator
toVSeparator = unsafeCastGObject . toGObject

instance VSeparatorClass VSeparator
instance SeparatorClass VSeparator
instance WidgetClass VSeparator
instance GObjectClass VSeparator where
  toGObject = GObject . castForeignPtr . unVSeparator
  unsafeCastGObject = VSeparator . castForeignPtr . unGObject

castToVSeparator :: GObjectClass obj => obj -> VSeparator
castToVSeparator = castTo gTypeVSeparator "VSeparator"

gTypeVSeparator :: GType
gTypeVSeparator =
  {# call fun unsafe gtk_vseparator_get_type #}

-- ****************************************************************** Invisible

{#pointer *GtkInvisible as Invisible foreign newtype #} deriving (Eq,Ord)

mkInvisible = (Invisible, objectUnrefFromMainloop)
unInvisible (Invisible o) = o

class WidgetClass o => InvisibleClass o
toInvisible :: InvisibleClass o => o -> Invisible
toInvisible = unsafeCastGObject . toGObject

instance InvisibleClass Invisible
instance WidgetClass Invisible
instance GObjectClass Invisible where
  toGObject = GObject . castForeignPtr . unInvisible
  unsafeCastGObject = Invisible . castForeignPtr . unGObject

castToInvisible :: GObjectClass obj => obj -> Invisible
castToInvisible = castTo gTypeInvisible "Invisible"

gTypeInvisible :: GType
gTypeInvisible =
  {# call fun unsafe gtk_invisible_get_type #}

-- **************************************************************** ProgressBar

{#pointer *GtkProgressBar as ProgressBar foreign newtype #} deriving (Eq,Ord)

mkProgressBar = (ProgressBar, objectUnrefFromMainloop)
unProgressBar (ProgressBar o) = o

class WidgetClass o => ProgressBarClass o
toProgressBar :: ProgressBarClass o => o -> ProgressBar
toProgressBar = unsafeCastGObject . toGObject

instance ProgressBarClass ProgressBar
instance WidgetClass ProgressBar
instance GObjectClass ProgressBar where
  toGObject = GObject . castForeignPtr . unProgressBar
  unsafeCastGObject = ProgressBar . castForeignPtr . unGObject

castToProgressBar :: GObjectClass obj => obj -> ProgressBar
castToProgressBar = castTo gTypeProgressBar "ProgressBar"

gTypeProgressBar :: GType
gTypeProgressBar =
  {# call fun unsafe gtk_progress_bar_get_type #}

-- ******************************************************************* LevelBar

{#pointer *GtkLevelBar as LevelBar foreign newtype #} deriving (Eq,Ord)

mkLevelBar = (LevelBar, objectUnrefFromMainloop)
unLevelBar (LevelBar o) = o

class WidgetClass o => LevelBarClass o
toLevelBar :: LevelBarClass o => o -> LevelBar
toLevelBar = unsafeCastGObject . toGObject

instance LevelBarClass LevelBar
instance WidgetClass LevelBar
instance GObjectClass LevelBar where
  toGObject = GObject . castForeignPtr . unLevelBar
  unsafeCastGObject = LevelBar . castForeignPtr . unGObject

castToLevelBar :: GObjectClass obj => obj -> LevelBar
castToLevelBar = castTo gTypeLevelBar "LevelBar"

gTypeLevelBar :: GType
gTypeLevelBar =
  {# call fun unsafe gtk_level_bar_get_type #}

-- ***************************************************************** Adjustment

{#pointer *GtkAdjustment as Adjustment foreign newtype #} deriving (Eq,Ord)

mkAdjustment = (Adjustment, objectUnrefFromMainloop)
unAdjustment (Adjustment o) = o

class GObjectClass o => AdjustmentClass o
toAdjustment :: AdjustmentClass o => o -> Adjustment
toAdjustment = unsafeCastGObject . toGObject

instance AdjustmentClass Adjustment
instance GObjectClass Adjustment where
  toGObject = GObject . castForeignPtr . unAdjustment
  unsafeCastGObject = Adjustment . castForeignPtr . unGObject

castToAdjustment :: GObjectClass obj => obj -> Adjustment
castToAdjustment = castTo gTypeAdjustment "Adjustment"

gTypeAdjustment :: GType
gTypeAdjustment =
  {# call fun unsafe gtk_adjustment_get_type #}

-- ****************************************************************** IMContext

{#pointer *GtkIMContext as IMContext foreign newtype #} deriving (Eq,Ord)

mkIMContext = (IMContext, objectUnrefFromMainloop)
unIMContext (IMContext o) = o

class GObjectClass o => IMContextClass o
toIMContext :: IMContextClass o => o -> IMContext
toIMContext = unsafeCastGObject . toGObject

instance IMContextClass IMContext
instance GObjectClass IMContext where
  toGObject = GObject . castForeignPtr . unIMContext
  unsafeCastGObject = IMContext . castForeignPtr . unGObject

castToIMContext :: GObjectClass obj => obj -> IMContext
castToIMContext = castTo gTypeIMContext "IMContext"

gTypeIMContext :: GType
gTypeIMContext =
  {# call fun unsafe gtk_im_context_get_type #}

-- ************************************************************* IMMulticontext

{#pointer *GtkIMMulticontext as IMMulticontext foreign newtype #} deriving (Eq,Ord)

mkIMMulticontext = (IMMulticontext, objectUnrefFromMainloop)
unIMMulticontext (IMMulticontext o) = o

class IMContextClass o => IMMulticontextClass o
toIMMulticontext :: IMMulticontextClass o => o -> IMMulticontext
toIMMulticontext = unsafeCastGObject . toGObject

instance IMMulticontextClass IMMulticontext
instance IMContextClass IMMulticontext
instance GObjectClass IMMulticontext where
  toGObject = GObject . castForeignPtr . unIMMulticontext
  unsafeCastGObject = IMMulticontext . castForeignPtr . unGObject

castToIMMulticontext :: GObjectClass obj => obj -> IMMulticontext
castToIMMulticontext = castTo gTypeIMMulticontext "IMMulticontext"

gTypeIMMulticontext :: GType
gTypeIMMulticontext =
  {# call fun unsafe gtk_im_multicontext_get_type #}

-- ************************************************************ IMContextSimple

{#pointer *GtkIMContextSimple as IMContextSimple foreign newtype #} deriving (Eq,Ord)

mkIMContextSimple = (IMContextSimple, objectUnrefFromMainloop)
unIMContextSimple (IMContextSimple o) = o

class IMContextClass o => IMContextSimpleClass o
toIMContextSimple :: IMContextSimpleClass o => o -> IMContextSimple
toIMContextSimple = unsafeCastGObject . toGObject

instance IMContextSimpleClass IMContextSimple
instance IMContextClass IMContextSimple
instance GObjectClass IMContextSimple where
  toGObject = GObject . castForeignPtr . unIMContextSimple
  unsafeCastGObject = IMContextSimple . castForeignPtr . unGObject

castToIMContextSimple :: GObjectClass obj => obj -> IMContextSimple
castToIMContextSimple = castTo gTypeIMContextSimple "IMContextSimple"

gTypeIMContextSimple :: GType
gTypeIMContextSimple =
  {# call fun unsafe gtk_im_context_simple_get_type #}

-- ************************************************************* TreeViewColumn

{#pointer *GtkTreeViewColumn as TreeViewColumn foreign newtype #} deriving (Eq,Ord)

mkTreeViewColumn = (TreeViewColumn, objectUnrefFromMainloop)
unTreeViewColumn (TreeViewColumn o) = o

class GObjectClass o => TreeViewColumnClass o
toTreeViewColumn :: TreeViewColumnClass o => o -> TreeViewColumn
toTreeViewColumn = unsafeCastGObject . toGObject

instance TreeViewColumnClass TreeViewColumn
instance GObjectClass TreeViewColumn where
  toGObject = GObject . castForeignPtr . unTreeViewColumn
  unsafeCastGObject = TreeViewColumn . castForeignPtr . unGObject

castToTreeViewColumn :: GObjectClass obj => obj -> TreeViewColumn
castToTreeViewColumn = castTo gTypeTreeViewColumn "TreeViewColumn"

gTypeTreeViewColumn :: GType
gTypeTreeViewColumn =
  {# call fun unsafe gtk_tree_view_column_get_type #}

-- *************************************************************** CellRenderer

{#pointer *GtkCellRenderer as CellRenderer foreign newtype #} deriving (Eq,Ord)

mkCellRenderer = (CellRenderer, objectUnrefFromMainloop)
unCellRenderer (CellRenderer o) = o

class GObjectClass o => CellRendererClass o
toCellRenderer :: CellRendererClass o => o -> CellRenderer
toCellRenderer = unsafeCastGObject . toGObject

instance CellRendererClass CellRenderer
instance GObjectClass CellRenderer where
  toGObject = GObject . castForeignPtr . unCellRenderer
  unsafeCastGObject = CellRenderer . castForeignPtr . unGObject

castToCellRenderer :: GObjectClass obj => obj -> CellRenderer
castToCellRenderer = castTo gTypeCellRenderer "CellRenderer"

gTypeCellRenderer :: GType
gTypeCellRenderer =
  {# call fun unsafe gtk_cell_renderer_get_type #}

-- ******************************************************** CellRendererSpinner

{#pointer *GtkCellRendererSpinner as CellRendererSpinner foreign newtype #} deriving (Eq,Ord)

mkCellRendererSpinner = (CellRendererSpinner, objectUnrefFromMainloop)
unCellRendererSpinner (CellRendererSpinner o) = o

class CellRendererClass o => CellRendererSpinnerClass o
toCellRendererSpinner :: CellRendererSpinnerClass o => o -> CellRendererSpinner
toCellRendererSpinner = unsafeCastGObject . toGObject

instance CellRendererSpinnerClass CellRendererSpinner
instance CellRendererClass CellRendererSpinner
instance GObjectClass CellRendererSpinner where
  toGObject = GObject . castForeignPtr . unCellRendererSpinner
  unsafeCastGObject = CellRendererSpinner . castForeignPtr . unGObject

castToCellRendererSpinner :: GObjectClass obj => obj -> CellRendererSpinner
castToCellRendererSpinner = castTo gTypeCellRendererSpinner "CellRendererSpinner"

gTypeCellRendererSpinner :: GType
gTypeCellRendererSpinner =
  {# call fun unsafe gtk_cell_renderer_spinner_get_type #}

-- ********************************************************* CellRendererPixbuf

{#pointer *GtkCellRendererPixbuf as CellRendererPixbuf foreign newtype #} deriving (Eq,Ord)

mkCellRendererPixbuf = (CellRendererPixbuf, objectUnrefFromMainloop)
unCellRendererPixbuf (CellRendererPixbuf o) = o

class CellRendererClass o => CellRendererPixbufClass o
toCellRendererPixbuf :: CellRendererPixbufClass o => o -> CellRendererPixbuf
toCellRendererPixbuf = unsafeCastGObject . toGObject

instance CellRendererPixbufClass CellRendererPixbuf
instance CellRendererClass CellRendererPixbuf
instance GObjectClass CellRendererPixbuf where
  toGObject = GObject . castForeignPtr . unCellRendererPixbuf
  unsafeCastGObject = CellRendererPixbuf . castForeignPtr . unGObject

castToCellRendererPixbuf :: GObjectClass obj => obj -> CellRendererPixbuf
castToCellRendererPixbuf = castTo gTypeCellRendererPixbuf "CellRendererPixbuf"

gTypeCellRendererPixbuf :: GType
gTypeCellRendererPixbuf =
  {# call fun unsafe gtk_cell_renderer_pixbuf_get_type #}

-- *********************************************************** CellRendererText

{#pointer *GtkCellRendererText as CellRendererText foreign newtype #} deriving (Eq,Ord)

mkCellRendererText = (CellRendererText, objectUnrefFromMainloop)
unCellRendererText (CellRendererText o) = o

class CellRendererClass o => CellRendererTextClass o
toCellRendererText :: CellRendererTextClass o => o -> CellRendererText
toCellRendererText = unsafeCastGObject . toGObject

instance CellRendererTextClass CellRendererText
instance CellRendererClass CellRendererText
instance GObjectClass CellRendererText where
  toGObject = GObject . castForeignPtr . unCellRendererText
  unsafeCastGObject = CellRendererText . castForeignPtr . unGObject

castToCellRendererText :: GObjectClass obj => obj -> CellRendererText
castToCellRendererText = castTo gTypeCellRendererText "CellRendererText"

gTypeCellRendererText :: GType
gTypeCellRendererText =
  {# call fun unsafe gtk_cell_renderer_text_get_type #}

-- ********************************************************** CellRendererAccel

{#pointer *GtkCellRendererAccel as CellRendererAccel foreign newtype #} deriving (Eq,Ord)

mkCellRendererAccel = (CellRendererAccel, objectUnrefFromMainloop)
unCellRendererAccel (CellRendererAccel o) = o

class CellRendererTextClass o => CellRendererAccelClass o
toCellRendererAccel :: CellRendererAccelClass o => o -> CellRendererAccel
toCellRendererAccel = unsafeCastGObject . toGObject

instance CellRendererAccelClass CellRendererAccel
instance CellRendererTextClass CellRendererAccel
instance CellRendererClass CellRendererAccel
instance GObjectClass CellRendererAccel where
  toGObject = GObject . castForeignPtr . unCellRendererAccel
  unsafeCastGObject = CellRendererAccel . castForeignPtr . unGObject

castToCellRendererAccel :: GObjectClass obj => obj -> CellRendererAccel
castToCellRendererAccel = castTo gTypeCellRendererAccel "CellRendererAccel"

gTypeCellRendererAccel :: GType
gTypeCellRendererAccel =
  {# call fun unsafe gtk_cell_renderer_accel_get_type #}

-- *********************************************************** CellRendererSpin

{#pointer *GtkCellRendererSpin as CellRendererSpin foreign newtype #} deriving (Eq,Ord)

mkCellRendererSpin = (CellRendererSpin, objectUnrefFromMainloop)
unCellRendererSpin (CellRendererSpin o) = o

class CellRendererTextClass o => CellRendererSpinClass o
toCellRendererSpin :: CellRendererSpinClass o => o -> CellRendererSpin
toCellRendererSpin = unsafeCastGObject . toGObject

instance CellRendererSpinClass CellRendererSpin
instance CellRendererTextClass CellRendererSpin
instance CellRendererClass CellRendererSpin
instance GObjectClass CellRendererSpin where
  toGObject = GObject . castForeignPtr . unCellRendererSpin
  unsafeCastGObject = CellRendererSpin . castForeignPtr . unGObject

castToCellRendererSpin :: GObjectClass obj => obj -> CellRendererSpin
castToCellRendererSpin = castTo gTypeCellRendererSpin "CellRendererSpin"

gTypeCellRendererSpin :: GType
gTypeCellRendererSpin =
  {# call fun unsafe gtk_cell_renderer_spin_get_type #}

-- ********************************************************** CellRendererCombo

{#pointer *GtkCellRendererCombo as CellRendererCombo foreign newtype #} deriving (Eq,Ord)

mkCellRendererCombo = (CellRendererCombo, objectUnrefFromMainloop)
unCellRendererCombo (CellRendererCombo o) = o

class CellRendererTextClass o => CellRendererComboClass o
toCellRendererCombo :: CellRendererComboClass o => o -> CellRendererCombo
toCellRendererCombo = unsafeCastGObject . toGObject

instance CellRendererComboClass CellRendererCombo
instance CellRendererTextClass CellRendererCombo
instance CellRendererClass CellRendererCombo
instance GObjectClass CellRendererCombo where
  toGObject = GObject . castForeignPtr . unCellRendererCombo
  unsafeCastGObject = CellRendererCombo . castForeignPtr . unGObject

castToCellRendererCombo :: GObjectClass obj => obj -> CellRendererCombo
castToCellRendererCombo = castTo gTypeCellRendererCombo "CellRendererCombo"

gTypeCellRendererCombo :: GType
gTypeCellRendererCombo =
  {# call fun unsafe gtk_cell_renderer_combo_get_type #}

-- ********************************************************* CellRendererToggle

{#pointer *GtkCellRendererToggle as CellRendererToggle foreign newtype #} deriving (Eq,Ord)

mkCellRendererToggle = (CellRendererToggle, objectUnrefFromMainloop)
unCellRendererToggle (CellRendererToggle o) = o

class CellRendererClass o => CellRendererToggleClass o
toCellRendererToggle :: CellRendererToggleClass o => o -> CellRendererToggle
toCellRendererToggle = unsafeCastGObject . toGObject

instance CellRendererToggleClass CellRendererToggle
instance CellRendererClass CellRendererToggle
instance GObjectClass CellRendererToggle where
  toGObject = GObject . castForeignPtr . unCellRendererToggle
  unsafeCastGObject = CellRendererToggle . castForeignPtr . unGObject

castToCellRendererToggle :: GObjectClass obj => obj -> CellRendererToggle
castToCellRendererToggle = castTo gTypeCellRendererToggle "CellRendererToggle"

gTypeCellRendererToggle :: GType
gTypeCellRendererToggle =
  {# call fun unsafe gtk_cell_renderer_toggle_get_type #}

-- ******************************************************* CellRendererProgress

{#pointer *GtkCellRendererProgress as CellRendererProgress foreign newtype #} deriving (Eq,Ord)

mkCellRendererProgress = (CellRendererProgress, objectUnrefFromMainloop)
unCellRendererProgress (CellRendererProgress o) = o

class CellRendererClass o => CellRendererProgressClass o
toCellRendererProgress :: CellRendererProgressClass o => o -> CellRendererProgress
toCellRendererProgress = unsafeCastGObject . toGObject

instance CellRendererProgressClass CellRendererProgress
instance CellRendererClass CellRendererProgress
instance GObjectClass CellRendererProgress where
  toGObject = GObject . castForeignPtr . unCellRendererProgress
  unsafeCastGObject = CellRendererProgress . castForeignPtr . unGObject

castToCellRendererProgress :: GObjectClass obj => obj -> CellRendererProgress
castToCellRendererProgress = castTo gTypeCellRendererProgress "CellRendererProgress"

gTypeCellRendererProgress :: GType
gTypeCellRendererProgress =
  {# call fun unsafe gtk_cell_renderer_progress_get_type #}

-- ***************************************************************** FileFilter

{#pointer *GtkFileFilter as FileFilter foreign newtype #} deriving (Eq,Ord)

mkFileFilter = (FileFilter, objectUnrefFromMainloop)
unFileFilter (FileFilter o) = o

class GObjectClass o => FileFilterClass o
toFileFilter :: FileFilterClass o => o -> FileFilter
toFileFilter = unsafeCastGObject . toGObject

instance FileFilterClass FileFilter
instance GObjectClass FileFilter where
  toGObject = GObject . castForeignPtr . unFileFilter
  unsafeCastGObject = FileFilter . castForeignPtr . unGObject

castToFileFilter :: GObjectClass obj => obj -> FileFilter
castToFileFilter = castTo gTypeFileFilter "FileFilter"

gTypeFileFilter :: GType
gTypeFileFilter =
  {# call fun unsafe gtk_file_filter_get_type #}

-- ******************************************************************** Builder

{#pointer *GtkBuilder as Builder foreign newtype #} deriving (Eq,Ord)

mkBuilder = (Builder, objectUnrefFromMainloop)
unBuilder (Builder o) = o

class GObjectClass o => BuilderClass o
toBuilder :: BuilderClass o => o -> Builder
toBuilder = unsafeCastGObject . toGObject

instance BuilderClass Builder
instance GObjectClass Builder where
  toGObject = GObject . castForeignPtr . unBuilder
  unsafeCastGObject = Builder . castForeignPtr . unGObject

castToBuilder :: GObjectClass obj => obj -> Builder
castToBuilder = castTo gTypeBuilder "Builder"

gTypeBuilder :: GType
gTypeBuilder =
  {# call fun unsafe gtk_builder_get_type #}

-- *************************************************************** StyleContext

{#pointer *GtkStyleContext as StyleContext foreign newtype #} deriving (Eq,Ord)

mkStyleContext = (StyleContext, objectUnrefFromMainloop)
unStyleContext (StyleContext o) = o

class GObjectClass o => StyleContextClass o
toStyleContext :: StyleContextClass o => o -> StyleContext
toStyleContext = unsafeCastGObject . toGObject

instance StyleContextClass StyleContext
instance GObjectClass StyleContext where
  toGObject = GObject . castForeignPtr . unStyleContext
  unsafeCastGObject = StyleContext . castForeignPtr . unGObject

castToStyleContext :: GObjectClass obj => obj -> StyleContext
castToStyleContext = castTo gTypeStyleContext "StyleContext"

gTypeStyleContext :: GType
gTypeStyleContext =
  {# call fun unsafe gtk_style_context_get_type #}

-- ************************************************************** StyleProvider

{#pointer *GtkStyleProvider as StyleProvider foreign newtype #} deriving (Eq,Ord)

mkStyleProvider = (StyleProvider, objectUnrefFromMainloop)
unStyleProvider (StyleProvider o) = o

class GObjectClass o => StyleProviderClass o
toStyleProvider :: StyleProviderClass o => o -> StyleProvider
toStyleProvider = unsafeCastGObject . toGObject

instance StyleProviderClass StyleProvider
instance GObjectClass StyleProvider where
  toGObject = GObject . castForeignPtr . unStyleProvider
  unsafeCastGObject = StyleProvider . castForeignPtr . unGObject

castToStyleProvider :: GObjectClass obj => obj -> StyleProvider
castToStyleProvider = castTo gTypeStyleProvider "StyleProvider"

gTypeStyleProvider :: GType
gTypeStyleProvider =
  {# call fun unsafe gtk_style_provider_get_type #}

-- **************************************************************** CssProvider

{#pointer *GtkCssProvider as CssProvider foreign newtype #} deriving (Eq,Ord)

mkCssProvider = (CssProvider, objectUnrefFromMainloop)
unCssProvider (CssProvider o) = o

class GObjectClass o => CssProviderClass o
toCssProvider :: CssProviderClass o => o -> CssProvider
toCssProvider = unsafeCastGObject . toGObject

instance CssProviderClass CssProvider
instance GObjectClass CssProvider where
  toGObject = GObject . castForeignPtr . unCssProvider
  unsafeCastGObject = CssProvider . castForeignPtr . unGObject

castToCssProvider :: GObjectClass obj => obj -> CssProvider
castToCssProvider = castTo gTypeCssProvider "CssProvider"

gTypeCssProvider :: GType
gTypeCssProvider =
  {# call fun unsafe gtk_css_provider_get_type #}

-- ***************************************************************** CellLayout

{#pointer *GtkCellLayout as CellLayout foreign newtype #} deriving (Eq,Ord)

mkCellLayout = (CellLayout, objectUnrefFromMainloop)
unCellLayout (CellLayout o) = o

class GObjectClass o => CellLayoutClass o
toCellLayout :: CellLayoutClass o => o -> CellLayout
toCellLayout = unsafeCastGObject . toGObject

instance CellLayoutClass CellLayout
instance GObjectClass CellLayout where
  toGObject = GObject . castForeignPtr . unCellLayout
  unsafeCastGObject = CellLayout . castForeignPtr . unGObject

castToCellLayout :: GObjectClass obj => obj -> CellLayout
castToCellLayout = castTo gTypeCellLayout "CellLayout"

gTypeCellLayout :: GType
gTypeCellLayout =
  {# call fun unsafe gtk_cell_layout_get_type #}

-- *************************************************************** TreeSortable

{#pointer *GtkTreeSortable as TreeSortable foreign newtype #} deriving (Eq,Ord)

mkTreeSortable = (TreeSortable, objectUnrefFromMainloop)
unTreeSortable (TreeSortable o) = o

class GObjectClass o => TreeSortableClass o
toTreeSortable :: TreeSortableClass o => o -> TreeSortable
toTreeSortable = unsafeCastGObject . toGObject

instance TreeSortableClass TreeSortable
instance GObjectClass TreeSortable where
  toGObject = GObject . castForeignPtr . unTreeSortable
  unsafeCastGObject = TreeSortable . castForeignPtr . unGObject

castToTreeSortable :: GObjectClass obj => obj -> TreeSortable
castToTreeSortable = castTo gTypeTreeSortable "TreeSortable"

gTypeTreeSortable :: GType
gTypeTreeSortable =
  {# call fun unsafe gtk_tree_sortable_get_type #}

-- ******************************************************************** Tooltip

{#pointer *GtkTooltip as Tooltip foreign newtype #} deriving (Eq,Ord)

mkTooltip = (Tooltip, objectUnrefFromMainloop)
unTooltip (Tooltip o) = o

class GObjectClass o => TooltipClass o
toTooltip :: TooltipClass o => o -> Tooltip
toTooltip = unsafeCastGObject . toGObject

instance TooltipClass Tooltip
instance GObjectClass Tooltip where
  toGObject = GObject . castForeignPtr . unTooltip
  unsafeCastGObject = Tooltip . castForeignPtr . unGObject

castToTooltip :: GObjectClass obj => obj -> Tooltip
castToTooltip = castTo gTypeTooltip "Tooltip"

gTypeTooltip :: GType
gTypeTooltip =
  {# call fun unsafe gtk_tooltip_get_type #}

-- ***************************************************************** StatusIcon

{#pointer *GtkStatusIcon as StatusIcon foreign newtype #} deriving (Eq,Ord)

mkStatusIcon = (StatusIcon, objectUnrefFromMainloop)
unStatusIcon (StatusIcon o) = o

class TooltipClass o => StatusIconClass o
toStatusIcon :: StatusIconClass o => o -> StatusIcon
toStatusIcon = unsafeCastGObject . toGObject

instance StatusIconClass StatusIcon
instance TooltipClass StatusIcon
instance GObjectClass StatusIcon where
  toGObject = GObject . castForeignPtr . unStatusIcon
  unsafeCastGObject = StatusIcon . castForeignPtr . unGObject

castToStatusIcon :: GObjectClass obj => obj -> StatusIcon
castToStatusIcon = castTo gTypeStatusIcon "StatusIcon"

gTypeStatusIcon :: GType
gTypeStatusIcon =
  {# call fun unsafe gtk_status_icon_get_type #}

-- ************************************************************** TreeSelection

{#pointer *GtkTreeSelection as TreeSelection foreign newtype #} deriving (Eq,Ord)

mkTreeSelection = (TreeSelection, objectUnrefFromMainloop)
unTreeSelection (TreeSelection o) = o

class GObjectClass o => TreeSelectionClass o
toTreeSelection :: TreeSelectionClass o => o -> TreeSelection
toTreeSelection = unsafeCastGObject . toGObject

instance TreeSelectionClass TreeSelection
instance GObjectClass TreeSelection where
  toGObject = GObject . castForeignPtr . unTreeSelection
  unsafeCastGObject = TreeSelection . castForeignPtr . unGObject

castToTreeSelection :: GObjectClass obj => obj -> TreeSelection
castToTreeSelection = castTo gTypeTreeSelection "TreeSelection"

gTypeTreeSelection :: GType
gTypeTreeSelection =
  {# call fun unsafe gtk_tree_selection_get_type #}

-- ****************************************************************** TreeModel

{#pointer *GtkTreeModel as TreeModel foreign newtype #} deriving (Eq,Ord)

mkTreeModel = (TreeModel, objectUnrefFromMainloop)
unTreeModel (TreeModel o) = o

class GObjectClass o => TreeModelClass o
toTreeModel :: TreeModelClass o => o -> TreeModel
toTreeModel = unsafeCastGObject . toGObject

instance TreeModelClass TreeModel
instance GObjectClass TreeModel where
  toGObject = GObject . castForeignPtr . unTreeModel
  unsafeCastGObject = TreeModel . castForeignPtr . unGObject

castToTreeModel :: GObjectClass obj => obj -> TreeModel
castToTreeModel = castTo gTypeTreeModel "TreeModel"

gTypeTreeModel :: GType
gTypeTreeModel =
  {# call fun unsafe gtk_tree_model_get_type #}

-- ****************************************************************** TreeStore

{#pointer *GtkTreeStore as TreeStore foreign newtype #} deriving (Eq,Ord)

mkTreeStore = (TreeStore, objectUnrefFromMainloop)
unTreeStore (TreeStore o) = o

class TreeModelClass o => TreeStoreClass o
toTreeStore :: TreeStoreClass o => o -> TreeStore
toTreeStore = unsafeCastGObject . toGObject

instance TreeStoreClass TreeStore
instance TreeModelClass TreeStore
instance GObjectClass TreeStore where
  toGObject = GObject . castForeignPtr . unTreeStore
  unsafeCastGObject = TreeStore . castForeignPtr . unGObject

castToTreeStore :: GObjectClass obj => obj -> TreeStore
castToTreeStore = castTo gTypeTreeStore "TreeStore"

gTypeTreeStore :: GType
gTypeTreeStore =
  {# call fun unsafe gtk_tree_store_get_type #}

-- ****************************************************************** ListStore

{#pointer *GtkListStore as ListStore foreign newtype #} deriving (Eq,Ord)

mkListStore = (ListStore, objectUnrefFromMainloop)
unListStore (ListStore o) = o

class TreeModelClass o => ListStoreClass o
toListStore :: ListStoreClass o => o -> ListStore
toListStore = unsafeCastGObject . toGObject

instance ListStoreClass ListStore
instance TreeModelClass ListStore
instance GObjectClass ListStore where
  toGObject = GObject . castForeignPtr . unListStore
  unsafeCastGObject = ListStore . castForeignPtr . unGObject

castToListStore :: GObjectClass obj => obj -> ListStore
castToListStore = castTo gTypeListStore "ListStore"

gTypeListStore :: GType
gTypeListStore =
  {# call fun unsafe gtk_list_store_get_type #}

-- ************************************************************** TreeModelSort

{#pointer *GtkTreeModelSort as TreeModelSort foreign newtype #} deriving (Eq,Ord)

mkTreeModelSort = (TreeModelSort, objectUnrefFromMainloop)
unTreeModelSort (TreeModelSort o) = o

class GObjectClass o => TreeModelSortClass o
toTreeModelSort :: TreeModelSortClass o => o -> TreeModelSort
toTreeModelSort = unsafeCastGObject . toGObject

instance TreeModelSortClass TreeModelSort
instance GObjectClass TreeModelSort where
  toGObject = GObject . castForeignPtr . unTreeModelSort
  unsafeCastGObject = TreeModelSort . castForeignPtr . unGObject

castToTreeModelSort :: GObjectClass obj => obj -> TreeModelSort
castToTreeModelSort = castTo gTypeTreeModelSort "TreeModelSort"

gTypeTreeModelSort :: GType
gTypeTreeModelSort =
  {# call fun unsafe gtk_tree_model_sort_get_type #}

-- ************************************************************ TreeModelFilter

{#pointer *GtkTreeModelFilter as TreeModelFilter foreign newtype #} deriving (Eq,Ord)

mkTreeModelFilter = (TreeModelFilter, objectUnrefFromMainloop)
unTreeModelFilter (TreeModelFilter o) = o

class GObjectClass o => TreeModelFilterClass o
toTreeModelFilter :: TreeModelFilterClass o => o -> TreeModelFilter
toTreeModelFilter = unsafeCastGObject . toGObject

instance TreeModelFilterClass TreeModelFilter
instance GObjectClass TreeModelFilter where
  toGObject = GObject . castForeignPtr . unTreeModelFilter
  unsafeCastGObject = TreeModelFilter . castForeignPtr . unGObject

castToTreeModelFilter :: GObjectClass obj => obj -> TreeModelFilter
castToTreeModelFilter = castTo gTypeTreeModelFilter "TreeModelFilter"

gTypeTreeModelFilter :: GType
gTypeTreeModelFilter =
  {# call fun unsafe gtk_tree_model_filter_get_type #}

-- **************************************************************** IconFactory

{#pointer *GtkIconFactory as IconFactory foreign newtype #} deriving (Eq,Ord)

mkIconFactory = (IconFactory, objectUnrefFromMainloop)
unIconFactory (IconFactory o) = o

class GObjectClass o => IconFactoryClass o
toIconFactory :: IconFactoryClass o => o -> IconFactory
toIconFactory = unsafeCastGObject . toGObject

instance IconFactoryClass IconFactory
instance GObjectClass IconFactory where
  toGObject = GObject . castForeignPtr . unIconFactory
  unsafeCastGObject = IconFactory . castForeignPtr . unGObject

castToIconFactory :: GObjectClass obj => obj -> IconFactory
castToIconFactory = castTo gTypeIconFactory "IconFactory"

gTypeIconFactory :: GType
gTypeIconFactory =
  {# call fun unsafe gtk_icon_factory_get_type #}

-- ****************************************************************** IconTheme

{#pointer *GtkIconTheme as IconTheme foreign newtype #} deriving (Eq,Ord)

mkIconTheme = (IconTheme, objectUnrefFromMainloop)
unIconTheme (IconTheme o) = o

class GObjectClass o => IconThemeClass o
toIconTheme :: IconThemeClass o => o -> IconTheme
toIconTheme = unsafeCastGObject . toGObject

instance IconThemeClass IconTheme
instance GObjectClass IconTheme where
  toGObject = GObject . castForeignPtr . unIconTheme
  unsafeCastGObject = IconTheme . castForeignPtr . unGObject

castToIconTheme :: GObjectClass obj => obj -> IconTheme
castToIconTheme = castTo gTypeIconTheme "IconTheme"

gTypeIconTheme :: GType
gTypeIconTheme =
  {# call fun unsafe gtk_icon_theme_get_type #}

-- ****************************************************************** SizeGroup

{#pointer *GtkSizeGroup as SizeGroup foreign newtype #} deriving (Eq,Ord)

mkSizeGroup = (SizeGroup, objectUnrefFromMainloop)
unSizeGroup (SizeGroup o) = o

class GObjectClass o => SizeGroupClass o
toSizeGroup :: SizeGroupClass o => o -> SizeGroup
toSizeGroup = unsafeCastGObject . toGObject

instance SizeGroupClass SizeGroup
instance GObjectClass SizeGroup where
  toGObject = GObject . castForeignPtr . unSizeGroup
  unsafeCastGObject = SizeGroup . castForeignPtr . unGObject

castToSizeGroup :: GObjectClass obj => obj -> SizeGroup
castToSizeGroup = castTo gTypeSizeGroup "SizeGroup"

gTypeSizeGroup :: GType
gTypeSizeGroup =
  {# call fun unsafe gtk_size_group_get_type #}

-- ****************************************************************** Clipboard

{#pointer *GtkClipboard as Clipboard foreign newtype #} deriving (Eq,Ord)

mkClipboard = (Clipboard, objectUnrefFromMainloop)
unClipboard (Clipboard o) = o

class GObjectClass o => ClipboardClass o
toClipboard :: ClipboardClass o => o -> Clipboard
toClipboard = unsafeCastGObject . toGObject

instance ClipboardClass Clipboard
instance GObjectClass Clipboard where
  toGObject = GObject . castForeignPtr . unClipboard
  unsafeCastGObject = Clipboard . castForeignPtr . unGObject

castToClipboard :: GObjectClass obj => obj -> Clipboard
castToClipboard = castTo gTypeClipboard "Clipboard"

gTypeClipboard :: GType
gTypeClipboard =
  {# call fun unsafe gtk_clipboard_get_type #}

-- ***************************************************************** AccelGroup

{#pointer *GtkAccelGroup as AccelGroup foreign newtype #} deriving (Eq,Ord)

mkAccelGroup = (AccelGroup, objectUnrefFromMainloop)
unAccelGroup (AccelGroup o) = o

class GObjectClass o => AccelGroupClass o
toAccelGroup :: AccelGroupClass o => o -> AccelGroup
toAccelGroup = unsafeCastGObject . toGObject

instance AccelGroupClass AccelGroup
instance GObjectClass AccelGroup where
  toGObject = GObject . castForeignPtr . unAccelGroup
  unsafeCastGObject = AccelGroup . castForeignPtr . unGObject

castToAccelGroup :: GObjectClass obj => obj -> AccelGroup
castToAccelGroup = castTo gTypeAccelGroup "AccelGroup"

gTypeAccelGroup :: GType
gTypeAccelGroup =
  {# call fun unsafe gtk_accel_group_get_type #}

-- ******************************************************************* AccelMap

{#pointer *GtkAccelMap as AccelMap foreign newtype #} deriving (Eq,Ord)

mkAccelMap = (AccelMap, objectUnrefFromMainloop)
unAccelMap (AccelMap o) = o

class GObjectClass o => AccelMapClass o
toAccelMap :: AccelMapClass o => o -> AccelMap
toAccelMap = unsafeCastGObject . toGObject

instance AccelMapClass AccelMap
instance GObjectClass AccelMap where
  toGObject = GObject . castForeignPtr . unAccelMap
  unsafeCastGObject = AccelMap . castForeignPtr . unGObject

castToAccelMap :: GObjectClass obj => obj -> AccelMap
castToAccelMap = castTo gTypeAccelMap "AccelMap"

gTypeAccelMap :: GType
gTypeAccelMap =
  {# call fun unsafe gtk_accel_map_get_type #}

-- ************************************************************ EntryCompletion

{#pointer *GtkEntryCompletion as EntryCompletion foreign newtype #} deriving (Eq,Ord)

mkEntryCompletion = (EntryCompletion, objectUnrefFromMainloop)
unEntryCompletion (EntryCompletion o) = o

class GObjectClass o => EntryCompletionClass o
toEntryCompletion :: EntryCompletionClass o => o -> EntryCompletion
toEntryCompletion = unsafeCastGObject . toGObject

instance EntryCompletionClass EntryCompletion
instance GObjectClass EntryCompletion where
  toGObject = GObject . castForeignPtr . unEntryCompletion
  unsafeCastGObject = EntryCompletion . castForeignPtr . unGObject

castToEntryCompletion :: GObjectClass obj => obj -> EntryCompletion
castToEntryCompletion = castTo gTypeEntryCompletion "EntryCompletion"

gTypeEntryCompletion :: GType
gTypeEntryCompletion =
  {# call fun unsafe gtk_entry_completion_get_type #}

-- **************************************************************** EntryBuffer

{#pointer *GtkEntryBuffer as EntryBuffer foreign newtype #} deriving (Eq,Ord)

mkEntryBuffer = (EntryBuffer, objectUnrefFromMainloop)
unEntryBuffer (EntryBuffer o) = o

class GObjectClass o => EntryBufferClass o
toEntryBuffer :: EntryBufferClass o => o -> EntryBuffer
toEntryBuffer = unsafeCastGObject . toGObject

instance EntryBufferClass EntryBuffer
instance GObjectClass EntryBuffer where
  toGObject = GObject . castForeignPtr . unEntryBuffer
  unsafeCastGObject = EntryBuffer . castForeignPtr . unGObject

castToEntryBuffer :: GObjectClass obj => obj -> EntryBuffer
castToEntryBuffer = castTo gTypeEntryBuffer "EntryBuffer"

gTypeEntryBuffer :: GType
gTypeEntryBuffer =
  {# call fun unsafe gtk_entry_buffer_get_type #}

-- ********************************************************************* Action

{#pointer *GtkAction as Action foreign newtype #} deriving (Eq,Ord)

mkAction = (Action, objectUnrefFromMainloop)
unAction (Action o) = o

class GObjectClass o => ActionClass o
toAction :: ActionClass o => o -> Action
toAction = unsafeCastGObject . toGObject

instance ActionClass Action
instance GObjectClass Action where
  toGObject = GObject . castForeignPtr . unAction
  unsafeCastGObject = Action . castForeignPtr . unGObject

castToAction :: GObjectClass obj => obj -> Action
castToAction = castTo gTypeAction "Action"

gTypeAction :: GType
gTypeAction =
  {# call fun unsafe gtk_action_get_type #}

-- *************************************************************** RecentAction

{#pointer *GtkRecentAction as RecentAction foreign newtype #} deriving (Eq,Ord)

mkRecentAction = (RecentAction, objectUnrefFromMainloop)
unRecentAction (RecentAction o) = o

class ActionClass o => RecentActionClass o
toRecentAction :: RecentActionClass o => o -> RecentAction
toRecentAction = unsafeCastGObject . toGObject

instance RecentActionClass RecentAction
instance ActionClass RecentAction
instance GObjectClass RecentAction where
  toGObject = GObject . castForeignPtr . unRecentAction
  unsafeCastGObject = RecentAction . castForeignPtr . unGObject

castToRecentAction :: GObjectClass obj => obj -> RecentAction
castToRecentAction = castTo gTypeRecentAction "RecentAction"

gTypeRecentAction :: GType
gTypeRecentAction =
  {# call fun unsafe gtk_recent_action_get_type #}

-- *************************************************************** ToggleAction

{#pointer *GtkToggleAction as ToggleAction foreign newtype #} deriving (Eq,Ord)

mkToggleAction = (ToggleAction, objectUnrefFromMainloop)
unToggleAction (ToggleAction o) = o

class ActionClass o => ToggleActionClass o
toToggleAction :: ToggleActionClass o => o -> ToggleAction
toToggleAction = unsafeCastGObject . toGObject

instance ToggleActionClass ToggleAction
instance ActionClass ToggleAction
instance GObjectClass ToggleAction where
  toGObject = GObject . castForeignPtr . unToggleAction
  unsafeCastGObject = ToggleAction . castForeignPtr . unGObject

castToToggleAction :: GObjectClass obj => obj -> ToggleAction
castToToggleAction = castTo gTypeToggleAction "ToggleAction"

gTypeToggleAction :: GType
gTypeToggleAction =
  {# call fun unsafe gtk_toggle_action_get_type #}

-- **************************************************************** RadioAction

{#pointer *GtkRadioAction as RadioAction foreign newtype #} deriving (Eq,Ord)

mkRadioAction = (RadioAction, objectUnrefFromMainloop)
unRadioAction (RadioAction o) = o

class ToggleActionClass o => RadioActionClass o
toRadioAction :: RadioActionClass o => o -> RadioAction
toRadioAction = unsafeCastGObject . toGObject

instance RadioActionClass RadioAction
instance ToggleActionClass RadioAction
instance ActionClass RadioAction
instance GObjectClass RadioAction where
  toGObject = GObject . castForeignPtr . unRadioAction
  unsafeCastGObject = RadioAction . castForeignPtr . unGObject

castToRadioAction :: GObjectClass obj => obj -> RadioAction
castToRadioAction = castTo gTypeRadioAction "RadioAction"

gTypeRadioAction :: GType
gTypeRadioAction =
  {# call fun unsafe gtk_radio_action_get_type #}

-- **************************************************************** ActionGroup

{#pointer *GtkActionGroup as ActionGroup foreign newtype #} deriving (Eq,Ord)

mkActionGroup = (ActionGroup, objectUnrefFromMainloop)
unActionGroup (ActionGroup o) = o

class GObjectClass o => ActionGroupClass o
toActionGroup :: ActionGroupClass o => o -> ActionGroup
toActionGroup = unsafeCastGObject . toGObject

instance ActionGroupClass ActionGroup
instance GObjectClass ActionGroup where
  toGObject = GObject . castForeignPtr . unActionGroup
  unsafeCastGObject = ActionGroup . castForeignPtr . unGObject

castToActionGroup :: GObjectClass obj => obj -> ActionGroup
castToActionGroup = castTo gTypeActionGroup "ActionGroup"

gTypeActionGroup :: GType
gTypeActionGroup =
  {# call fun unsafe gtk_action_group_get_type #}

-- ****************************************************************** UIManager

{#pointer *GtkUIManager as UIManager foreign newtype #} deriving (Eq,Ord)

mkUIManager = (UIManager, objectUnrefFromMainloop)
unUIManager (UIManager o) = o

class GObjectClass o => UIManagerClass o
toUIManager :: UIManagerClass o => o -> UIManager
toUIManager = unsafeCastGObject . toGObject

instance UIManagerClass UIManager
instance GObjectClass UIManager where
  toGObject = GObject . castForeignPtr . unUIManager
  unsafeCastGObject = UIManager . castForeignPtr . unGObject

castToUIManager :: GObjectClass obj => obj -> UIManager
castToUIManager = castTo gTypeUIManager "UIManager"

gTypeUIManager :: GType
gTypeUIManager =
  {# call fun unsafe gtk_ui_manager_get_type #}

-- **************************************************************** WindowGroup

{#pointer *GtkWindowGroup as WindowGroup foreign newtype #} deriving (Eq,Ord)

mkWindowGroup = (WindowGroup, objectUnrefFromMainloop)
unWindowGroup (WindowGroup o) = o

class GObjectClass o => WindowGroupClass o
toWindowGroup :: WindowGroupClass o => o -> WindowGroup
toWindowGroup = unsafeCastGObject . toGObject

instance WindowGroupClass WindowGroup
instance GObjectClass WindowGroup where
  toGObject = GObject . castForeignPtr . unWindowGroup
  unsafeCastGObject = WindowGroup . castForeignPtr . unGObject

castToWindowGroup :: GObjectClass obj => obj -> WindowGroup
castToWindowGroup = castTo gTypeWindowGroup "WindowGroup"

gTypeWindowGroup :: GType
gTypeWindowGroup =
  {# call fun unsafe gtk_window_group_get_type #}

-- *************************************************************** CellEditable

{#pointer *GtkCellEditable as CellEditable foreign newtype #} deriving (Eq,Ord)

mkCellEditable = (CellEditable, objectUnrefFromMainloop)
unCellEditable (CellEditable o) = o

class GObjectClass o => CellEditableClass o
toCellEditable :: CellEditableClass o => o -> CellEditable
toCellEditable = unsafeCastGObject . toGObject

instance CellEditableClass CellEditable
instance GObjectClass CellEditable where
  toGObject = GObject . castForeignPtr . unCellEditable
  unsafeCastGObject = CellEditable . castForeignPtr . unGObject

castToCellEditable :: GObjectClass obj => obj -> CellEditable
castToCellEditable = castTo gTypeCellEditable "CellEditable"

gTypeCellEditable :: GType
gTypeCellEditable =
  {# call fun unsafe gtk_cell_editable_get_type #}

-- ******************************************************************* Editable

{#pointer *GtkEditable as Editable foreign newtype #} deriving (Eq,Ord)

mkEditable = (Editable, objectUnrefFromMainloop)
unEditable (Editable o) = o

class GObjectClass o => EditableClass o
toEditable :: EditableClass o => o -> Editable
toEditable = unsafeCastGObject . toGObject

instance EditableClass Editable
instance GObjectClass Editable where
  toGObject = GObject . castForeignPtr . unEditable
  unsafeCastGObject = Editable . castForeignPtr . unGObject

castToEditable :: GObjectClass obj => obj -> Editable
castToEditable = castTo gTypeEditable "Editable"

gTypeEditable :: GType
gTypeEditable =
  {# call fun unsafe gtk_editable_get_type #}

-- **************************************************************** FileChooser

{#pointer *GtkFileChooser as FileChooser foreign newtype #} deriving (Eq,Ord)

mkFileChooser = (FileChooser, objectUnrefFromMainloop)
unFileChooser (FileChooser o) = o

class GObjectClass o => FileChooserClass o
toFileChooser :: FileChooserClass o => o -> FileChooser
toFileChooser = unsafeCastGObject . toGObject

instance FileChooserClass FileChooser
instance GObjectClass FileChooser where
  toGObject = GObject . castForeignPtr . unFileChooser
  unsafeCastGObject = FileChooser . castForeignPtr . unGObject

castToFileChooser :: GObjectClass obj => obj -> FileChooser
castToFileChooser = castTo gTypeFileChooser "FileChooser"

gTypeFileChooser :: GType
gTypeFileChooser =
  {# call fun unsafe gtk_file_chooser_get_type #}

