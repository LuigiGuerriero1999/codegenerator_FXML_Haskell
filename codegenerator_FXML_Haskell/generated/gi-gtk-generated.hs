{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main (Main.main) where
import qualified GI.Gtk as Gtk
import GI.Gtk.Enums (WindowType(..), Orientation(..))
import GI.Gtk (Adjustment(Adjustment))
import Data.GI.Base ( AttrOp((:=)) )

main :: IO ()
main = do
  Gtk.init Nothing
  window <- Gtk.windowNew WindowTypeToplevel
  Gtk.set window [Gtk.windowTitle := "demoapplicatie", Gtk.windowResizable := True, Gtk.windowDefaultWidth := 448, Gtk.windowDefaultHeight := 369]
  
  -- Layout 
  layout_660128918Container  <- Gtk.layoutNew (Nothing::Maybe Adjustment) (Nothing::Maybe Adjustment)
  
  -- Grid 
  grid_1082618500 <- Gtk.gridNew
  Gtk.gridSetColumnHomogeneous grid_1082618500 True
  Gtk.gridSetRowHomogeneous grid_1082618500 True
  
  -- Button 
  button_1865166626 <- Gtk.buttonNewWithLabel "Button"
  button_1865166626Container <- Gtk.boxNew OrientationHorizontal 1
  Gtk.set button_1865166626Container [Gtk.widgetWidthRequest :=62,  Gtk.widgetHeightRequest :=24]
  Gtk.boxPackStart button_1865166626Container button_1865166626 True True 0
   
  --Label 
  label_1723283468 <- Gtk.labelNew (Just "Label")
  
  -- CheckButton 
  checkButton_1944543519 <- Gtk.checkButtonNewWithLabel "CheckBox"
  
  -- RadioButton 
  radioButton_1261187988 <- Gtk.radioButtonNewWithLabelFromWidget (Nothing::Maybe Gtk.RadioButton) "RadioButton"
  
  --Label 
  tabLabel_877169331 <- Gtk.labelNew (Just "testje")
  
  --Notebook 
  noteBook_8184785 <- Gtk.notebookNew 
  noteBook_8184785Container  <- Gtk.boxNew OrientationHorizontal 1
  Gtk.set noteBook_8184785Container [Gtk.widgetWidthRequest :=200,  Gtk.widgetHeightRequest :=200]
  Gtk.boxPackStart noteBook_8184785Container noteBook_8184785 True True 0
   
  -- Layout 
  layout_913538985Container  <- Gtk.layoutNew (Nothing::Maybe Adjustment) (Nothing::Maybe Adjustment)
  
  -- Entry 
  entry_1275703739 <- Gtk.entryNew
  Gtk.entrySetAlignment entry_1275703739 0.0 
  entry_1275703739Container <- Gtk.boxNew OrientationHorizontal 1
  Gtk.set entry_1275703739Container [Gtk.widgetWidthRequest :=171,  Gtk.widgetHeightRequest :=24]
  Gtk.boxPackStart entry_1275703739Container entry_1275703739 True True 0
   
  -- Relations 
  Gtk.layoutPut layout_660128918Container grid_1082618500 52 95
  Gtk.gridAttach grid_1082618500 button_1865166626Container 0 0 1 1 
  Gtk.gridAttach grid_1082618500 label_1723283468 0 1 1 1 
  Gtk.layoutPut layout_660128918Container checkButton_1944543519 44 223
  Gtk.layoutPut layout_660128918Container radioButton_1261187988 28 272
  Gtk.layoutPut layout_660128918Container noteBook_8184785Container 199 81
  Gtk.layoutPut layout_913538985Container entry_1275703739Container 0 49
  
  --Notebook relations 
  Gtk.notebookAppendPage noteBook_8184785 layout_913538985Container (Just tabLabel_877169331)
  
  Gtk.setContainerChild window layout_660128918Container
  
  Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window
  Gtk.main
  