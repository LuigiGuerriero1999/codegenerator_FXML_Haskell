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
  Gtk.set window [Gtk.windowTitle := "demoapplicatie", Gtk.windowResizable := True, Gtk.windowDefaultWidth := 490, Gtk.windowDefaultHeight := 413]
  
  -- Layout 
  layout_1228819714Container  <- Gtk.layoutNew (Nothing::Maybe Adjustment) (Nothing::Maybe Adjustment)
  
  -- Grid 
  grid_682168364 <- Gtk.gridNew
  Gtk.gridSetColumnHomogeneous grid_682168364 True
  Gtk.gridSetRowHomogeneous grid_682168364 True
  
  -- Button 
  button_1320113256 <- Gtk.buttonNewWithLabel "Button"
  button_1320113256Container <- Gtk.boxNew OrientationHorizontal 1
  Gtk.set button_1320113256Container [Gtk.widgetWidthRequest :=62,  Gtk.widgetHeightRequest :=24]
  Gtk.boxPackStart button_1320113256Container button_1320113256 True True 0
   
  --Label 
  label_1709574519 <- Gtk.labelNew (Just "Label")
  
  -- CheckButton 
  checkButton_144161790 <- Gtk.checkButtonNewWithLabel "CheckBox"
  
  -- RadioButton 
  radioButton_555570272 <- Gtk.radioButtonNewWithLabelFromWidget (Nothing::Maybe Gtk.RadioButton) "RadioButton"
  
  --Label 
  tabLabel_2603186 <- Gtk.labelNew (Just "Test")
  
  --Notebook 
  noteBook_1901922038 <- Gtk.notebookNew 
  noteBook_1901922038Container  <- Gtk.boxNew OrientationHorizontal 1
  Gtk.set noteBook_1901922038Container [Gtk.widgetWidthRequest :=200,  Gtk.widgetHeightRequest :=200]
  Gtk.boxPackStart noteBook_1901922038Container noteBook_1901922038 True True 0
   
  -- Layout 
  layout_1483311536Container  <- Gtk.layoutNew (Nothing::Maybe Adjustment) (Nothing::Maybe Adjustment)
  
  -- Entry 
  entry_98110470 <- Gtk.entryNew
  Gtk.entrySetAlignment entry_98110470 0.0 
  entry_98110470Container <- Gtk.boxNew OrientationHorizontal 1
  Gtk.set entry_98110470Container [Gtk.widgetWidthRequest :=171,  Gtk.widgetHeightRequest :=24]
  Gtk.boxPackStart entry_98110470Container entry_98110470 True True 0
   
  -- Relations 
  Gtk.layoutPut layout_1228819714Container grid_682168364 36 112
  Gtk.gridAttach grid_682168364 button_1320113256Container 0 0 1 1 
  Gtk.gridAttach grid_682168364 label_1709574519 0 1 1 1 
  Gtk.layoutPut layout_1228819714Container checkButton_144161790 42 212
  Gtk.layoutPut layout_1228819714Container radioButton_555570272 34 260
  Gtk.layoutPut layout_1228819714Container noteBook_1901922038Container 244 72
  Gtk.layoutPut layout_1483311536Container entry_98110470Container 15 61
  
  --Notebook relations 
  Gtk.notebookAppendPage noteBook_1901922038 layout_1483311536Container (Just tabLabel_2603186)
  
  Gtk.setContainerChild window layout_1228819714Container
  
  Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window
  Gtk.main
  