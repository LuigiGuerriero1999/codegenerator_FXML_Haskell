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
  Gtk.set window [Gtk.windowTitle := "JavaFX demo-app", Gtk.windowResizable := True, Gtk.windowDefaultWidth := 729, Gtk.windowDefaultHeight := 422]
  
  -- Layout 
  layout_74813728Container  <- Gtk.layoutNew (Nothing::Maybe Adjustment) (Nothing::Maybe Adjustment)
  
  -- LinkButton 
  linkButton_753302649 <- Gtk.linkButtonNewWithLabel "" (Just "Test")
  
  --Label 
  label_2102870097 <- Gtk.labelNew (Just "Deze link brengt je naar de UHasselt beginpagina")
  
  -- Relations 
  Gtk.layoutPut layout_74813728Container linkButton_753302649 165 172
  Gtk.layoutPut layout_74813728Container label_2102870097 78 122
  
  Gtk.setContainerChild window layout_74813728Container
  
  Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window
  Gtk.main
  