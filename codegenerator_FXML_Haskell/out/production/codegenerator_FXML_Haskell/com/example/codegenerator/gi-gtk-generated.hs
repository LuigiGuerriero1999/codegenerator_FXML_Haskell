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
  Gtk.set window [Gtk.windowTitle := "JavaFX demo app", Gtk.windowResizable := True, Gtk.windowDefaultWidth := 600, Gtk.windowDefaultHeight := 400]
  
  buttonShowText <- Gtk.buttonNew
  Gtk.set buttonShowText [Gtk.buttonLabel :="Test"]
  buttonShowTextContainerBox <- Gtk.boxNew OrientationHorizontal 1
  Gtk.set buttonShowTextContainerBox [Gtk.widgetWidthRequest := 141, Gtk.widgetHeightRequest := 78]
  Gtk.boxPackStart buttonShowTextContainerBox buttonShowText True True 0
   
  layoutContainer <- Gtk.layoutNew (Nothing::Maybe Adjustment) (Nothing::Maybe Adjustment)
  Gtk.layoutPut layoutContainer buttonShowTextContainerBox 103 109

  
  Gtk.setContainerChild window layoutContainer 
  
  Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window
  Gtk.main
  