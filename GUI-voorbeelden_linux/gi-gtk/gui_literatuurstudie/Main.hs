{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}


module Main (Main.main) where

import qualified GI.Gtk as Gtk
import GI.Gtk.Enums (WindowType(..), Orientation(..))
import GI.Gtk (Adjustment(Adjustment))


main :: IO ()
main = do
  Gtk.init Nothing
  window <- Gtk.windowNew WindowTypeToplevel 
  Gtk.setWindowResizable window True
  Gtk.setWindowDefaultWidth window 600
  Gtk.setWindowDefaultHeight window 400
  Gtk.setWindowTitle window "gi-gtk demo-app"
  
  --initialiseer de elementen die in de vbox moeten komen: 1) textLabel, textInput, textLabel
  labelTextType <- Gtk.labelNew (Just "Typ hier uw tekst:")
  labelTextUpdate <- Gtk.labelNew (Just "Hier komt de tekst")
  textField <- Gtk.entryNew

  --Layout = Anchorpane in GTK, initialisere + children toevoegen
  layoutContainer <- Gtk.layoutNew (Nothing::Maybe Adjustment) (Nothing::Maybe Adjustment)
  Gtk.layoutPut layoutContainer labelTextType 103 109
  Gtk.layoutPut layoutContainer textField 75 223
  Gtk.layoutPut layoutContainer labelTextUpdate 101 353 

  --button initialiseren
  buttonShowText <- Gtk.buttonNew
  Gtk.setButtonLabel buttonShowText "Toon Tekst"
  Gtk.onButtonClicked buttonShowText $ do
    x <- Gtk.getEntryText textField
    Gtk.labelSetText labelTextUpdate x
    
  --grid initialiseren en children aan parents toevoegen
  grid <- Gtk.gridNew                                       -- grid (2x1) maken waar links de packing boxes komen met de 3 widgets en rechts 1 button
  Gtk.gridSetColumnHomogeneous grid True                    -- grid even groot maken horizontaal
  Gtk.gridSetRowHomogeneous grid True                       -- grid even groot maken verticaal
  Gtk.gridAttach grid layoutContainer 0 0 1 1               -- x y w h -> layoutContainer toevoegen
  Gtk.gridAttach grid buttonShowText 1 0 1 1                -- x y w h -> button rechts in de grid plaatsen
  Gtk.setContainerChild window grid                         -- grid als child toevoegen aan de window
  
  Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window
  Gtk.main
 