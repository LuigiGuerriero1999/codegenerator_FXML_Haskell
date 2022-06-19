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
  Gtk.set window [Gtk.windowTitle := "gi-gtk demo-app", Gtk.windowResizable := True, Gtk.windowDefaultWidth := 600, Gtk.windowDefaultHeight := 400]
  
  --initialiseer elementen voor GTKLayout: 1) textLabel, textInput, textLabel
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
  grid <- Gtk.gridNew                        -- grid (2x1) maken 
  Gtk.gridSetColumnHomogeneous grid True     -- horizontaal even groot
  Gtk.gridSetRowHomogeneous grid True        -- verticaal even groot
  Gtk.gridAttach grid layoutContainer 0 0 1 1-- x y w h -> layoutContainer links
  Gtk.gridAttach grid buttonShowText 1 0 1 1 -- x y w h -> button rechts 
  Gtk.setContainerChild window grid          -- grid als child aan topwindow
  
  Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window
  Gtk.main