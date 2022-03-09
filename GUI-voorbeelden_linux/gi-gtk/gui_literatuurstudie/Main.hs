{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds, TypeApplications #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- A simple program to demonstrate Gtk2Hs.
module Main (Main.main) where

import qualified GI.Gtk as Gtk
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Fixed (Centi)
import Control.Concurrent (threadDelay, forkIO)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Control.Monad (unless)

import GI.Gtk.Enums (WindowType(..), Orientation(..))



main :: IO ()
main = do
  Gtk.init Nothing
  window <- Gtk.windowNew WindowTypeToplevel 
  Gtk.setWindowResizable window True
  Gtk.setWindowDefaultWidth window 600
  Gtk.setWindowDefaultHeight window 400
  Gtk.setWindowTitle window "gi-gtk demo-app"

  --adjustment <- Gtk.adjustmentNew 1 1 1 1 1 1
  --layoutContainer <- Gtk.layoutNew Nothing Nothing :: (IO Gtk.Layout)

  --We hebben 3 items die verticaal onder elkaar gelegen staan, dus we maken een vbox aan
  vbox <- Gtk.boxNew OrientationVertical 1

  --initialiseer de elementen die in de vbox moeten komen: 1) textLabel, textInput, textLabel
  labelTextType <- Gtk.labelNew (Just "Typ hier uw tekst:")
  labelTextUpdate <- Gtk.labelNew (Just "Hier komt de tekst")
  textField <- Gtk.entryNew
  
  --button initialiseren
  buttonShowText <- Gtk.buttonNew
  Gtk.setButtonLabel buttonShowText "Toon Tekst"
  Gtk.onButtonClicked buttonShowText $ do
    x <- Gtk.getEntryText textField
    Gtk.labelSetText labelTextUpdate x

  --packing boxes voor 3 widgets verticaal onder elkaar, kan ook met tables gemaakt worden
  Gtk.boxPackStart vbox labelTextType True True 1
  Gtk.boxPackStart vbox textField True True 1
  Gtk.boxPackStart vbox labelTextUpdate True True 1
    
  --grid initialiseren en children aan parents toevoegen
  grid <- Gtk.gridNew                                       -- grid (2x1) maken waar links de packing boxes komen met de 3 widgets en rechts 1 button
  Gtk.gridSetColumnHomogeneous grid True                    -- grid even groot maken horizontaal
  Gtk.gridSetRowHomogeneous grid True                       -- grid even groot maken verticaal
  Gtk.gridAttach grid vbox 0 0 1 1                          -- x y w h -> vbox toevoegen
  Gtk.gridAttach grid buttonShowText 1 0 1 1                -- x y w h -> button rechts in de grid plaatsen
  Gtk.setContainerChild window grid                         -- grid als child toevoegen aan de window
  
  Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window
  Gtk.main
