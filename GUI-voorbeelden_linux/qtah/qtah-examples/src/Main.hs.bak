{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QSplitter as QSplitter
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import Graphics.UI.Qtah.Signal (connect_)
import System.Environment (getArgs)

-- UI type om later terug te geven (met de juiste attributen)
data UI = UI 
  { uiWindow :: QWidget.QWidget
  , uiLabel2 :: QLabel.QLabel
  , uiTextEdit :: IO String } 

main :: IO ()
main = withScopedPtr (getArgs >>= QApplication.new) $ \_ -> do
  ui <- newWindow
  QWidget.show $ uiWindow ui
  QCoreApplication.exec

-- functie om een window aan te maken 
newWindow = do 
  window <- QWidget.new
  QWidget.setWindowTitle window "Qtah Demo App"
  QWidget.setFixedSizeRaw window 600 400

  -- linkerkant aanmaken
  label1 <- QLabel.newWithText "Typ hier uw tekst:"   -- eerste label (bovenaan) aanmaken
  textEdit <- QTextEdit.new                           -- textedit (in het midden) aanmaken
  QWidget.setFixedSizeRaw textEdit 150 30
  label2 <- QLabel.newWithText "Hier komt de tekst"   -- tweede label (onderaan) aanmaken
  linkerkant <- QWidget.new
  QWidget.setFixedSizeRaw linkerkant 300 300
  linkerkantLayout <- QVBoxLayout.new                 -- een layout (anchorpane) aanmaken van de linkerkant
  QWidget.setLayout linkerkant linkerkantLayout
  -- alle widgets (labels/textedits) meegeven aan de layout van de linkerkant
  QBoxLayout.addWidget linkerkantLayout label1
  QBoxLayout.addWidget linkerkantLayout textEdit
  QBoxLayout.addWidget linkerkantLayout label2

  -- rechterkant aanmaken
  button <- QPushButton.newWithText "Toon tekst"      -- button aanmaken
  QWidget.setFixedSizeRaw button 250 250
  rechterkant <- QWidget.new
  QWidget.setFixedSizeRaw rechterkant 300 300
  rechterkantLayout <- QVBoxLayout.new                -- een layout (anchorpane) aanmaken van de rechterkant
  QWidget.setLayout rechterkant rechterkantLayout
  QBoxLayout.addWidget rechterkantLayout button       -- de button meegeven aan de layout van de rechterkant

  -- splitter aanmaken die een widget (window in dit geval) in twee delen verdeeld (linkerkant en rechterkant)
  splitter <- QSplitter.new
  QSplitter.addWidget splitter linkerkant
  QSplitter.addWidget splitter rechterkant

  layout <- QVBoxLayout.newWithParent window  -- globale layout (anchorpane) met als parent de window en waar de splitter wordt meegegeven
  QBoxLayout.addWidget layout splitter

  -- ui definiëren met de window, de tweede label (omdat de tekst hiervan moet veranderen) en de tekst van de textedit
  let ui = UI { uiWindow = window
              , uiLabel2 = label2
              , uiTextEdit = QTextEdit.toPlainText textEdit}

  connect_ button QAbstractButton.clickedSignal $ \_ -> toonTekst ui  -- een signaal koppelen aan de button om een actie (toontekst) uit te voeren

  return ui -- de ui terug geven

toonTekst :: UI -> IO ()  -- functie om de tekst in label2 te veranderen naar de tekst van de textedit
toonTekst ui = do
  tekst <- uiTextEdit ui
  QLabel.setText (uiLabel2 ui) tekst
  