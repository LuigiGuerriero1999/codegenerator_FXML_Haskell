{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations

--functie voor het togglen als op de Button wordt gedrukt
buttonCb :: Ref Button -> IO ()            
buttonCb b' = do
  l' <- getLabel b'
  if (l' == "Hello world")
    then setLabel b' "Goodbye world"
    else setLabel b' "Hello world"

ui :: IO ()
ui = do
 window <- windowNew (Size (Width 600) (Height 400)) Nothing (Just "FLTKHS demo app")         
 begin window

 --initialiseer de elementen die in de Layout moeten komen: 1) textLabel, textInput, textLabel
 labelTextType <- boxNewWithBoxtype NoBox (toRectangle $ (103, 109, 100, 24)) "Typ hier uw tekst"
 labelTextUpdate <- boxNewWithBoxtype NoBox (toRectangle $ (101, 353, 100, 24)) "Hier komt de tekst"
 textField <- inputNew (toRectangle $ (75, 223, 171, 24)) (Nothing) (Just FlNormalInput)

 --button initialiseren
 b' <- buttonNew (Rectangle (Position (X 300) (Y 0)) (Size (Width 300) (Height 400))) (Just "Toon Tekst")     
 setLabelsize b' (FontSize 13)
 setCallback b' buttonCb  --callback van de button initialiseren

 end window
 showWidget window

main :: IO ()
main = ui >> FL.run >> FL.flush 

replMain :: IO ()
replMain = ui >> FL.replRun


