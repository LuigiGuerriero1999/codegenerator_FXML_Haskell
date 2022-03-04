import Graphics.UI.Gtk

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World from GTK2Hs"]


main :: IO ()
main = do
  initGUI
  window <- windowNew 
  hbox    <- hBoxNew True 10

  --buttons initialiseren
  button1 <- buttonNewWithLabel "Eerste knop"
  button2 <- buttonNewWithLabel "Tweede knop"

  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerBorderWidth := 10, containerChild := hbox]

  --packing boxes, GTK's versie van containers, kan ook met tables gemaakt worden
  boxPackStart hbox button1 PackGrow 0
  boxPackStart hbox button2 PackGrow 0

  --callback functie als er op de button wordt geklikt
  onClicked button1 (hello button1)
  onClicked button2 (hello button2)

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI


