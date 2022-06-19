import Graphics.UI.Gtk

--hulp functie voor tekst te updaten in de label
updateText label text = set label [labelText  := text]

--hulp functie voor de tekst van een Entry te verkrijgen
getEntryText entry = do 
    s <- entryGetText  entry
    if s == ""
    then return ""
    else return s
        
main :: IO ()
main = do
    initGUI
    window <- windowNew
    set window [ windowTitle := "GTK2Hs demo-app", windowResizable := True, 
				 windowDefaultWidth  := 600, windowDefaultHeight := 400 ]

    --initialiseer elementen voor GTKLayout: 1) textLabel, textInput, textLabel
    labelTextType <- labelNew (Just "Typ hier uw tekst:")
    labelTextUpdate <- labelNew (Just "Hier komt de tekst")
    textField <- entryNew

    --Layout = Anchorpane in GTK, initialisere + children toevoegen
    layoutContainer <- layoutNew Nothing Nothing
    layoutPut layoutContainer labelTextType 103 109
    layoutPut layoutContainer textField 75 223
    layoutPut layoutContainer labelTextUpdate 101 353

    --button initialiseren
    buttonShowText <- buttonNewWithLabel "Toon tekst"
    buttonShowText `on` buttonActivated $ do
        x <- getEntryText textField
        updateText labelTextUpdate x
       
    --grid initialiseren en children aan parents toevoegen
    grid <- gridNew                           -- grid (2x1) maken 
    gridSetRowHomogeneous grid True           -- horizontaal even groot
    gridSetColumnHomogeneous grid True        -- verticaal even groot
    let attach x y w h item = gridAttach grid item x y w h -- hulpfunctie gridplaatsing
    attach 0 0 1 1 layoutContainer            -- x y w h -> layoutContainer links
    attach 1 0 1 1 buttonShowText             -- x y w h -> button rechts
    containerAdd window grid                  -- grid als child aan topwindow
   
    widgetShowAll window
    mainGUI