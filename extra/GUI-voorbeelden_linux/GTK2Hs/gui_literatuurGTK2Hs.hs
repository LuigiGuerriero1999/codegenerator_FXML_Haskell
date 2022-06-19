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
    set window [ windowTitle         := "GTK2Hs demo-app"
                , windowResizable    := True
                , windowDefaultWidth  := 600
                , windowDefaultHeight := 400 ]

    --We hebben 3 items die verticaal onder elkaar gelegen staan, dus we maken een vbox aan
    vbox    <- vBoxNew True 1

    --initialiseer de elementen die in de vbox moeten komen: 1) textLabel, textInput, textLabel
    labelTextType <- labelNew (Just "Typ hier uw tekst:")
    labelTextUpdate <- labelNew (Just "Hier komt de tekst")
    textField <- entryNew

    --button initialiseren
    buttonShowText <- buttonNewWithLabel "Toon tekst"
    buttonShowText `on` buttonActivated $ do
        x <- getEntryText textField
        updateText labelTextUpdate x
       
    --packing boxes voor 3 widgets verticaal onder elkaar, kan ook met tables gemaakt worden
    boxPackStart vbox labelTextType PackGrow 0
    boxPackStart vbox textField PackGrow 0
    boxPackStart vbox labelTextUpdate PackGrow 0
    
    --grid initialiseren en children aan parents toevoegen
    grid <- gridNew                                         -- grid (2x1) maken waar links de packing boxes komen met de 3 widgets en rechts 1 button
    gridSetRowHomogeneous grid True                         -- grid even groot maken horizontaal
    gridSetColumnHomogeneous grid True                      -- grid even groot maken verticaal
    let attach x y w h item = gridAttach grid item x y w h  -- hulpfunctie voor iets in een grid te plaatsen
    attach 0 0 1 1 vbox                                     -- de vbox met 3 widgets onder elkaar links in de grid plaatsen
    attach 1 0 1 1 buttonShowText                           -- de button rechts in de grid plaatsen
    containerAdd window grid                                -- grid als child toevoegen aan de window
   
    widgetShowAll window
    mainGUI

