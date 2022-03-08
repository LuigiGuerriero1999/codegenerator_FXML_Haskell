module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder


main = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "gtk_gui.glade"
    mainWindow <- builderGetObject builder castToWindow "window"
   
    widgetShowAll mainWindow
    mainGUI