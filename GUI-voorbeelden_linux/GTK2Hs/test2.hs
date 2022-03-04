module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder


main = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "testboy.glade"
    mainWindow <- builderGetObject builder castToWindow "window1"
    onDestroy mainWindow mainQuit
    widgetShowAll mainWindow
    mainGUI