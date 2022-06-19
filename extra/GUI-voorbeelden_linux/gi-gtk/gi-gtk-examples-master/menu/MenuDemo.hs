{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GI.Gtk
       (widgetShowAll, onWidgetDestroy, setContainerChild, setWindowTitle,
        windowNew, mainQuit, menuItemNewWithLabel, menuItemNewWithMnemonic,
        onMenuItemActivate, menuShellAppend, menuItemSetSubmenu, menuNew,
        menuBarNew)
import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk.Enums (WindowType(..))
import qualified Data.Text as T (unpack)

{-
  widgets that go into making a menubar and submenus:
  * menu item (what the user wants to select)
  * menu      (acts as a container for the menu items)
  * menubar   (container for each of the individual menus)
  menuitem widgets are used for two different things:
  * they are packed into the menu
  * they are packed into the menubar, which, when selected, activates the menu
  Functions:
  * menuBarNew
    creates a new menubar, which can be packed into a container like a
    window or a box
  * menuNew
    creates a new menu, which is never actually shown; it is just a
    container for the menu items
  * menuItemNew, menuItemNewWithLabel, menuItemMenuWithMnemonic
    create the menu items that are to be displayed; they are actually
    buttons with associated actions
  Once a menu item has been created, it should be put into a menu with
  the menuShellAppend function.
  In order to capture when the item is selected by the user, the
  activate signal need to be connected in the usual way.
-}

createMenuBar descr
    = do bar <- menuBarNew
         mapM_ (createMenu bar) descr
         return bar
    where
      createMenu bar (name,items)
          = do menu <- menuNew
               item <- menuItemNewWithLabelOrMnemonic name
               menuItemSetSubmenu item (Just menu)
               menuShellAppend bar item
               mapM_ (createMenuItem menu) items
      createMenuItem menu (name,action)
          = do item <- menuItemNewWithLabelOrMnemonic name
               menuShellAppend menu item
               case action of
                 Just act -> onMenuItemActivate item act
                 Nothing  -> onMenuItemActivate item (return ())
      menuItemNewWithLabelOrMnemonic name
          | '_' `elem` T.unpack name = menuItemNewWithMnemonic name
          | otherwise                = menuItemNewWithLabel name

menuBarDescr
    = [ ("_File", [ ("Open", Nothing)
                  , ("Save", Nothing)
                  , ("_Quit", Just mainQuit)
                  ]
        )
      , ("Help",  [ ("_Help", Nothing)
                  ]
        )
      ]

main =
    do Gtk.init Nothing
       window <- windowNew WindowTypeToplevel
       menuBar <- createMenuBar menuBarDescr
       setWindowTitle window "Demo"
       setContainerChild window menuBar
       onWidgetDestroy window mainQuit
       widgetShowAll window
       Gtk.main
