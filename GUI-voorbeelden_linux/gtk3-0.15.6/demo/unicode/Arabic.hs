-- Example of an international dialog box.
import Graphics.UI.Gtk

import Control.Applicative
import Prelude
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)

main :: IO ()
main = do
  initGUI
  dia <- dialogNew
  dialogAddButton dia stockYes ResponseYes
  dialogAddButton dia stockNo ResponseNo
  contain <- castToBox <$> dialogGetContentArea dia
  theText <- labelNew (Nothing :: Maybe Text)
  labelSetMarkup theText (T.pack arabic)
  boxPackStart contain theText PackNatural 0
  widgetShowAll dia
  res <- dialogRun dia
  case res of
    ResponseNo -> yell
    _ -> return ()

arabic :: String
arabic = markSpan [FontSize (SizePoint 36)]  $
 --"Is Haskell a "++markSpan [FontForeground "red"] "fantastic"++" language?"++
 -- Do you find Haskell a fantastic language? (language has a grammatical
 -- mistake in it)
  map chr [0x647,0x644,32,0x62A,0x62C,0x62F,0x646,32]++
  markSpan [FontForeground "red"]
    (map chr [0x647,0x622,0x633,0x643,0x622,0x644])++
  map chr [32,0x644,0x63A,0x62A,32,0x645,0x62F,0x647,0x634,0x62A,0x61F]

yell :: IO ()
yell = do
  dia <- dialogNew
  dialogAddButton dia stockOk ResponseOk
  contain <- castToBox <$> dialogGetContentArea dia
  msg <- labelNew (Just "This is not an option.")
  boxPackStart contain msg PackNatural 0
  widgetShow msg
  dialogRun dia
  return ()
