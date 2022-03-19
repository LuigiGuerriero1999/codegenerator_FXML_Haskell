-- This file is part of Qtah.
--
-- Copyright 2015-2021 The Qtah Authors.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Graphics.UI.Qtah.Generator.Interface.Gui.QClipboard (
  aModule,
  c_QClipboard,
  e_Mode,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetDtorPrivate,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Gui.QImage (c_QImage)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerQClipboardMode,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QClipboard"] $
  [ QtExportClassAndSignals c_QClipboard signals
  , qtExport e_Mode
  ]

(c_QClipboard, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QClipboard"] $
  classSetDtorPrivate $
  classSetEntityPrefix "" $
  makeClass (ident "QClipboard") Nothing [c_QObject] $
  collect
  [ just $ mkMethod' "clear" "clear" np voidT
  , just $ mkMethod' "clear" "clearWithMode" [enumT e_Mode] voidT
  , just $ mkConstMethod' "image" "image" np $ objT c_QImage
  , just $ mkConstMethod' "image" "imageAll" [enumT e_Mode] $ objT c_QImage
    -- TODO mimeData
  , just $ mkConstMethod "ownsClipboard" np boolT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "ownsFindBuffer" np boolT
  , just $ mkConstMethod "ownsSelection" np boolT
  , just $ mkConstMethod' "pixmap" "pixmap" np $ objT c_QPixmap
  , just $ mkConstMethod' "pixmap" "pixmapAll" [enumT e_Mode] $ objT c_QPixmap
  , just $ mkMethod' "setImage" "setImage" [objT c_QImage] voidT
  , just $ mkMethod' "setImage" "setImageAll" [objT c_QImage, enumT e_Mode] voidT
    -- TODO setMimeData
  , just $ mkMethod' "setPixmap" "setPixmap" [objT c_QPixmap] voidT
  , just $ mkMethod' "setPixmap" "setPixmapAll" [objT c_QPixmap, enumT e_Mode] voidT
  , just $ mkMethod' "setText" "setText" [objT c_QString] voidT
  , just $ mkMethod' "setText" "setTextWithMode" [objT c_QString, enumT e_Mode] voidT
  , just $ mkConstMethod "supportsFindBuffer" np boolT
  , just $ mkConstMethod "supportsSelection" np boolT
  , just $ mkConstMethod' "text" "text" np $ objT c_QString
  , just $ mkConstMethod' "text" "textWithMode" [enumT e_Mode] $ objT c_QString
  , just $ mkConstMethod' "text" "textSubtype" [refT $ objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "text" "textSubtypeWithMode" [refT $ objT c_QString, enumT e_Mode] $
    objT c_QString
  ]

signalGens :: [SignalGen]
signalGens =
  collect
  [ test (qtVersion >= [4, 2]) $ makeSignal "changed" listenerQClipboardMode
  , just $ makeSignal "dataChanged" listener
  , test (qtVersion >= [4, 2]) $ makeSignal "findBufferChanged" listener
  , just $ makeSignal "selectionChanged" listener
  ]

e_Mode =
  makeQtEnum (ident1 "QClipboard" "Mode") [includeStd "QClipboard"]
  [ "Clipboard"
  , "Selection"
  , "FindBuffer"
  ]
