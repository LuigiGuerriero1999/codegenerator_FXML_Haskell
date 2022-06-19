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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QFileDialog (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkStaticMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QDir (c_QDir, fl_Filters)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_WindowFlags)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listenerQString)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QDialog (c_QDialog)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QFileDialog"] $
  [ QtExportClassAndSignals c_QFileDialog signals
  , qtExport e_AcceptMode
  , qtExport e_DialogLabel
  , qtExport e_FileMode
  , qtExport e_Option
  , qtExport fl_Options
  , qtExport e_ViewMode
  ]

(c_QFileDialog, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QFileDialog"] $
  classSetEntityPrefix "" $
  makeClass (ident "QFileDialog") Nothing [c_QDialog] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithParentAndFlags" [ptrT $ objT c_QWidget, flagsT fl_WindowFlags]
  , just $ mkCtor "newWithParentAndCaption" [ptrT $ objT c_QWidget, objT c_QString]
  , just $ mkCtor "newWithParentAndCaptionAndDirectory"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString]
  , just $ mkCtor "newWithParentAndCaptionAndDirectoryAndFilter"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, objT c_QString]
  , just $ mkProp "acceptMode" $ enumT e_AcceptMode
  , just $ mkProp "defaultSuffix" $ objT c_QString
  , just $ mkConstMethod "directory" np $ objT c_QDir
    -- TODO directoryUrl (>=5.2)
  , just $ mkProp "fileMode" $ enumT e_FileMode
  , test (qtVersion >= [4, 4]) $ mkProp "filter" $ flagsT fl_Filters
  , just $ mkStaticMethod' "getExistingDirectory" "getExistingDirectory"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkStaticMethod' "getExistingDirectory" "getExistingDirectoryWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, flagsT fl_Options] $ objT c_QString
    -- TODO getExistingDirectoryUrl (>=5.2)
  , just $ mkStaticMethod' "getOpenFileName" "getOpenFileName"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkStaticMethod' "getOpenFileName" "getOpenFileNameWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     objT c_QString, ptrT $ objT c_QString, flagsT fl_Options] $
    objT c_QString
  , just $ mkStaticMethod' "getOpenFileNames" "getOpenFileNames"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, objT c_QString] $ objT c_QStringList
  , just $ mkStaticMethod' "getOpenFileNames" "getOpenFileNamesWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     objT c_QString, ptrT $ objT c_QString, flagsT fl_Options] $
    objT c_QStringList
    -- TODO getOpenFileUrl (>=5.2)
    -- TODO getOpenFileUrls (>=5.2)
  , just $ mkStaticMethod' "getSaveFileName" "getSaveFileName"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkStaticMethod' "getSaveFileName" "getSaveFileNameWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     objT c_QString, ptrT $ objT c_QString, flagsT fl_Options] $
    objT c_QString
    -- TODO getSaveFileUrl (>=5.2)
  , just $ mkProp "history" $ objT c_QStringList
    -- TODO iconProvider
    -- TODO itemDelegate
  , just $ mkConstMethod "labelText" [enumT e_DialogLabel] $ objT c_QString
  , test (qtVersion >= [5, 2]) $ mkProp "mimeTypeFilters" $ objT c_QStringList
  , test (qtVersion >= [4, 4]) $ mkProp "nameFilters" $ objT c_QStringList
    -- TODO open (>=4.5)
  , test (qtVersion >= [4, 5]) $ mkProp "options" $ flagsT fl_Options
    -- TODO proxyModel
    -- TODO restoreState (>=4.3)
    -- TODO saveState (>=4.3)
  , just $ mkMethod "selectFile" [objT c_QString] voidT
    -- TODO selectMimeTypeFilter (>=5.2)
  , just $ mkMethod "selectNameFilter" [objT c_QString] voidT
    -- TODO selectUrl (>=5.2)
  , just $ mkConstMethod "selectedFiles" np $ objT c_QStringList
  , test (qtVersion >= [4, 4]) $ mkConstMethod "selectedNameFilter" np $ objT c_QString
    -- TODO selectedUrls (>=5.2)
  , just $ mkMethod' "setDirectory" "setDirectory" [objT c_QDir] voidT
  , just $ mkMethod' "setDirectory" "setDirectoryPath" [objT c_QString] voidT
    -- TODO setDirectoryUrl(QUrl) (>=5.2)
    -- TODO setHistory
    -- TODO setIconProvider
    -- TODO setItemDelegate
  , just $ mkMethod "setLabelText" [enumT e_DialogLabel, objT c_QString] voidT
  , test (qtVersion >= [4, 4]) $ mkMethod "setNameFilter" [objT c_QString] voidT
  , test (qtVersion >= [4, 5]) $ mkMethod "setOption" [enumT e_Option, boolT] voidT
    -- TODO setProxyModel (>=4.3)
    -- TODO sidebarUrls (>=4.3)
    -- TODO testOption (>=4.5)
  , just $ mkProp "viewMode" $ enumT e_ViewMode
  ]

signalGens :: [SignalGen]
signalGens =
  collect
  [ just $ makeSignal "currentChanged" listenerQString
    -- TODO currentUrlChanged (>=5.2)
  , just $ makeSignal "directoryEntered" listenerQString
    -- TODO directoryUrlEntered (>=5.2)
  , just $ makeSignal "fileSelected" listenerQString
    -- TODO filesSelected
  , test (qtVersion >= [4, 3]) $ makeSignal "filterSelected" listenerQString
    -- TODO urlSelected (>=5.2)
    -- TODO urlsSelected (>=5.2)
  ]

e_AcceptMode =
  makeQtEnum (ident1 "QFileDialog" "AcceptMode") [includeStd "QFileDialog"]
  [ "AcceptOpen"
  , "AcceptSave"
  ]

e_DialogLabel =
  makeQtEnum (ident1 "QFileDialog" "DialogLabel") [includeStd "QFileDialog"]
  [ "LookIn"
  , "FileName"
  , "FileType"
  , "Accept"
  , "Reject"
  ]

e_FileMode =
  makeQtEnum (ident1 "QFileDialog" "FileMode") [includeStd "QFileDialog"] $
  collect
  [ just "AnyFile"
  , just "ExistingFile"
  , just "Directory"
  , just "ExistingFiles"
  , test (qtVersion < [4, 5]) "DirectoryOnly"
  ]

(e_Option, fl_Options) =
  makeQtEnumAndFlags (ident1 "QFileDialog" "Option") "Options" [includeStd "QFileDialog"]
  [ "ShowDirsOnly"
  , "DontResolveSymlinks"
  , "DontConfirmOverwrite"
  , "DontUseSheet"
  , "DontUseNativeDialog"
  , "ReadOnly"
  , "HideNameFilterDetails"
  , "DontUseCustomDirectoryIcons"
  ]

e_ViewMode =
  makeQtEnum (ident1 "QFileDialog" "ViewMode") [includeStd "QFileDialog"]
  [ "Detail"
  , "List"
  ]
