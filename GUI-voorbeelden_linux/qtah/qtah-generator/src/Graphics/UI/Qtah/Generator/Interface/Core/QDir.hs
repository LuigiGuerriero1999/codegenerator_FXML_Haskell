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

module Graphics.UI.Qtah.Generator.Interface.Core.QDir (
  aModule,
  c_QDir,
  e_Filter,
  fl_Filters,
  e_SortFlag,
  fl_SortFlags,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Operator (OpArray),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkProp,
  mkStaticMethod,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QDir"]
  [ qtExport c_QDir
  , qtExport e_Filter
  , qtExport fl_Filters
  , qtExport e_SortFlag
  , qtExport fl_SortFlags
  ]

c_QDir =
  addReqIncludes [includeStd "QDir"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QDir") Nothing [] $
  collect
  [ just $ mkCtor "new" [objT c_QString]
  , just $ mkConstMethod "absoluteFilePath" [objT c_QString] $ objT c_QString
  , just $ mkConstMethod "absolutePath" np $ objT c_QString
  , test (qtVersion >= [4, 3]) $
    mkStaticMethod "addSearchPath" [objT c_QString, objT c_QString] voidT
  , just $ mkConstMethod "canonicalPath" np $ objT c_QString
  , just $ mkMethod "cd" [objT c_QString] boolT
  , just $ mkMethod "cdUp" np boolT
  , just $ mkStaticMethod "cleanPath" [objT c_QString] $ objT c_QString
  , just $ mkConstMethod "count" np intT
  , just $ mkStaticMethod "current" np $ objT c_QDir
  , just $ mkStaticMethod "currentPath" np $ objT c_QString
  , just $ mkConstMethod "dirName" np $ objT c_QString
    -- TODO drives
    -- TODO entryInfoList
    -- TODO entryList
  , just $ mkConstMethod' "exists" "exists" np boolT
  , just $ mkConstMethod' "exists" "entryExists" [objT c_QString] boolT
  , just $ mkConstMethod "filePath" [objT c_QString] $ objT c_QString
  , just $ mkProp "filter" $ flagsT fl_Filters
  , test (qtVersion >= [4, 2]) $
    mkStaticMethod "fromNativeSeparators" [objT c_QString] $ objT c_QString
  , just $ mkStaticMethod "home" np $ objT c_QDir
  , just $ mkStaticMethod "homePath" np $ objT c_QString
  , just $ mkConstMethod "isAbsolute" np boolT
  , just $ mkStaticMethod "isAbsolutePath" [objT c_QString] boolT
  , just $ mkConstMethod "isReadable" np boolT
  , just $ mkConstMethod "isRelative" np boolT
  , just $ mkStaticMethod "isRelativePath" [objT c_QString] boolT
  , just $ mkConstMethod "isRoot" np boolT
  , just $ mkMethod "makeAbsolute" np boolT
  , just $ mkStaticMethod "match" [objT c_QString, objT c_QString] boolT
    -- TODO match(QStringList, QString)
  , just $ mkConstMethod "mkdir" [objT c_QString] boolT
  , just $ mkConstMethod "mkpath" [objT c_QString] boolT
    -- TODO nameFilters
  , just $ mkProp "path" $ objT c_QString
  , just $ mkMethod "refresh" np voidT
  , just $ mkConstMethod "relativeFilePath" [objT c_QString] $ objT c_QString
  , just $ mkMethod "remove" [objT c_QString] boolT
  , test (qtVersion >= [5, 0]) $ mkMethod "removeRecursively" np boolT
  , just $ mkMethod "rename" [objT c_QString, objT c_QString] boolT
  , just $ mkConstMethod "rmdir" [objT c_QString] boolT
  , just $ mkConstMethod "rmpath" [objT c_QString] boolT
  , just $ mkStaticMethod "root" np $ objT c_QDir
  , just $ mkStaticMethod "rootPath" np $ objT c_QString
    -- TODO searchPaths (>=4.3)
  , just $ mkStaticMethod "separator" np $ objT c_QChar
  , just $ mkStaticMethod "setCurrent" [objT c_QString] boolT
  , just $ mkProp "sorting" $ flagsT fl_SortFlags
  , test (qtVersion >= [5, 0]) $ mkMethod "swap" [refT $ objT c_QDir] voidT
  , just $ mkStaticMethod "temp" np $ objT c_QDir
  , just $ mkStaticMethod "tempPath" np $ objT c_QString
  , test (qtVersion >= [4, 2]) $
    mkStaticMethod "toNativeSeparators" [objT c_QString] $ objT c_QString
  , just $ mkConstMethod OpArray [intT] $ objT c_QString
  ]

(e_Filter, fl_Filters) =
  makeQtEnumAndFlags (ident1 "QDir" "Filter") "Filters" [includeStd "QDir"] $
  [ "Dirs"
  , "AllDirs"
  , "Files"
  , "Drives"
  , "NoSymLinks"
  , "NoDotAndDotDot"
  , "NoDot"
  , "NoDotDot"
  , "AllEntries"
  , "Readable"
  , "Writable"
  , "Executable"
  , "Modified"
  , "Hidden"
  , "System"
  , "CaseSensitive"
  ]

(e_SortFlag, fl_SortFlags) =
  makeQtEnumAndFlags (ident1 "QDir" "SortFlag") "SortFlags" [includeStd "QDir"]
  [ "Name"
  , "Time"
  , "Size"
  , "Type"
  , "Unsorted"
  , "NoSort"
  , "DirsFirst"
  , "DirsLast"
  , "Reversed"
  , "IgnoreCase"
  , "LocaleAware"
  ]
