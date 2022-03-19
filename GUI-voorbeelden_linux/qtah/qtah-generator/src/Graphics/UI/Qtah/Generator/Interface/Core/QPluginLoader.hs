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

module Graphics.UI.Qtah.Generator.Interface.Core.QPluginLoader (
  aModule,
  c_QPluginLoader,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkStaticMethod,
  mkCtor,
  mkMethod',
  mkMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QLibrary (fl_LoadHints)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
--import Graphics.UI.Qtah.Generator.Interface.Core.QJsonObject (c_QJsonObject)
--import Graphics.UI.Qtah.Generator.Interface.Core.QStaticPlugin (c_QStaticPlugin)
--import Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorQStaticPlugin)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQObject)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QPluginLoader"] $
  [qtExport c_QPluginLoader]

c_QPluginLoader =
  addReqIncludes [ includeStd "QPluginLoader" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QPluginLoader") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkCtor "newWithFilename" [refT $ constT $ objT c_QString]
  , just $ mkCtor "newWithFilenameAndParent" [refT $ constT $ objT c_QString, ptrT $ objT c_QObject]
  , test (qtVersion >= [4, 2]) $ mkConstMethod "errorString" np $ objT c_QString
  , just $ mkMethod' "instance" "getInstance" np $ ptrT $ objT c_QObject
  , just $ mkConstMethod "isLoaded" np $ boolT
  , just $ mkMethod "load" np boolT
  --, just $ mkConstMethod "metaData" np $ objT c_QJsonObject
  , just $ mkStaticMethod "staticInstances" np $ objT c_QListQObject
  --, just $ mkStaticMethod "staticPlugins" np $ objT c_QVectorQStaticPlugin
  , just $ mkMethod "unload" np boolT
  , just $ mkProp "fileName" $ objT c_QString
  , test (qtVersion >= [4, 4]) $ mkProp "loadHints" $ flagsT fl_LoadHints
  ]
