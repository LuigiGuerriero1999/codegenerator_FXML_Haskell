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

module Graphics.UI.Qtah.Generator.Interface.Core.QMimeData (
  aModule,
  c_QMimeData,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkProp,
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, objT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QMimeData"] $
  [qtExport c_QMimeData]

c_QMimeData =
  addReqIncludes [ includeStd "QMimeData" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QMimeData") Nothing [c_QObject]
  [ mkCtor "new" np
  , mkMethod "clear" np voidT
  , mkProp "colorData" $ objT c_QVariant
  , mkConstMethod' "data" "getData" [objT c_QString] $ objT c_QByteArray
  , mkConstMethod "formats" np $ objT c_QStringList
  , mkConstMethod "hasColor" np boolT
  , mkConstMethod "hasFormat" [objT c_QString] boolT
  , mkConstMethod "hasHtml" np boolT
  , mkConstMethod "hasImage" np boolT
  , mkConstMethod "hasText" np boolT
  , mkConstMethod "hasUrls" np boolT
  , mkProp "html" $ objT c_QString
  , mkProp "imageData" $ objT c_QVariant
  , mkMethod "removeFormat" [objT c_QString] voidT
  , mkMethod "setData" [objT c_QString, objT c_QByteArray] voidT
  , mkProp "text" $ objT c_QString
  -- TODO void QMimeData::setUrls(const QList<QUrl> &urls)
  ]
