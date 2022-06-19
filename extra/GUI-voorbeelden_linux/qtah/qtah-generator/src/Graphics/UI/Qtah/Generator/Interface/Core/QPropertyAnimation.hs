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

module Graphics.UI.Qtah.Generator.Interface.Core.QPropertyAnimation (
  aModule,
  c_QPropertyAnimation,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkProp,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QVariantAnimation (c_QVariantAnimation)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Foreign.Hoppy.Generator.Types (objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QPropertyAnimation"] [4, 6] $
  [qtExport c_QPropertyAnimation]

c_QPropertyAnimation =
  addReqIncludes [ includeStd "QPropertyAnimation" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QPropertyAnimation") Nothing [c_QVariantAnimation] $
  collect
  [ just $ mkProp "propertyName" $ objT c_QByteArray
  , just $ mkProp "targetObject" $ ptrT $ objT c_QObject
  ]
