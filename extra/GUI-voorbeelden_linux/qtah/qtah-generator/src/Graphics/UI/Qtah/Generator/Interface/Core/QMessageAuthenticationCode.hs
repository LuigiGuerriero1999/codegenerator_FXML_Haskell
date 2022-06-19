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

module Graphics.UI.Qtah.Generator.Interface.Core.QMessageAuthenticationCode (
  aModule,
  c_QMessageAuthenticationCode,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkStaticMethod,
  mkConstMethod,
  mkCtor,
  mkMethod',
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Types (charT, intT, boolT, voidT, enumT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QCryptographicHash (e_Algorithm)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QMessageAuthenticationCode"] [5, 1] $
  [qtExport c_QMessageAuthenticationCode]

c_QMessageAuthenticationCode =
  addReqIncludes [ includeStd "QMessageAuthenticationCode" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QMessageAuthenticationCode") Nothing [] $
  collect
  [ just $ mkCtor "new" [enumT e_Algorithm]
  , just $ mkCtor "newWithKey" [enumT e_Algorithm, refT $ constT $ objT c_QByteArray]
  , just $ mkMethod' "addData" "addDataRaw" [ptrT $ constT charT, intT] voidT
  , just $ mkMethod' "addData" "addDataByteArray" [refT $ constT $ objT c_QByteArray] voidT
  , just $ mkMethod' "addData" "addDataDevice" [ptrT $ objT c_QIODevice] boolT
  , just $ mkStaticMethod "hash" [refT $ constT $ objT c_QByteArray, refT $ constT $ objT c_QByteArray, enumT e_Algorithm] $ objT c_QByteArray
  , just $ mkMethod "reset" np voidT
  , just $ mkConstMethod "result" np $ objT c_QByteArray
  , just $ mkMethod "setKey" [refT $ constT $ objT c_QByteArray] voidT
  ]
