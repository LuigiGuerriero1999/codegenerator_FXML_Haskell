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

module Graphics.UI.Qtah.Generator.Interface.Core.QTextEncoder (
  aModule,
  c_QTextEncoder,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Types (intT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QTextCodec (c_QTextCodec) --fl_ConversionFlags)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
-- TODO import Graphics.UI.Qtah.Generator.Interface.Core.QStringView (c_QStringView)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QTextEncoder"] $
  [qtExport c_QTextEncoder]

c_QTextEncoder =
  addReqIncludes [ includeStd "QTextEncoder" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QTextEncoder") Nothing [] $
  collect
  [
    just $ mkCtor "new" [ptrT $ objT c_QTextCodec]
  --, test (qtVersion >= [4, 7]) $ mkCtor "newWithConverFlags" [ptrT $ objT c_QTextCodec, flagsT fl_ConversionFlags]
  --, test (qtVersion >= [5, 10]) $ mkMethod' "fromUnicode" "fromUnicodeWithStringview" [objT c_QStringView] $ objT c_QByteArray
  , just $ mkMethod' "fromUnicode" "fromUnicodeString" [refT $ constT $ objT c_QString] $ objT c_QByteArray
  , just $ mkMethod' "fromUnicode" "fromUnicodeQChar" [ptrT $ constT $ objT c_QChar, intT] $ objT c_QByteArray
  ]
