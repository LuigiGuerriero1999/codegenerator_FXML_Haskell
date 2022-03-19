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

-- | Shared bindings for @\"Qtah*\"@ qualified imports in generated bindings.
-- Some classes use qualified imports that don't start with @\"Qtah\"@; such
-- imports are one-off and are not listed here.
module Graphics.UI.Qtah.Generator.Interface.Imports (
  importForByteString,
  importForByteStringUnsafe,
  importForBits,
  importForChar,
  importForEvent,
  importForFlags,
  importForForeign,
  importForForeignC,
  importForPrelude,
  importForSceneEvent,
  importForSignal,
  importForRuntime,
  ) where

import Foreign.Hoppy.Generator.Spec (HsImportSet, hsQualifiedImport)

importForByteString :: HsImportSet
importForByteString = hsQualifiedImport "Data.ByteString" "QtahDBS"

importForByteStringUnsafe :: HsImportSet
importForByteStringUnsafe = hsQualifiedImport "Data.ByteString.Unsafe" "QtahDBSU"

importForBits :: HsImportSet
importForBits = hsQualifiedImport "Data.Bits" "QtahDB"

importForChar :: HsImportSet
importForChar = hsQualifiedImport "Data.Char" "QtahDC"

importForEvent :: HsImportSet
importForEvent = hsQualifiedImport "Graphics.UI.Qtah.Event" "QtahEvent"

importForFlags :: HsImportSet
importForFlags = hsQualifiedImport "Graphics.UI.Qtah.Flags" "QtahFlags"

importForForeign :: HsImportSet
importForForeign = hsQualifiedImport "Foreign" "QtahF"

importForForeignC :: HsImportSet
importForForeignC = hsQualifiedImport "Foreign.C" "QtahFC"

importForPrelude :: HsImportSet
importForPrelude = hsQualifiedImport "Prelude" "QtahP"

importForSceneEvent :: HsImportSet
importForSceneEvent = hsQualifiedImport "Graphics.UI.Qtah.SceneEvent" "QtahSceneEvent"

importForSignal :: HsImportSet
importForSignal = hsQualifiedImport "Graphics.UI.Qtah.Signal" "QtahSignal"

importForRuntime :: HsImportSet
importForRuntime = hsQualifiedImport "Foreign.Hoppy.Runtime" "QtahFHR"
