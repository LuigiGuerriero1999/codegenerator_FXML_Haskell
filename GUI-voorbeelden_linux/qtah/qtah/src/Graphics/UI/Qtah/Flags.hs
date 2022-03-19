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

-- | Support for Qt flags, which are bitwise combinations of enum values wrapped
-- in @QFlags@.
module Graphics.UI.Qtah.Flags (
  Flags (..),
  numToFlags,
  flagsToNum,
  ) where

import Foreign.Hoppy.Runtime (CppEnum, fromCppEnum, toCppEnum)

-- | A C++ enum @e@ with numeric type @n@ can have an associated type @f@ that
-- represents a corresponding @QFlags@ instance.
class CppEnum n e => Flags n e f | e -> f, f -> e, f -> n where
  -- | Converts an enum value to a 'Flags' value.
  enumToFlags :: e -> f

  -- | Converts a 'Flags' value to an enum value.
  flagsToEnum :: f -> e

-- | Builds a 'Flags' value representing a raw number.
numToFlags :: Flags n e f => n -> f
numToFlags = enumToFlags . toCppEnum

-- | Extracts the raw numeric value in a 'Flags' value.
flagsToNum :: Flags n e f => f -> n
flagsToNum = fromCppEnum . flagsToEnum
