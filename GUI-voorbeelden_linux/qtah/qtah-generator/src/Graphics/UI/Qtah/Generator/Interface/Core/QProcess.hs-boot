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

module Graphics.UI.Qtah.Generator.Interface.Core.QProcess (
  c_QProcess,
  e_ExitStatus,
  e_InputChannelMode,
  e_ProcessChannel,
  e_ProcessChannelMode,
  e_ProcessError,
  e_ProcessState,
  ) where

import Foreign.Hoppy.Generator.Spec (Class, CppEnum)


c_QProcess :: Class

e_ExitStatus :: CppEnum

e_InputChannelMode :: CppEnum

e_ProcessChannel :: CppEnum

e_ProcessChannelMode :: CppEnum

e_ProcessError :: CppEnum

e_ProcessState :: CppEnum
