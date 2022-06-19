# This file is part of Qtah.
#
# Copyright 2015-2021 The Qtah Authors.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# This is a reimplementation of haskell.lib.buildStrictly.  It still calls
# buildFromSdist, but it only enables -Werror, not -Wall.

haskellLib:
pkg:
haskellLib.buildFromSdist (haskellLib.appendConfigureFlag pkg "--ghc-option=-Werror")
