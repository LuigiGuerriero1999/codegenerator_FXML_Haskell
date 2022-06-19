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

{ mkDerivation, base, binary, bytestring, hoppy-runtime, HUnit
, qt, qtah-cpp, qtah-generator, lib
, forceParallelBuilding ? false
}:
mkDerivation {
  pname = "qtah";
  version = "0.8.1";
  src = ./.;
  buildTools = [ qt.wrapQtAppsHook ];
  setupHaskellDepends = [ qtah-generator ];
  libraryHaskellDepends = [
    base binary bytestring hoppy-runtime qtah-cpp qtah-generator
  ];
  libraryToolDepends = [ qt.qtbase ];
  testHaskellDepends = [ base hoppy-runtime HUnit ];
  homepage = "http://khumba.net/projects/qtah";
  description = "Qt bindings for Haskell";
  license = lib.licenses.lgpl3Plus;

  preConfigure =
    if forceParallelBuilding
    then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
    else null;

  # Save 9000+ lines of useless warnings.
  haddockFlags = [ "--haddock-options=--no-print-missing-docs" ];
}
