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

# Builds a release version of qtah-examples and all other (dependent) packages.
# The closure of the derivation for this expression contains the source tarballs
# for all of Qtah:
#
# nix-store -r $(nix-store -qR $(nix-store -q --deriver result) | grep -e 'qtah.*source')

{ pkgs ? import <nixpkgs> {}
, overlays ? []
}:

let
  pkgsWithQtah = import ./nixpkgs.nix { inherit pkgs overlays; };
in pkgsWithQtah.haskellPackages.qtah-examples
