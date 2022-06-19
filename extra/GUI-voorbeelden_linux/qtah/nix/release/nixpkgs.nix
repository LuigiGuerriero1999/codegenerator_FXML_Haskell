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

# Evaluates to Nixpkgs with an overlay from patched (qt5-sources.nix) Qtah
# sources applied, so that the following packages are available:
#
#     qtah-generator
#     qtah-cpp-qt5
#     qtah-qt5
#     qtah-examples

# Warning, use of import-from-derivation follows.

{ pkgs ? import <nixpkgs> {}
, overlays ? []
}:

let

  # Create a patched version of the sources, where qtah-cpp is renamed to
  # qtah-cpp-qt5, and similarly with qtah (to qtah-qt5).
  sources = import ./qt5-sources.nix { inherit pkgs; };

  qtahOverlay = import (sources + /nix/overlay.nix);

in import <nixpkgs> {
  overlays =
    (map (x: if builtins.isPath x then import x else x) overlays) ++
    [ qtahOverlay ];
}
