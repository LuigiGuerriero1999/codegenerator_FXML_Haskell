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

# Derviation that contains the complete raw Qt sources, after running
# "scripts/set-qt-version 5" to modify the sources to rename packages for
# release:
#
#     qtah-cpp -> qtah-cpp-qt5
#     qtah -> qtah-qt5

{ pkgs ? import <nixpkgs> {} }:

let

  package = { bash, nix-gitignore, stdenv }:
    stdenv.mkDerivation {
      name = "qtah-qt5-sources";
      src = nix-gitignore.gitignoreSource [] ../..;

      buildInputs = [ bash ];

      unpackPhase = "true";
      buildPhase = "true";
      installPhase = ''
        mkdir -p $out
        cp -r $src/* $out
        find $out -type f -exec chmod u+w {} +
        find $out -type d -exec chmod u+w {} +
        cd $out
        bash scripts/set-qt-version 5
      '';
    };

in pkgs.callPackage package {}
