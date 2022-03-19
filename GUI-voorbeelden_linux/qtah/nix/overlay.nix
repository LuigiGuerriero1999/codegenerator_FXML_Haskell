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

let

  qtName = "qt5";

  haskellOptions =
    if builtins.pathExists ../config.nix
    then import ../config.nix
    else {};

  haskellOverrides = super: hself: hsuper:
    let qt = super.${qtName};
        buildStrictly = import ./build-strictly.nix super.haskell.lib;
        # Disable profiling for these qtah derivations by default; qtah builds
        # *really* slowly with them.
        mkDerivation = args: hsuper.mkDerivation ({ enableLibraryProfiling = false; } // args);
        opts = haskellOptions // { inherit mkDerivation; };
    in builtins.mapAttrs (name: pkg: buildStrictly pkg) {
      qtah-generator = hsuper.callPackage ../qtah-generator opts;
      qtah-cpp = hsuper.callPackage ../qtah-cpp (opts // { inherit qt; });
      qtah = hsuper.callPackage ../qtah (opts // { inherit qt; });
      qtah-examples = hsuper.callPackage ../qtah-examples (opts // { inherit qt; });
    };

in self: super: {
  haskell = super.haskell // {
    packageOverrides =
      super.lib.composeExtensions
        (super.haskell.packageOverrides or (x: y: {}))
        (haskellOverrides super);
  };
}
