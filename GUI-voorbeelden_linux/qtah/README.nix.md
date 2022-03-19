Nix expressions are provided to ease building Qtah within
[Nixpkgs](https://nixos.org/nixpkgs).  Qtah can be inserted into Nixpkgs with
the overlay at `nix/overlay.nix`, or you can use `nix/nixpkgs.nix` which
provides Nixpkgs with Qtah built-in.  You may need to locally modify
`nixpkgs.nix` to include the overlay provided by a local copy of Hoppy (at
`nix/overlay.nix` in the Hoppy repo).

The Nix expressions for Qtah packages accept one additional optional parameter.
The boolean `forceParallelBuilding` will force a parallel build for faster
development, at the risk of nondeterministic results (see
[Nixpkgs bug 3220](https://github.com/NixOS/nixpkgs/issues/3220)).

---

This file is part of Qtah.

Copyright 2015-2021 The Qtah Authors.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
