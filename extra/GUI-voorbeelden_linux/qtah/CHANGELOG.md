# Qtah Changelog

## Unreleased

## (2021-02-06) *-0.8.0

- Nix-style local builds i.e. `cabal v2-build` are now supported (issue #26),
  through numerous changes: qtah-generator is now "build-type: Simple" and a
  library, qtah-listener-gen is ported to Haskell, a missing register hook was
  added to qtah-cpp.

- Added 9 new classes thanks to @juliagoda.

## (2020-06-03) qtah-generator-0.7.1

- Restored compatibility with Qt 5.5 by adding missing version bounds on enum
  values, mostly in the `Qt` namespace (issue #49).  This also adds a couple of
  `Qt:Key_*` constants that were in the code but were commented out for some
  reason.

## (2020-04-13) *-0.7.0

- Added the ability to emit Qt signals from Haskell code by exposing them as the
  regular methods that they are (issue #40).  These methods' names begin with
  `emit` to make it clear that they emit signals.

- Fixed building on macOS w.r.t. new enum autodetection (issue #43).

- Version bound fixes for parts of the Qt API (issue #42).

- Removed QApplication signals `aboutToReleaseGpuResources` and
  `aboutToUseGpuResources`.  These were dropped a while ago (perhaps with the
  launch of Qt 5) and were only available for the Symbian platform.

- Fixed misspelling of QWidget's `windowTitleChanged` signal making this signal
  inoperable.  Also fixed the version bounds for this and the
  `windowIconChanged` signals.

- Updated to support GHC 8.8.* and 8.10.1.  Some MonadFail and import changes
  were required.

## (2019-12-23) *-0.6.0

This release marks the largest API increase in any Qtah release so far: 62 new
classes, many others fleshed out with new methods, and lots of new enums as
well.  This also bumps Qtah from Hoppy 0.5 to Hoppy 0.7, incorporating all of
the new features launched in Hoppy 0.6 (the impact of which is summarized here).

- [API change] `QFlags` are now implemented in Qtah itself, rather than relying
  on Bitspaces from Hoppy.  There is a new `Flags` typeclasses to complement the
  existing `IsFoo` per-bitspace/flags typeclasses.  This new typeclass links a
  flags type together with its enum and raw numeric types.  The `IsFoo`
  typeclasses no longer have instances for `Int`, to make conversions between
  numeric types more explicit.

- [API change] Passing this on from Hoppy, there are some important changes to
  enums.

  One: All enums are now capable of representing unknown values, i.e. if Qt
  defines values that Qtah doesn't, Qtah code will no longer invoke `error` on
  seeing such a value.  There is an additional "Unknown" data constructor on
  each enum that holds a number.  GHC will warn about these if you don't pattern
  match against them.

  Two: Hoppy previously had issues when two enum entries had the same numeric
  value, so for the binding definitions we just picked the most sensible one.
  Hoppy now supports this, so we can start including all enum entries.  Some
  but not all enums have had their missing values added.

  Three: Enums no longer have instances for `Bounded` and `Enum`.  Instead, they
  have instances for a new `CppEnum` typeclass provided by Hoppy.  The `Bounded`
  and `Enum` instances weren't implemented correctly.

  Four: Hoppy now handles enum numeric types correctly rather than assuming
  everything is an `Int`.  This might require you to perform additional casts
  for type safety.

- Thanks to Maxim Koltsov and Yuriy Syrovetskiy for more additions and bug
  fixes, and an especially huge thanks to Jagoda Górska for a huge number of
  additions in this release: 62 new classes, many others fleshed out with new
  methods, and lots of new enums as well.

- As of this release, Qtah will no longer support Qt 4.x.  Qt 4 has been dead
  for a long time now, with most Linux distributions having dropped it, and it
  is no longer feasible to continue testing against and supporting it.

- Removed uses of CPP from qtah-generator, except for Setup.hs.  This bumps
  minimum version requirements to base >= 4.8.0 (GHC 8.0) and mtl >= 2.2.1, both
  of which have been available for over two years.

- The Nix expressions, which had long been languishing, have been brought up to
  date, and now make use of Nix overlays.  The example now includes Nix
  expressions.

## (2018-09-07) qtah-0.5.1

- Fixed the build under Cabal 2.2.

## (2018-09-06) qtah-cpp-0.5.1

- Fixed the build under Cabal 2.2.

## (2018-06-05) *-0.5.0

Thanks to Yuriy Syrovetskiy, Maxim Koltsov, and Paul Tsupikoff for contributions
to this release.

- New coverage of the Qt API, plus some version range fixes, totalling 10 new
  classes, 8 new enums, and 12 other classes expanded upon.

- Fixed the invocation of qtah-listener-gen under Windows/MSYS (issue #25).

- Fixed a linking issue with the use of Qtah data types via Template Haskell,
  such as in lenses (MR !13).

- Changed qtah.cabal's "extra-libaries: qtah" to be specified dynamically by
  Setup.hs, since Setup.hs is needed to be able to find this library.  This
  fixes an issue for cabal2nix (part of issue #27, MR !13).

## (2018-01-27) *-0.4.0

- [API change] Changed QAction's constructors so that "new" and "newWithText"
  don't require parent parameters, and have "WithParent" forms instead, like
  normal.

- [API change] `QImage::fromData*()` and `::loadFromData*()` were renamed to
  `...DataRaw*()`, because there are QByteString versions of these functions
  now.

- New classes, many of these thanks to Yuriy Syrovetskiy:
  - Core: QByteArray, QTextCodec
  - Gui: QIcon, QPixmap, QStandardItem{,Model}
  - Widgets: QSystemTrayIcon, QToolBar, QTreeView, QTreeWidget{,Item}

- Added support for Cabal 2.0 and fixed building under macOS (thanks Yuriy).

- qtah-listener-gen now checks that bash 4.1 or newer is available, as it
  requires features introduced in that version, and OS X doesn't ship a bash
  anywhere near that new (issue #19).

- install.sh now defaults to Qt 5 when no `QTAH_QT_FLAGS` environment variable
  is set, instead of selecting the system default (which is still Qt 4 on some
  Linux distros).  To get the old behaviour, set `QTAH_QT_FLAGS` to an empty
  value.

## (2017-06-10) *-0.3.0, qtah-cpp-0.3.1

37 new classes and 10 new enums in this release!

- Added support for the Qt graphics view framework thanks to @effectfully.

- Began adding model-view classes.

- Qtah now builds on OS X, with help from @biglambda.

- qtah-examples now presents a launcher, so that we can have more than one
  example program.

- Discovered that "ghc-options: -dynamic" can be used for executables and tests
  linking to Qtah in lieu of having to pass Cabal "--enable-executable-dynamic"
  on the command line (thanks again @effectfully).

- qtah-cpp now uses all available CPUs to build by default, as "cabal build"
  does (this requires Cabal >=1.20).

- Dependency bump to support directory-1.3 in GHC 8.0.2, and HUnit-1.6.

- Support for Qt <4.8 has been dropped, since even Qt 4.8 is quite old now.

## (2017-01-14) qtah-examples-0.2.1

- Add a configure warning when qtah-examples is being built without dynamic
  executable linking, since without this hint, it's not obvious at all why a
  simple "cabal install qtah-examples" fails.

## (2016-12-16) *-0.2.0, qtah-generator-0.2.1

- Makes use of the new class/enum/bitspace prefix customization feature in Hoppy
  0.3.0 to strip the repetitive class name off of enum value names, for enums
  contained within classes (issue #10).  This is an API change from 0.1.*.  (We
  also do this internally for class entities to simplify Qtah's generator.)

- Fixed the conversion from QPoint and QPointF to their H* components mistakenly
  swapping the components.

- Fixed build issue with Cabal 1.24 / GHC 8 (issue #14).

## (2016-10-01) qtah-0.1.2, qtah-examples-0.1.2

- Version bump to support binary-0.8.*.

## (2016-08-04) qtah-generator-0.1.2, qtah-cpp-0.1.2, qtah-0.1.1

- Fixes the custom install logic to install additional files into requested
  locations instead of the default system ones.

## (2016-07-30) qtah-cpp-0.1.1, qtah-examples-0.1.1

- Another fix for NixOS, qtah-cpp expected qtchooser to be available when a
  version preference was specified (issue #8).

- The notepad example is a much more usable program now.

## (2016-07-15) qtah-generator-0.1.1

- Allow `QTAH_QT=x` to work when qtchooser is not available, to fix building
  with Nixpkgs (issue #8).

## (2016-07-10) *-0.1.0

- Initial release.
