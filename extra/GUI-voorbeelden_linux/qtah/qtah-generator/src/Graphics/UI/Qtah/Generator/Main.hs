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

module Graphics.UI.Qtah.Generator.Main (
  run,
  generateCpp,
  generateHs,
  ) where

import Control.Monad (when)
import Data.List (intercalate)
import qualified Foreign.Hoppy.Generator.Main as GeneratorMain (run)
import Foreign.Hoppy.Generator.Spec (
  Interface,
  Module,
  interface,
  interfaceAddHaskellModuleBase,
  moduleModify',
  moduleSetCppPath,
  moduleSetHppPath,
  )
import qualified Foreign.Hoppy.Generator.Std as Std
import Graphics.UI.Qtah.Generator.Enum (installEnumCalculator)
import Graphics.UI.Qtah.Generator.Config (qmakeArguments, qmakeExecutable, qtVersion)
import Graphics.UI.Qtah.Generator.Module
import qualified Graphics.UI.Qtah.Generator.Interface.Core as Core
import qualified Graphics.UI.Qtah.Generator.Interface.Gui as Gui
import qualified Graphics.UI.Qtah.Generator.Interface.Internal as Internal
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets as Widgets
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

mod_std :: Module
mod_std = moduleModify' Std.mod_std $ do
  moduleSetHppPath "b_std.hpp"
  moduleSetCppPath "b_std.cpp"

modules :: [AModule]
modules =
  concat
  [ [ AHoppyModule mod_std
    ]
  , Core.modules
  , Gui.modules
  , Internal.modules
  , Widgets.modules
  ]

interfaceResult :: Either String Interface
interfaceResult =
  fmap installEnumCalculator $
  interfaceAddHaskellModuleBase ["Graphics", "UI", "Qtah"] =<<
  interface "qtah" (concatMap aModuleHoppyModules modules)

-- | Generates the C++ side of the Qtah bindings, producing files in the given
-- directory.
generateCpp :: FilePath -> IO ()
generateCpp path = run ["--gen-cpp", path]

-- | Generates the Haskell side of the Qtah bindings in the given source
-- directory.
generateHs :: FilePath -> IO ()
generateHs path = run ["--gen-hs", path]

-- | Runs the Qtah generator with the given command line arguments.
run :: [String] -> IO ()
run args =
  case interfaceResult of
    Left errorMsg -> do
      putStrLn $ "Error initializing interface: " ++ errorMsg
      exitFailure
    Right iface -> do
      -- If building against Qt 4, then warn that it is no longer supported.
      when (qtVersion < [5, 0]) $ do
        hPutStrLn stderr $
          "WARNING: Qtah no longer supports Qt 4.x.  Please upgrade to Qt 5.  Found version " ++
          intercalate "." (map show qtVersion) ++ "."

      case args of
        ["--qt-version"] -> putStrLn $ intercalate "." $ map show qtVersion
        ["--qmake-executable"] -> putStr $ unlines $ qmakeExecutable : qmakeArguments
        _ -> do
          _ <- GeneratorMain.run [iface] args
          return ()
