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

module Graphics.UI.Qtah.Generator.Interface.Core.QMessageLogger (
  aModule,
  c_QMessageLogger,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod',
  mkCtor,
  np,
  )
import Foreign.Hoppy.Generator.Types (charT, intT, constT, objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QDebug (c_QDebug)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QMessageLogger"] [5, 0] $
  [qtExport c_QMessageLogger]

c_QMessageLogger =
  addReqIncludes [ includeStd "QMessageLogger" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QMessageLogger") Nothing [] $
  collect
  [ -- TODO QStrings instead of const char*.
    just $ mkCtor "new" np
  , just $ mkCtor "newWithContext" [ptrT $ constT $ charT, intT, ptrT $ constT $ charT]
  , just $ mkCtor "newWithContextAndCategory" [ptrT $ constT charT, intT, ptrT $ constT charT, ptrT $ constT charT]

    -- The following methods also have overloads that take format strings with
    -- variadic arguments.  We omit those as Qtah doesn't support varargs.

  , just $ mkConstMethod' "critical" "critical" np $ objT c_QDebug
  , just $ mkConstMethod' "debug" "debug" np $ objT c_QDebug
  , test (qtVersion >= [5, 5]) $ mkConstMethod' "info" "info" np $ objT c_QDebug
  , test (qtVersion >= [5, 3]) $ mkConstMethod' "warning" "warning" np $ objT c_QDebug
  ]
