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

module Graphics.UI.Qtah.Generator.Interface.Gui.QDoubleValidator (
  aModule,
  c_QDoubleValidator,
  e_Notation,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod',
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (doubleT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Gui.QValidator (c_QValidator)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QDoubleValidator"] $
  collect
  [ just $ qtExport c_QDoubleValidator
  , test (qtVersion >= [4, 3]) $ qtExport e_Notation
  ]

c_QDoubleValidator =
  addReqIncludes [includeStd "QDoubleValidator"] $
  classSetEntityPrefix "" $
  makeClass (ident "QDoubleValidator") Nothing [c_QValidator] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkCtor "newWithOptions" [doubleT, doubleT, intT]
  , just $ mkCtor "newWithOptionsAndParent" [doubleT, doubleT, intT, ptrT $ objT c_QObject]
  , just $ mkProp "bottom" doubleT
  , just $ mkProp "decimals" intT
  , test (qtVersion >= [4, 3]) $ mkProp "notation" $ enumT e_Notation
  , just $ mkMethod' "setRange" "setRange" [doubleT, doubleT] voidT
  , just $ mkMethod' "setRange" "setRangeAndDecimals" [doubleT, doubleT, intT] voidT
  , just $ mkProp "top" doubleT
  ]

e_Notation =
  makeQtEnum (ident1 "QDoubleValidator" "Notation") [includeStd "QDoubleValidator"]
  [ "StandardNotation"
  , "ScientificNotation"
  ]
