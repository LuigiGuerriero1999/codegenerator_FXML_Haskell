-- This file is part of Qtah.
--
-- Copyright 2018-2021 The Qtah Authors.
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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemDelegate (
  aModule,
  c_QAbstractItemDelegate,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractItemDelegate"]
  [ QtExportClassAndSignals c_QAbstractItemDelegate signals ]

(c_QAbstractItemDelegate, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QAbstractItemDelegate"] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractItemDelegate") Nothing [c_QObject]
  [ -- TODO Methods.
  ]

signalGens :: [SignalGen]
signalGens =
  [ -- TODO Signals.
  ]
