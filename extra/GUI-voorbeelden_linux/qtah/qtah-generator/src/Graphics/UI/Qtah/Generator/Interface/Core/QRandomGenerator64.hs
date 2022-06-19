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

module Graphics.UI.Qtah.Generator.Interface.Core.QRandomGenerator64 (
  aModule,
  c_QRandomGenerator64,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Operator (OpCall),
  classSetConversionToGc,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkStaticMethod,
  mkCtor,
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (constT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (quint32, quint64, qsizetype)
import Graphics.UI.Qtah.Generator.Interface.Core.QRandomGenerator (c_QRandomGenerator)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QRandomGenerator64"] [5, 10] $
  [qtExport c_QRandomGenerator64]

c_QRandomGenerator64 =
  addReqIncludes [ includeStd "QRandomGenerator64" ] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QRandomGenerator64") Nothing [c_QRandomGenerator] $
  collect
  [ just $ mkCtor "newWithQuints" [ptrT $ constT quint32, ptrT $ constT quint32]
  -- TODO QRandomGenerator::QRandomGenerator(std::seed_seq &sseq)
  , just $ mkCtor "newWithQuintQsizetype" [ptrT $ constT quint32, qsizetype]
  -- TODO QRandomGenerator::QRandomGenerator(const quint32 (&)[N] seedBuffer = ...)
  , just $ mkCtor "new" np
  , just $ mkCtor "newWithQuint" [quint32]
  , just $ mkMethod "generate" np quint64
  , just $ mkStaticMethod "max" np quint64
  , just $ mkStaticMethod "min" np quint64
  , just $ mkMethod OpCall np quint64
  ]
