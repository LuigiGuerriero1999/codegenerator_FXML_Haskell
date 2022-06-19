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

module Graphics.UI.Qtah.Generator.Interface.Core.QRandomGenerator (
  aModule,
  c_QRandomGenerator,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Operator (OpCall),
  classSetConversionToGc,
  classSetMonomorphicSuperclass,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkStaticMethod,
  mkCtor,
  mkMethod',
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (doubleT, intT, uintT, ullongT, voidT, constT, objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (quint32, quint64, qsizetype)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QRandomGenerator"] [5, 10] $
  [qtExport c_QRandomGenerator]

c_QRandomGenerator =
  addReqIncludes [ includeStd "QRandomGenerator" ] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetMonomorphicSuperclass $
  classSetEntityPrefix "" $
  makeClass (ident "QRandomGenerator") Nothing [] $
  collect
  [ just $ mkCtor "newWithQuints" [ptrT $ constT quint32, ptrT $ constT quint32]
  -- TODO QRandomGenerator::QRandomGenerator(std::seed_seq &sseq)
  , just $ mkCtor "newWithQuintQsizetype" [ptrT $ constT quint32, qsizetype]
  -- TODO QRandomGenerator::QRandomGenerator(const quint32 (&)[N] seedBuffer = ...)
  , just $ mkCtor "new" np
  , just $ mkCtor "newWithQuint" [quint32]
  , just $ mkMethod' "bounded" "boundedWithDouble" [doubleT] doubleT
  , just $ mkMethod' "bounded" "boundedWithQuint" [quint32] quint32
  , just $ mkMethod' "bounded" "boundedWirhQuints" [quint32, quint32] quint32
  , just $ mkMethod' "bounded" "boundedWithInt" [intT] intT
  , just $ mkMethod' "bounded" "boundedWithInts" [intT, intT] intT
  , just $ mkMethod "discard" [ullongT] $ voidT
  , just $ mkMethod "fillRange" [ptrT uintT, qsizetype] voidT
  -- TODO just $ mkMethod "fillRange" [ptrT $ uintT, qsizetype] $ voidT
  , just $ mkMethod "generate64" np quint64
  , just $ mkMethod "generate" np quint32
  -- TODO void QRandomGenerator::generate(ForwardIterator begin, ForwardIterator end)
  , just $ mkMethod "generateDouble" np doubleT
  , just $ mkStaticMethod "global" np $ ptrT $ objT c_QRandomGenerator
  , just $ mkStaticMethod "max" np quint32
  , just $ mkStaticMethod "min" np quint32
  , just $ mkStaticMethod "securelySeeded" np $ objT c_QRandomGenerator
  , just $ mkMethod' "seed" "seed" np voidT
  , just $ mkMethod' "seed" "seedWithQuint" [quint32] voidT
  -- TODO void QRandomGenerator::seed(std::seed_seq &seed)
  , just $ mkStaticMethod "system" np $ ptrT $ objT c_QRandomGenerator
  , just $ mkMethod OpCall np quint32
  ]
