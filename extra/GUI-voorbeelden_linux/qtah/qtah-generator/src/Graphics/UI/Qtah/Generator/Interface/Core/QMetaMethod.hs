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

module Graphics.UI.Qtah.Generator.Interface.Core.QMetaMethod (
  aModule,
  c_QMetaMethod,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Operator (OpNe, OpEq),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, refT, enumT, constT, charT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQByteArray)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QMetaMethod"]
  [ qtExport c_QMetaMethod
  , qtExport e_Access
  , qtExport e_MethodType
  ]

c_QMetaMethod =
  addReqIncludes [includeStd "QMetaMethod"] $
  classSetConversionToGc $
  classAddFeatures [Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QMetaMethod") Nothing [] $
  collect
  [ just $ mkConstMethod "access" np $ enumT e_Access
  , test (qtVersion >= [5, 0]) $ mkConstMethod "isValid" np boolT
  , test (qtVersion >= [4, 6]) $ mkConstMethod "methodIndex" np intT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "methodSignature" np $ objT c_QByteArray
  , just $ mkConstMethod "methodType" np $ enumT e_MethodType
  , test (qtVersion >= [5, 0]) $ mkConstMethod "name" np $ objT c_QByteArray
  , test (qtVersion >= [5, 0]) $ mkConstMethod "parameterCount" np intT
  , just $ mkConstMethod "parameterNames" np $ objT c_QListQByteArray
  , test (qtVersion >= [5, 0]) $ mkConstMethod "parameterType" [intT] intT
  , just $ mkConstMethod "parameterTypes" np $ objT c_QListQByteArray
  , test (qtVersion >= [5, 0]) $ mkConstMethod "returnType" np intT
  , test (qtVersion >= [5, 1]) $ mkConstMethod "revision" np intT
    -- TODO QStrings instead of const char*.
  , just $ mkConstMethod "tag" np $ ptrT $ constT charT
  , just $ mkConstMethod "typeName" np $ ptrT $ constT charT
  , test (qtVersion >= [5, 0]) $ mkMethod OpNe [refT $ constT $ objT c_QMetaMethod] boolT
  , test (qtVersion >= [5, 0]) $ mkMethod OpEq [refT $ constT $ objT c_QMetaMethod] boolT

  -- TODO invoke methods
  ]

e_Access =
  makeQtEnum (ident1 "QMetaMethod" "Access") [includeStd "QMetaMethod"]
  [ "Private"
  , "Protected"
  , "Public"
  ]

e_MethodType =
  makeQtEnum (ident1 "QMetaMethod" "MethodType") [includeStd "QMetaMethod"]
  [ "Method"
  , "Signal"
  , "Slot"
  , "Constructor"
  ]
