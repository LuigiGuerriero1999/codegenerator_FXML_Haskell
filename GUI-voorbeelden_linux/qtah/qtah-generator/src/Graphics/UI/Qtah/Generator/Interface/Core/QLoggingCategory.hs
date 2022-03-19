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

module Graphics.UI.Qtah.Generator.Interface.Core.QLoggingCategory (
  aModule,
  c_QLoggingCategory,
  categoryFilter,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Type,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkStaticMethod,
  mkCtor,
  mkMethod,
  np,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Foreign.Hoppy.Generator.Types (charT, voidT, boolT, enumT, constT, objT, ptrT, refT, fnT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_QtMsgType)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QLoggingCategory"] [5, 2] $
  [qtExport c_QLoggingCategory]

c_QLoggingCategory =
  addReqIncludes [ includeStd "QLoggingCategory" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QLoggingCategory") Nothing [] $
  collect
  [ -- TODO QStrings instead of const char*.
    test (qtVersion >= [5, 4]) $ mkCtor "newWithMsgType" [ptrT $ constT charT, enumT e_QtMsgType]
  , just $ mkCtor "new" [ptrT $ constT charT]
  , just $ mkConstMethod "categoryName" np $ ptrT $ constT charT
  , just $ mkStaticMethod "defaultCategory" np $ ptrT $ objT c_QLoggingCategory
  , just $ mkStaticMethod "installFilter" [categoryFilter] categoryFilter
  , just $ mkConstMethod "isCriticalEnabled" np boolT
  , just $ mkConstMethod "isDebugEnabled" np boolT
  , just $ mkConstMethod "isEnabled" [enumT e_QtMsgType] boolT
  , test (qtVersion >= [5, 5]) $ mkConstMethod "isInfoEnabled" np boolT
  , just $ mkConstMethod "isWarningEnabled" np boolT
  , just $ mkMethod "setEnabled" [enumT e_QtMsgType, boolT] voidT
  , just $ mkStaticMethod "setFilterRules" [refT $ constT $ objT c_QString] voidT
    -- OMIT operator()
  ]

categoryFilter :: Type
categoryFilter = ptrT $ fnT [ptrT $ objT c_QLoggingCategory] voidT
