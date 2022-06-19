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

module Graphics.UI.Qtah.Generator.Interface.Core.QCryptographicHash (
  aModule,
  c_QCryptographicHash,
  e_Algorithm,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkStaticMethod,
  mkCtor,
  mkMethod',
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, charT, intT, voidT, enumT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QCryptographicHash"] [4, 3] $
  collect
  [ just $ qtExport c_QCryptographicHash
  , just $ qtExport e_Algorithm
  ]

c_QCryptographicHash =
  addReqIncludes [ includeStd "QCryptographicHash" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QCryptographicHash") Nothing [] $
  collect
  [ just $ mkCtor "new" [enumT e_Algorithm]
  , just $ mkMethod' "addData" "addDataRaw" [ptrT $ constT charT, intT] voidT
  , just $ mkMethod' "addData" "addDataByteArray" [refT $ constT $ objT c_QByteArray] voidT
  , test (qtVersion >= [5, 0]) $ mkMethod' "addData" "addDataIODevice" [ptrT $ objT c_QIODevice] boolT
  , just $ mkStaticMethod "hash" [refT $ constT $ objT c_QByteArray, enumT e_Algorithm] $ objT c_QByteArray
  , test (qtVersion >= [5, 12]) $ mkStaticMethod "hashLength" [enumT e_Algorithm] intT
  , just $ mkMethod "reset" np $ voidT
  , just $ mkConstMethod "result" np $ objT c_QByteArray
  ]

e_Algorithm =
  makeQtEnum (ident1 "QCryptographicHash" "Algorithm") [includeStd "QCryptographicHash"] $
  collect
  [ just "Md4"
  , just "Md5"
  , just "Sha1"
  , test (qtVersion >= [5, 0]) "Sha224"
  , test (qtVersion >= [5, 0]) "Sha256"
  , test (qtVersion >= [5, 0]) "Sha384"
  , test (qtVersion >= [5, 0]) "Sha512"
  , test (qtVersion >= [5, 1]) "Sha3_224"
  , test (qtVersion >= [5, 1]) "Sha3_256"
  , test (qtVersion >= [5, 1]) "Sha3_384"
  , test (qtVersion >= [5, 1]) "Sha3_512"
  , test (qtVersion >= [5, 9, 2]) "Keccak_224"
  , test (qtVersion >= [5, 9, 2]) "Keccak_256"
  , test (qtVersion >= [5, 9, 2]) "Keccak_384"
  , test (qtVersion >= [5, 9, 2]) "Keccak_512"
  ]
