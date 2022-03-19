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

module Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (
  aModule,
  c_QIODevice,
  e_OpenModeFlag,
  fl_OpenMode,
  ) where

import Control.Monad (guard)
import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  mkMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, constT, intT, objT, ptrT, refT, ptrT, voidT, charT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listener, listenerQlonglong, listenerIntQlonglong, listenerInt)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qlonglong)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QIODevice"] $
  [ QtExportClassAndSignals c_QIODevice signals
  , qtExport e_OpenModeFlag
  , qtExport fl_OpenMode
  ]

(c_QIODevice, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [ includeStd "QIODevice" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QIODevice") Nothing [c_QObject] $
  collect
  [ just $ mkConstMethod "atEnd" np boolT
  , just $ mkConstMethod "bytesAvailable" np qlonglong
  , just $ mkConstMethod "bytesToWrite" np qlonglong
  , just $ mkConstMethod "canReadLine" np boolT
  , just $ mkMethod "close" np voidT
  , test (qtVersion >= [5, 7]) $ mkMethod "commitTransaction" np voidT
  , test (qtVersion >= [5, 7]) $ mkConstMethod "currentReadChannel" np intT
  , test (qtVersion >= [5, 7]) $ mkConstMethod "currentWriteChannel" np intT
  , just $ mkConstMethod "errorString" np $ objT c_QString
    -- TODO bool getChar(char*)
  , just $ mkConstMethod "isOpen" np boolT
  , just $ mkConstMethod "isReadable" np boolT
  , just $ mkConstMethod "isSequential" np boolT
  , just $ mkConstMethod "isTextModeEnabled" np boolT
  , test (qtVersion >= [5, 7]) $ mkConstMethod "isTransactionStarted" np boolT
  , just $ mkConstMethod "isWritable" np boolT
  , just $ mkMethod "open" [flagsT fl_OpenMode] boolT
  , just $ mkConstMethod "openMode" np $ flagsT fl_OpenMode
  , test (qtVersion >= [4, 1]) $ mkMethod' "peek" "peekByteArray" [qlonglong] $ objT c_QByteArray
  , test (qtVersion >= [4, 1]) $ mkMethod' "peek" "peekRaw" [ptrT charT, qlonglong] qlonglong
  , just $ mkConstMethod "pos" np qlonglong
  , just $ mkMethod "putChar" [charT] boolT
  , just $ mkMethod' "read" "readByteArray" [qlonglong] $ objT c_QByteArray
  , just $ mkMethod' "read" "readRaw" [ptrT charT, qlonglong] qlonglong
  , just $ mkMethod "readAll" np $ objT c_QByteArray
  , test (qtVersion >= [5, 7]) $ mkConstMethod "readChannelCount" np intT
  , just $ mkMethod' "readLine" "readLineByteArray" np $ objT c_QByteArray
  , just $ mkMethod' "readLine" "readLineByteArrayWithSize" [qlonglong] $ objT c_QByteArray
  , just $ mkMethod' "readLine" "readLineRaw" [ptrT charT, qlonglong] qlonglong
  , just $ mkMethod "reset" np boolT
  , test (qtVersion >= [5, 7]) $ mkMethod "rollbackTransaction" np voidT
  , just $ mkMethod "seek" [qlonglong] boolT
  , test (qtVersion >= [5, 7]) $ mkMethod "setCurrentReadChannel" [intT] voidT
  , test (qtVersion >= [5, 7]) $ mkMethod "setCurrentWriteChannel" [intT] voidT
  , just $ mkMethod "setTextModeEnabled" [boolT] voidT
  , just $ mkConstMethod "size" np qlonglong
  , test (qtVersion >= [5, 10]) $ mkMethod "skip" [qlonglong] qlonglong
  , test (qtVersion >= [5, 7]) $ mkMethod "startTransaction" np voidT
  , just $ mkMethod "ungetChar" [charT] voidT
  , just $ mkMethod "waitForBytesWritten" [intT] boolT
  , just $ mkMethod "waitForReadyRead" [intT] boolT
  , just $ mkMethod' "write" "writeByteArray" [refT $ constT $ objT c_QByteArray] qlonglong
  , just $ mkMethod' "write" "writeRaw" [ptrT $ constT charT] qlonglong
  , just $ mkMethod' "write" "writeRawWithSize" [ptrT $ constT charT, qlonglong] qlonglong
  , test (qtVersion >= [5, 7]) $ mkConstMethod "writeChannelCount" np intT
  ]

(e_OpenModeFlag, fl_OpenMode) =
  makeQtEnumAndFlags (ident1 "QIODevice" "OpenModeFlag") "OpenMode" [includeStd "QIODevice"] $
  [ "NotOpen"
  , "ReadOnly"
  , "WriteOnly"
  , "ReadWrite"
  , "Append"
  , "Truncate"
  , "Text"
  , "Unbuffered"
  ] ++
  (guard (qtVersion >= [5, 11]) *>
    [ "NewOnly"
    , "ExistingOnly"
    ])

signalGens :: [SignalGen]
signalGens =
  collect $
  [ just $ makeSignal "aboutToClose" listener
  , just $ makeSignal "bytesWritten" listenerQlonglong
  , test (qtVersion >= [5, 7]) $ makeSignal "channelBytesWritten" listenerIntQlonglong
  , test (qtVersion >= [5, 7]) $ makeSignal "channelReadyRead" listenerInt
  , test (qtVersion >= [4, 4]) $ makeSignal "readChannelFinished" listener
  , just $ makeSignal "readyRead" listener
  ]
