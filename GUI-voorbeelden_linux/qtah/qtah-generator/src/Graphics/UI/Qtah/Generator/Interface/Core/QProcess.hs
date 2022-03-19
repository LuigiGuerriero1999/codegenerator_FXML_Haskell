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

module Graphics.UI.Qtah.Generator.Interface.Core.QProcess (
  aModule,
  c_QProcess,
  e_ExitStatus,
  e_InputChannelMode,
  e_ProcessChannel,
  e_ProcessChannelMode,
  e_ProcessError,
  e_ProcessState,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkStaticMethod,
  mkStaticMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, refT, voidT, enumT, constT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice, fl_OpenMode)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QProcessEnvironment (c_QProcessEnvironment)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerProcessError,
  listenerIntExitStatus,
  listenerProcessState
  )
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qlonglong)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QProcess"] $
  [ QtExportClassAndSignals c_QProcess signals
  , qtExport e_ExitStatus
  , qtExport e_InputChannelMode
  , qtExport e_ProcessChannel
  , qtExport e_ProcessChannelMode
  , qtExport e_ProcessError
  , qtExport e_ProcessState
  ]

(c_QProcess, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QProcess"] $
  classSetEntityPrefix "" $
  makeClass (ident "QProcess") Nothing [c_QIODevice] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , test (qtVersion >= [5, 0]) $ mkConstMethod "arguments" np $ objT c_QStringList
  , just $ mkConstMethod "atEnd" np boolT
  , just $ mkConstMethod "bytesAvailable" np qlonglong
  , just $ mkConstMethod "bytesToWrite" np qlonglong
  , just $ mkConstMethod "canReadLine" np boolT
  , just $ mkMethod "close" np voidT
  , just $ mkMethod "closeReadChannel" [enumT e_ProcessChannel] voidT
  , just $ mkMethod "closeWriteChannel" np voidT
  -- TODO QProcess::CreateProcessArgumentModifier QProcess::createProcessArgumentsModifier() const
  , just $ mkConstMethod "error" np $ enumT e_ProcessError
  , just $ mkStaticMethod' "execute" "execute" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList] intT
  , just $ mkStaticMethod' "execute" "executeWithCommand" [refT $ constT $ objT c_QString] intT
  , just $ mkConstMethod "exitCode" np intT
  , test (qtVersion >= [4, 1]) $ mkConstMethod "exitStatus" np $ enumT e_ExitStatus
  , test (qtVersion >= [5, 2]) $ mkConstMethod "inputChannelMode" np $ enumT e_InputChannelMode
  , just $ mkConstMethod "isSequential" np boolT
 -- , test (qtVersion >= [4, 7]) $ mkConstMethod "nativeArguments" np $ objT c_QString
  , test (qtVersion >= [5, 2]) $ mkStaticMethod "nullDevice" np $ objT c_QString
  , just $ mkMethod' "open" "open" np boolT
  , just $ mkMethod' "open" "openWithMode" [flagsT fl_OpenMode] boolT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "processChannelMode" np $ enumT e_ProcessChannelMode
  , test (qtVersion >= [4, 2]) $ mkConstMethod "processEnvironment" np $ objT c_QProcessEnvironment
  , test (qtVersion >= [5, 3]) $ mkConstMethod "processId" np qlonglong
  , test (qtVersion >= [5, 0]) $ mkConstMethod "program" np $ objT c_QString
  , just $ mkMethod "readAllStandardError" np $ objT c_QByteArray
  , just $ mkMethod "readAllStandardOutput" np $ objT c_QByteArray
  , just $ mkConstMethod "readChannel" np $ enumT e_ProcessChannel
  , test (qtVersion >= [5, 1]) $ mkMethod "setArguments" [refT $ constT $ objT c_QStringList] voidT
  --TODO void QProcess::setCreateProcessArgumentsModifier(QProcess::CreateProcessArgumentModifier modifier)
  , test (qtVersion >= [5, 2]) $ mkMethod "setInputChannelMode" [enumT e_InputChannelMode] voidT
 -- , test (qtVersion >= [4, 7]) $ mkMethod "setNativeArguments" [refT $ constT $ objT c_QString] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setProcessChannelMode" [enumT e_ProcessChannelMode] voidT
  , test (qtVersion >= [4, 6]) $ mkMethod "setProcessEnvironment" [refT $ constT $ objT c_QProcessEnvironment] voidT
  , test (qtVersion >= [5, 1]) $ mkMethod "setProgram" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "setReadChannel" [enumT e_ProcessChannel] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "setStandardErrorFile" "setStandardErrorFile" [refT $ constT $ objT c_QString] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "setStandardErrorFile" "setStandardErrorFileWithMode" [refT $ constT $ objT c_QString, flagsT fl_OpenMode] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setStandardInputFile" [refT $ constT $ objT c_QString] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "setStandardOutputFile" "setStandardOutputFile" [refT $ constT $ objT c_QString] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "setStandardOutputFile" "setStandardOutputFileWithMode" [refT $ constT $ objT c_QString, flagsT fl_OpenMode] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setStandardOutputProcess" [ptrT $ objT c_QProcess] voidT
  , just $ mkMethod "setWorkingDirectory" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "start" "start" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList] voidT
  , just $ mkMethod' "start" "startWithProgramArgMode" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList, flagsT fl_OpenMode] voidT
  , just $ mkMethod' "start" "startWithCommand" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "start" "startWithCommandMode" [refT $ constT $ objT c_QString, flagsT fl_OpenMode] voidT
  , test (qtVersion >= [5, 1]) $ mkMethod' "start" "startWithMode" [flagsT fl_OpenMode] voidT
  , test (qtVersion >= [5, 10]) $ mkMethod' "startDetached" "startDetached" np boolT
  --, test (qtVersion >= [5, 10]) $ mkMethod' "startDetached" "startDetachedWithPid" [ptrT qlonglong] boolT
  , just $ mkStaticMethod' "startDetached" "startDetachedStatic" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList] boolT
  , just $ mkStaticMethod' "startDetached" "startDetachedStaticDir" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList, refT $ constT $ objT c_QString] boolT
  --, just $ mkStaticMethod' "startDetached" "startDetachedStaticDirPid" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList, refT $ constT $ objT c_QString, ptrT qlonglong] boolT
  , just $ mkStaticMethod' "startDetached" "startDetachedStaticCommand" [refT $ constT $ objT c_QString] boolT
  , just $ mkConstMethod "state" np $ enumT e_ProcessState
  , test (qtVersion >= [4, 1]) $ mkMethod "systemEnvironment" np $ objT c_QStringList
  , just $ mkMethod' "waitForBytesWritten" "waitForBytesWritten" np boolT
  , just $ mkMethod' "waitForBytesWritten" "waitForBytesWrittenMsecs" [intT] boolT
  , just $ mkMethod' "waitForFinished" "waitForFinished" np boolT
  , just $ mkMethod' "waitForFinished" "waitForFinishedMsecs" [intT] boolT
  , just $ mkMethod' "waitForReadyRead" "waitForReadyRead" np boolT
  , just $ mkMethod' "waitForReadyRead" "waitForReadyReadMsecs" [intT] boolT
  , just $ mkMethod' "waitForStarted" "waitForStarted" np boolT
  , just $ mkMethod' "waitForStarted" "waitForStartedMsecs" [intT] boolT
  , just $ mkConstMethod "workingDirectory" np $ objT c_QString

  , just $ mkMethod "kill" np voidT
  , just $ mkMethod "terminate" np voidT
  ]

e_ExitStatus =
  makeQtEnum (ident1 "QProcess" "ExitStatus") [includeStd "QProcess"]
  [ "NormalExit"
  , "CrashExit"
  ]

e_InputChannelMode =
  makeQtEnum (ident1 "QProcess" "InputChannelMode") [includeStd "QProcess"]
  [ "ManagedInputChannel"
  , "ForwardedInputChannel"
  ]

e_ProcessChannel =
  makeQtEnum (ident1 "QProcess" "ProcessChannel") [includeStd "QProcess"]
  [ "StandardOutput"
  , "StandardError"
  ]

e_ProcessChannelMode =
  makeQtEnum (ident1 "QProcess" "ProcessChannelMode") [includeStd "QProcess"] $
  collect $
  [ just "SeparateChannels"
  , just "MergedChannels"
  , just "ForwardedChannels"
  , test (qtVersion >= [5, 2]) "ForwardedErrorChannel"
  , test (qtVersion >= [5, 2]) "ForwardedOutputChannel"
  ]

e_ProcessError =
  makeQtEnum (ident1 "QProcess" "ProcessError") [includeStd "QProcess"]
  [ "FailedToStart"
  , "Crashed"
  , "Timedout"
  , "WriteError"
  , "ReadError"
  , "UnknownError"
  ]

e_ProcessState =
  makeQtEnum (ident1 "QProcess" "ProcessState") [includeStd "QProcess"]
  [ "NotRunning"
  , "Starting"
  , "Running"
  ]

signalGens :: [SignalGen]
signalGens =
  collect $
  [ test (qtVersion >= [5, 6]) $ makeSignal "errorOccurred" listenerProcessError
  , just $ makeSignal "finished" listenerIntExitStatus
  , just $ makeSignalPrivate "readyReadStandardError" listener
  , just $ makeSignalPrivate "readyReadStandardOutput" listener
  , just $ makeSignalPrivate "started" listener
  , just $ makeSignalPrivate "stateChanged" listenerProcessState
  ]
