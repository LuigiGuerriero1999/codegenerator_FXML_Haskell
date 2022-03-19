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

module Graphics.UI.Qtah.Generator.Interface.Core.QCoreApplication (
  aModule,
  c_QCoreApplication,
  ) where

import Foreign.Hoppy.Generator.Spec (
  MethodApplicability (MStatic),
  Purity (Nonpure),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident2,
  includeLocal,
  includeStd,
  makeFnMethod,
  makeClass,
  mkStaticMethod,
  mkStaticMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, voidT, enumT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qlonglong, e_ApplicationAttribute)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QCoreApplication"]
  [ qtExport c_QCoreApplication ]

c_QCoreApplication =
  addReqIncludes [ includeStd "QCoreApplication"
                 , includeLocal "wrap_qcoreapplication.hpp"
                 ] $
  classSetEntityPrefix "" $
  makeClass (ident "QCoreApplication") Nothing [c_QObject] $
  collect
  [ just $ makeFnMethod (ident2 "qtah" "qcoreapplication" "create") "new" MStatic Nonpure
    [objT c_QStringList] $ ptrT $ objT c_QCoreApplication

  , just $ mkStaticMethod "addLibraryPath" [objT c_QString] voidT
  , just $ mkStaticMethod "applicationDirPath" np $ objT c_QString
  , just $ mkStaticMethod "applicationFilePath" np $ objT c_QString
  , just $ mkStaticMethod "applicationName" np $ objT c_QString
  , test (qtVersion >= [4, 4]) $ mkStaticMethod "applicationPid" np qlonglong
  , just $ mkStaticMethod "applicationVersion" np $ objT c_QString
  , test (qtVersion >= [4, 1]) $ mkStaticMethod "arguments" np $ objT c_QStringList
  , just $ mkStaticMethod "closingDown" np boolT
  -- TODO QAbstractEventDispatcher * eventDispatcher()
  , just $ mkStaticMethod "exec" np voidT
  , just $ mkStaticMethod "exit" [intT] voidT
  -- TODO bool installTranslator(QTranslator *translationFile)
  , just $ mkStaticMethod' "instance" "getInstance" np $ ptrT $ objT c_QCoreApplication
  , test (qtVersion >= [5, 0]) $ mkStaticMethod "isQuitLockEnabled" np boolT
  , test (qtVersion >= [5, 3]) $ mkStaticMethod "isSetuidAllowed" np boolT
  , just $ mkStaticMethod "libraryPaths" np $ objT c_QStringList
  , just $ mkStaticMethod "organizationDomain" np $ objT c_QString
  , just $ mkStaticMethod "organizationName" np $ objT c_QString
  , test (qtVersion >= [4, 3]) $ mkStaticMethod' "postEvent" "postEvent"
    [ptrT $ objT c_QObject, ptrT $ objT c_QEvent] voidT
  , test (qtVersion >= [4, 3]) $ mkStaticMethod' "postEvent" "postEventWithPriority"
    [ptrT $ objT c_QObject, ptrT $ objT c_QEvent, intT] voidT
  -- TODO void processEvents(QEventLoop::ProcessEventsFlags flags = QEventLoop::AllEvents)
  -- TODO void processEvents(QEventLoop::ProcessEventsFlags flags, int maxtime)
  , just $ mkStaticMethod "quit" np voidT
  , just $ mkStaticMethod "removeLibraryPath" [objT c_QString] voidT
  , just $ mkStaticMethod' "removePostedEvents" "removePostedEvents" [ptrT $ objT c_QObject] voidT
  , just $ mkStaticMethod' "removePostedEvents" "removePostedEventsWithEventType" [ ptrT $ objT c_QObject, intT] voidT
  -- TODO bool QCoreApplication::removeTranslator(QTranslator *translationFile)
  , just $ mkStaticMethod "sendEvent" [ptrT $ objT c_QObject, ptrT $ objT c_QEvent] boolT
  , just $ mkStaticMethod' "sendPostedEvents" "sendPostedEvents" np voidT
  , just $ mkStaticMethod' "sendPostedEvents" "sendPostedEventsWithObject" [ptrT $ objT c_QObject] voidT
  , just $ mkStaticMethod' "sendPostedEvents" "sendPostedEventsWithObjectAndEventType" [ ptrT $ objT c_QObject, intT] voidT
  , just $ mkStaticMethod "setApplicationName" [objT c_QString] voidT
  , just $ mkStaticMethod "setApplicationVersion" [objT c_QString] voidT
  , just $ mkStaticMethod' "setAttribute" "setAttribute" [enumT e_ApplicationAttribute] voidT
  , just $ mkStaticMethod' "setAttribute" "setAttributeWithBool" [enumT e_ApplicationAttribute, boolT] voidT
  -- TODO void setEventDispatcher(QAbstractEventDispatcher *eventDispatcher)
  , just $ mkStaticMethod "setLibraryPaths" [objT c_QStringList] voidT
  , just $ mkStaticMethod "setOrganizationDomain" [objT c_QString] voidT
  , just $ mkStaticMethod "setOrganizationName" [objT c_QString] voidT
  , test (qtVersion >= [5, 0]) $ mkStaticMethod "setQuitLockEnabled" [boolT] voidT
  , test (qtVersion >= [5, 3]) $ mkStaticMethod "setSetuidAllowed" [boolT] voidT
  , just $ mkStaticMethod "startingUp" np boolT
  , just $ mkStaticMethod "testAttribute" [enumT e_ApplicationAttribute] boolT
    -- TODO QString wrappers for these.
  , just $ makeFnMethod (ident2 "qtah" "qcoreapplication" "translate")
    "translate" MStatic Nonpure
    [objT c_QString, objT c_QString] $ objT c_QString
  , just $ makeFnMethod (ident2 "qtah" "qcoreapplication" "translate")
    "translateWithDisambiguation" MStatic Nonpure
    [objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ makeFnMethod (ident2 "qtah" "qcoreapplication" "translate")
    "translateWithDisambiguationAndNum" MStatic Nonpure
    [objT c_QString, objT c_QString, objT c_QString, intT] $ objT c_QString
  ]
