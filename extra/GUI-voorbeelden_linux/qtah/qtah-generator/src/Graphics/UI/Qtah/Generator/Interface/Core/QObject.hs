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

module Graphics.UI.Qtah.Generator.Interface.Core.QObject (
  aModule,
  c_QObject,
  ) where

import Foreign.Hoppy.Generator.Spec (
  (~:),
  MethodApplicability (MConst, MNormal, MStatic),
  Purity (Nonpure),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkConstMethod',
  mkStaticMethod',
  mkCtor,
  mkMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, constT, intT, objT, ptrT, refT, ptrT, voidT, charT, enumT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QList (
  c_QListQByteArray,
  c_QListQObject,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.QMetaObject (c_QMetaObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QMetaObject.Connection (c_Connection)
import Graphics.UI.Qtah.Generator.Interface.Core.QMetaMethod (c_QMetaMethod)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Foreign.Hoppy.Generator.Std.String (c_string)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QThread (c_QThread)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listenerPtrQObject,
  listenerQString,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_ConnectionType)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QObject"] $
  [ QtExportClassAndSignals c_QObject signals ]

(c_QObject, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [ includeStd "QObject"
                 , includeLocal "wrap_qobject.hpp"
                 ] $
  classSetEntityPrefix "" $
  makeClass (ident "QObject") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" ["parent" ~: ptrT $ objT c_QObject]
  , just $ mkMethod "blockSignals" ["block" ~: boolT] boolT
  , just $ mkMethod "children" np $ objT c_QListQObject

    -- TODO QStrings instead of const char*.
  , just $ mkConstMethod' "connect" "connect" [ptrT $ constT $ objT c_QObject, ptrT $ constT charT, ptrT $ constT charT] $ objT c_Connection
  , just $ mkConstMethod' "connect" "connectWithType" [ptrT $ constT $ objT c_QObject, ptrT $ constT charT, ptrT $ constT charT, enumT e_ConnectionType] $ objT c_Connection

  , just $ mkStaticMethod' "connect" "connectWithTypeStatic" [ptrT $ constT $ objT c_QObject, ptrT $ constT charT, ptrT $ constT $ objT c_QObject, ptrT $ constT charT, enumT e_ConnectionType] $ objT c_Connection
  , just $ mkStaticMethod' "connect" "connectWithSenderSignalStatic" [ptrT $ constT $ objT c_QObject, refT $ constT $ objT c_QMetaMethod, ptrT $ constT $ objT c_QObject, refT $ constT $ objT c_QMetaMethod] $ objT c_Connection
  , just $ mkStaticMethod' "connect" "connectWithSenderSignalTypeStatic" [ptrT $ constT $ objT c_QObject, refT $ constT $ objT c_QMetaMethod, ptrT $ constT $ objT c_QObject, refT $ constT $ objT c_QMetaMethod, enumT e_ConnectionType] $ objT c_Connection

  , just $ makeFnMethod (ident2 "qtah" "qobject" "connect") "connectStatic" MStatic Nonpure
    [ptrT $ constT $ objT c_QObject, objT c_string, ptrT $ constT $ objT c_QObject, objT c_string] $ objT c_Connection
  , just $ makeFnMethod (ident2 "qtah" "qobject" "disconnect") "disconnectStatic" MStatic Nonpure
    [ptrT $ constT $ objT c_QObject, objT c_string, ptrT $ constT $ objT c_QObject, objT c_string] boolT

  , just $ mkMethod "deleteLater" np voidT

  , just $ mkConstMethod' "disconnect" "disconnect" np boolT
  , just $ mkConstMethod' "disconnect" "disconnectWithReceiver" [ptrT $ constT $ objT c_QObject] boolT
  , just $ mkConstMethod' "disconnect" "disconnectWithReceiverAndMethod" [ptrT $ constT $ objT c_QObject, ptrT $ constT charT] boolT
  , just $ mkConstMethod' "disconnect" "disconnectWithSignal" [ptrT $ constT charT] boolT
  , just $ mkConstMethod' "disconnect" "disconnectWithSignalAndReceiver" [ptrT $ constT charT, ptrT $ constT $ objT c_QObject] boolT
  , just $ mkConstMethod' "disconnect" "disconnectWithSignalAndReceiverAndMethod" [ptrT $ constT charT, ptrT $ constT $ objT c_QObject, ptrT $ constT charT] boolT
  , just $ mkStaticMethod' "disconnect" "disconnectWithMetaobject" [objT c_Connection] boolT

  , just $ mkMethod "dumpObjectInfo" np voidT
  , just $ mkMethod "dumpObjectTree" np voidT
  , just $ mkConstMethod "dynamicPropertyNames" np $ objT c_QListQByteArray
  , just $ mkMethod "event" ["event" ~: ptrT $ objT c_QEvent] boolT
  , just $ mkMethod "eventFilter" ["watched" ~: ptrT $ objT c_QObject, "event" ~: ptrT $ objT c_QEvent] boolT
    -- TODO findChild
    -- TODO findChildren
  , just $ makeFnMethod (ident2 "qtah" "qobject" "inherits") "inherits" MConst Nonpure
    ["" ~: objT c_QObject, "className" ~: objT c_QString] boolT
  , just $ mkMethod "installEventFilter" ["filterObj" ~: ptrT $ objT c_QObject] voidT
  , just $ mkConstMethod "isWidgetType" np boolT
  , -- This is a guess on the version bound.
    test (qtVersion >= [5, 0]) $ mkConstMethod "isWindowType" np boolT
  , just $ mkMethod "killTimer" ["id" ~: intT] voidT
  , just $ mkConstMethod "metaObject" np $ ptrT $ constT $ objT c_QMetaObject
  , just $ mkMethod "moveToThread" ["targetThread" ~: ptrT $ objT c_QThread] voidT
  , just $ mkProp "objectName" $ objT c_QString
  , just $ mkProp "parent" $ ptrT $ objT c_QObject
  , just $ makeFnMethod (ident2 "qtah" "qobject" "property") "property" MConst Nonpure
    ["" ~: objT c_QObject, "name" ~: objT c_QString] $ objT c_QVariant
  , just $ mkMethod "removeEventFilter" ["obj" ~: ptrT $ objT c_QObject] voidT
  , just $ makeFnMethod (ident2 "qtah" "qobject" "setProperty") "setProperty" MNormal Nonpure
    ["" ~: refT $ objT c_QObject, "name" ~: objT c_QString, "value" ~: objT c_QVariant] voidT
  , just $ mkConstMethod "signalsBlocked" np boolT
  , just $ mkMethod "startTimer" ["interval" ~: intT] intT
  , just $ mkConstMethod "thread" np $ ptrT $ objT c_QThread
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "destroyed" listenerPtrQObject
  , makeSignalPrivate "objectNameChanged" listenerQString
  ]
