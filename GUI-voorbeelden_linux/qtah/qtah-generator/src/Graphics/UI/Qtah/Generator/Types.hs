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

module Graphics.UI.Qtah.Generator.Types (
  QtExport (..),
  qtExport,
  qtExportToExports,
  makeQtEnum,
  makeQtEnum',
  makeQtEnumAndFlags,
  makeQtEnumAndFlags',
  makeQtEnumAndFlagsWithOverrides,
  ListenerInfo (ListenerInfo),
  Signal, SignalGen, makeSignal, makeSignal', makeSignalPrivate,
  makeQtClassAndSignals,
  signalCName, signalHaskellName, signalClass, signalListenerClass, signalCallback,
  ) where

import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Foreign.Hoppy.Generator.Spec (
  Callback,
  Class,
  ClassEntity (CEMethod),
  CppEnum,
  Scoped (Unscoped),
  Export (Export),
  Exportable,
  ForeignLanguage (Haskell),
  Function,
  Identifier,
  Include,
  addReqIncludes,
  callbackParams,
  callbackReturn,
  classAddEntities,
  enumAddEntryNameOverrides,
  enumSetHasBitOperations,
  enumSetUnknownValueEntry,
  enumSetValuePrefix,
  identifierParts,
  idPartBase,
  makeAutoEnum,
  mkMethod'_,
  onParameterType,
  stripToGc,
  toExtName,
  toExport,
  )
import Graphics.UI.Qtah.Generator.Common (upperFirst)
import Graphics.UI.Qtah.Generator.Flags (Flags, makeFlags)

data QtExport =
  QtExport Export
  | QtExportFnRenamed Function String
  | QtExportClassAndSignals Class [Signal]
    -- ^ Exports a class together with signals that belong to it.  These signals
    -- should have been constructed with 'makeQtClassAndSignals' so that the
    -- class has manual emit methods.
  | QtExportEvent Class
  | QtExportSceneEvent Class
  | QtExportSpecials
    -- ^ This is a special value that is exported exactly once, and generates
    -- some bindings that need special logic.

qtExport :: Exportable a => a -> QtExport
qtExport = QtExport . toExport

qtExportToExports :: QtExport -> [Export]
qtExportToExports qtExport = case qtExport of
  QtExport export -> [export]
  QtExportFnRenamed fn _ -> [Export fn]
  QtExportClassAndSignals cls _ -> [Export cls]
  QtExportEvent cls -> [Export cls]
  QtExportSceneEvent cls -> [Export cls]
  QtExportSpecials -> []

-- | Creates a 'CppEnum' whose 'ExtName' is the concatenation of all part of its
-- 'Identifier'.  This should be used for all Qt enums.
makeQtEnum :: Identifier -> [Include] -> [String] -> CppEnum
makeQtEnum identifier includes names =
  makeQtEnum' identifier
              Unscoped  -- Most Qt enums are unscoped.
              includes
              names

-- | Creates a 'CppEnum' like 'makeQtEnum' does, but also takes a boolean
-- parameter indicating whether the enum is scoped.
makeQtEnum' :: Identifier -> Scoped -> [Include] -> [String] -> CppEnum
makeQtEnum' identifier scoped includes names =
  addReqIncludes includes $
  enumSetValuePrefix "" $
  enumSetUnknownValueEntry ("Unknown" ++ niceName) $
  enumSetHasBitOperations False $
  addEntryOverrides $
  makeAutoEnum identifier
               (Just $ toExtName niceName)
               scoped
               names
  where niceName = concatMap idPartBase $ identifierParts identifier
        addEntryOverrides = enumAddEntryNameOverrides Haskell applicableOverrides
        applicableOverrides = filter (\(from, _) -> S.member from nameSet) enumNameOverrides
        nameSet = S.fromList names

-- | Creates a 'CppEnum' and 'Flags' pair, with the same entries and related
-- names.
makeQtEnumAndFlags :: Identifier -> String -> [Include] -> [String] -> (CppEnum, Flags)
makeQtEnumAndFlags enumIdentifier flagsName includes names =
  let enum = makeQtEnum enumIdentifier includes names
      flags = makeFlags enum flagsName
  in (enum, flags)

-- | Creates a 'CppEnum' and 'Flags' pair like 'makeQtEnumAndFlags' does, but
-- also takes a boolean parameter indicating whether the enum is scoped.
makeQtEnumAndFlags' :: Identifier -> String -> Scoped -> [Include] -> [String] -> (CppEnum, Flags)
makeQtEnumAndFlags' enumIdentifier flagsName scoped includes names =
  let enum = makeQtEnum' enumIdentifier scoped includes names
      flags = makeFlags enum flagsName
  in (enum, flags)

-- | This version of 'makeQtEnumAndFlags' accepts entry name overrides, which is
-- useful because flag bindings can conflict with method names (they're both
-- Haskell identifiers starting with lower-case letters).
makeQtEnumAndFlagsWithOverrides ::
  Identifier -> String -> [Include] -> [String] -> [(String, String)] -> (CppEnum, Flags)
makeQtEnumAndFlagsWithOverrides enumIdentifier flagsName includes names nameOverrides =
  let enum = enumAddEntryNameOverrides Haskell nameOverrides $
             makeQtEnum enumIdentifier includes names
      flags = makeFlags enum flagsName
  in (enum, flags)

-- | Global enum entry name overrides.  These are applied to all enum entries,
-- to handle the cases where they overlap with Haskell keywords.
--
-- TODO Fill these out based on enums we're defined so far.
enumNameOverrides :: [(String, String)]
enumNameOverrides =
  [ ("Type", "Typ")
  ]

-- | Specification for a signal in the Qt signals and slots framework.
data Signal = Signal
  { signalClass :: Class
    -- ^ The class to which the signal belongs.
  , signalCName :: String
    -- ^ The C name of the signal, without parameters, e.g. @"clicked"@.
  , signalHaskellName :: String
    -- ^ The base name of the Haskell binding for the signal.  Normally the same
    -- as the C name.
  , signalListenerClass :: Class
    -- ^ An appropriately typed listener class.
  , signalCallback :: Callback
    -- ^ The callback type used by the listener.
  , signalPrivate :: Bool
    -- ^ Most signals can both be connected to and emitted by the user.
    -- @QObject::objectNameChanged@ for example is a \"private signal,\" which
    -- can be connected to but not emitted manually.  For such signals, this
    -- field is true.
  }

-- | A curried function for constructing a signal that only needs the class that
-- the signal belongs to.
type SignalGen = Class -> Signal

data ListenerInfo = ListenerInfo Class Callback

-- The class is the last argument to these makeSignal functions because it is
-- curried; see qtExportClassAndSignals.

-- TODO Docs here.

-- | Constructs a signal for use with 'qtExportClassAndSignals'.  The signal can
-- be listened for via Haskell callback functions.  The constructed signal is
-- public: an "emit" method will also be added to the class for manually
-- emitting the signal.
--
-- The first argument is used both as the signal's C++ name, and the name it
-- will be given in Haskell.
makeSignal :: String  -- ^ 'signalCName'
           -> ListenerInfo  -- ^ 'signalListenerClass' and 'signalCallback'.
           -> SignalGen  -- ^ Curried function to take 'signalClass' and return the signal.
makeSignal cName (ListenerInfo listenerClass callback) cls =
  Signal cls cName cName listenerClass callback False

-- | Constructs a signal for use with 'qtExportClassAndSignals', as 'makeSignal'
-- does, except separate C++ and Haskell names may be provided.
--
-- The first argument is used both as the signal's C++ name, and the second
-- argument is the name it will be given in Haskell.  This is analogous to
-- @mkMethod@ and @mkMethod'@.
makeSignal' :: String  -- ^ 'signalCName'
            -> String  -- ^ 'signalHaskellName'
            -> ListenerInfo  -- ^ 'signalListenerClass' and 'signalCallback'.
            -> SignalGen  -- ^ Curried function to take 'signalClass' and return the signal.
makeSignal' cName hsName (ListenerInfo listenerClass callback) cls =
  Signal cls cName hsName listenerClass callback False

-- | Constructs a signal for use with 'qtExportClassAndSignals', as 'makeSignal'
-- does, except the constructed signal is private: no "emit" method is added to
-- the class for manually emitting the signal.
makeSignalPrivate ::
     String  -- ^ 'signalCName'
  -> ListenerInfo  -- ^ 'signalListenerClass' and 'signalCallback'.
  -> SignalGen  -- ^ Curried function to take 'signalClass' and return the signal.
makeSignalPrivate cName (ListenerInfo listenerClass callback) cls =
  Signal cls cName cName listenerClass callback True

-- | Combines a class with signals that belong to it.  'SignalGen' values are
-- combined with the class to produce 'Signal's, and methods for emitting
-- (public) signals manually are added in the returned class.
makeQtClassAndSignals :: [SignalGen] -> Class -> (Class, [Signal])
makeQtClassAndSignals sigs cls = (cls', sigs')
  where cls' = flip classAddEntities cls $ flip mapMaybe sigs' $ \sig ->
          if signalPrivate sig
          then Nothing
          else Just $
               let cName = signalCName sig
                   -- We prepend "emit" to the name of the Haskell method to make it clear
                   -- that it emits a signal, and to avoid collisions with other
                   -- namespaces that are distinct in C++ but shared in Haskell.  For
                   -- example, QAbstractItemView::DoubleClicked produces a 'doubleClicked'
                   -- Haskell binding; without this prefix, we would also try to generate
                   -- a method with that name.
                   hsName = "emit" ++ upperFirst (signalHaskellName sig)
                   callback = signalCallback sig
                   -- We have to strip toGcT off of callback parameters.  It makes sense
                   -- in callback arguments because we can have the GC manage objects the
                   -- signal listener receives, but it doesn't make sense when passing
                   -- objects *to* a manual signal emit call.  See for example
                   -- cb_QModelIndexQModelIndexQVectorIntVoid.
                   params = map (onParameterType stripToGc) $ callbackParams callback
                   retType = callbackReturn callback
               in CEMethod $ mkMethod'_ cName hsName params retType

        sigs' = map ($ cls') sigs
