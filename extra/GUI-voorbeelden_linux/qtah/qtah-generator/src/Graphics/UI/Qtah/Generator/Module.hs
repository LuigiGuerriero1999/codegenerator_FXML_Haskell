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

{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Generator.Module (
  AModule (..),
  aModuleHoppyModules,
  QtModule,
  makeQtModule,
  makeQtModuleWithMinVersion,
  qtModulePath,
  qtModuleQtExports,
  qtModuleHoppy,
  ) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.Char (toLower)
import Data.Foldable (forM_)
import Data.List (find, intersperse, sort)
import Data.Maybe (isJust)
#if !MIN_VERSION_base(4,13,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Cpp (chunkContents, execChunkWriter, sayType)
import Foreign.Hoppy.Generator.Language.Haskell (
  Generator,
  HsTypeSide (HsHsSide),
  addExport,
  addExport',
  addExports,
  addExtension,
  addImports,
  askInterface,
  cppTypeToHsTypeAndUse,
  getClassHaskellConversion,
  getModuleForExtName,
  getModuleName,
  indent,
  inFunction,
  ln,
  prettyPrint,
  sayLn,
  saysLn,
  toHsFnName',
  )
import Foreign.Hoppy.Generator.Spec (
  Class,
  Constness (Const, Nonconst),
  Ctor,
  ExtName,
  FnName (FnName),
  ForeignLanguage (Haskell),
  Function,
  Method,
  MethodImpl (RealMethod),
  Module,
  Type,
  addAddendumHaskell,
  callbackParams,
  castExport,
  classCtors,
  classEntityForeignName,
  classExtName,
  classHaskellConversionFromCppFn,
  classHaskellConversionToCppFn,
  classMethods,
  ctorExtName,
  enumValueMapNames,
  fnExtName,
  fromExtName,
  getPrimaryExtName,
  hsImport1,
  hsImports,
  hsWholeModuleImport,
  makeModule,
  methodExtName,
  methodImpl,
  moduleAddExports,
  moduleAddHaskellName,
  moduleModify',
  parameterType,
  varGetterExtName,
  varIsConst,
  varSetterExtName,
  )
import Foreign.Hoppy.Generator.Spec.Callback (callbackT)
import Foreign.Hoppy.Generator.Spec.Class (
  toHsCastMethodName',
  toHsDataTypeName',
  toHsDownCastMethodName',
  toHsPtrClassName',
  toHsValueClassName',
  )
import Foreign.Hoppy.Generator.Spec.Enum (enumGetOverriddenEntryName, enumValues, toHsEnumTypeName')
import Foreign.Hoppy.Generator.Types (objT)
import Graphics.UI.Qtah.Generator.Config (Version, qrealFloat, qtVersion)
import Graphics.UI.Qtah.Generator.Common (fromMaybeM)
import Graphics.UI.Qtah.Generator.Flags (
  flagsEnum,
  toHsFlagsBindingName',
  toHsFlagsTypeName',
  toHsFlagsTypeclassName',
  )
import Graphics.UI.Qtah.Generator.Types (
  QtExport (
    QtExport,
    QtExportClassAndSignals,
    QtExportEvent,
    QtExportFnRenamed,
    QtExportSceneEvent,
    QtExportSpecials
  ),
  Signal,
  qtExportToExports,
  signalCallback,
  signalClass,
  signalCName,
  signalHaskellName,
  signalListenerClass,
  )
import Graphics.UI.Qtah.Generator.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsQualType (HsQualType),
  HsType (HsTyApp, HsTyCon, HsTyFun, HsTyVar),
  )

-- | A union of Hoppy and Qt modules.
data AModule = AHoppyModule Module | AQtModule QtModule

aModuleHoppyModules :: AModule -> [Module]
aModuleHoppyModules (AHoppyModule m) = [m]
aModuleHoppyModules (AQtModule qm) = [qtModuleHoppy qm, qtModuleHoppyWrapper qm]

-- | A @QtModule@ (distinct from a Hoppy 'Module'), is a description of a
-- Haskell module in the @Graphics.UI.Qtah.Q@ namespace that:
--
--     1. reexports 'Export's from a Hoppy module, dropping @ClassName_@
--        prefixes from the reexported names.
--     2. generates Signal definitions for Qt signals.
data QtModule = QtModule
  { qtModulePath :: [String]
  , qtModuleQtExports :: [QtExport]
    -- ^ A list of exports whose generated Hoppy bindings will be re-exported in
    -- this module.
  , qtModuleHoppy :: Module
  , qtModuleHoppyWrapper :: Module
  }

makeQtModule :: [String] -> [QtExport] -> QtModule
makeQtModule [] _ = error "makeQtModule: Module path must be nonempty."
makeQtModule modulePath@(_:moduleNameParts) qtExports =
  let lowerName = map toLower $ concat moduleNameParts
  in QtModule
     { qtModulePath = modulePath
     , qtModuleQtExports = qtExports
     , qtModuleHoppy =
       moduleModify' (makeModule lowerName
                      (concat ["b_", lowerName, ".hpp"])
                      (concat ["b_", lowerName, ".cpp"])) $ do
         moduleAddHaskellName $ "Generated" : modulePath
         moduleAddExports $ concatMap qtExportToExports qtExports
     , qtModuleHoppyWrapper =
       addAddendumHaskell (sayWrapperModule modulePath qtExports) $
       moduleModify' (makeModule (lowerName ++ "wrap")
                      (concat ["b_", lowerName, "_w.hpp"])
                      (concat ["b_", lowerName, "_w.cpp"])) $
       moduleAddHaskellName modulePath
     }

-- | Creates a 'QtModule' (a la 'makeQtModule') that has a minimum version
-- applied to all of its contents.  If Qtah is being built against a version of
-- Qt below this minimum version, then the module will still be generated, but
-- it will be empty; the exports list will be replaced with an empty list.
makeQtModuleWithMinVersion :: [String] -> Version -> [QtExport] -> QtModule
makeQtModuleWithMinVersion modulePath minVersion qtExports =
  makeQtModule modulePath $
  if qtVersion >= minVersion then qtExports else []

sayWrapperModule :: [String] -> [QtExport] -> Generator ()
sayWrapperModule modulePath qtExports = inFunction "<Qtah generateModule>" $ do
  addExtension "NoMonomorphismRestriction"

  -- As in generated Hoppy bindings, avoid non-qualified Prelude uses in
  -- generated code here.
  addImports $ hsImports "Prelude" []

  -- Import the underlying Hoppy module wholesale.
  case concatMap qtExportToExports qtExports of
    [] -> return ()
    export:_ -> importWholeModuleForExtName $ getPrimaryExtName export

  -- Generate bindings for all of the exports.
  mapM_ (sayQtExport modulePath) qtExports

getFnImportName :: Function -> String
getFnImportName = toHsFnName' . fnExtName

getFnReexportName :: Function -> String
getFnReexportName = getFnImportName

classUpCastReexportName :: String
classUpCastReexportName = "cast"

classUpCastConstReexportName :: String
classUpCastConstReexportName = "castConst"

classDownCastReexportName :: String
classDownCastReexportName = "downCast"

classDownCastConstReexportName :: String
classDownCastConstReexportName = "downCastConst"

classEncodeReexportName :: String
classEncodeReexportName = "encode"

classDecodeReexportName :: String
classDecodeReexportName = "decode"

getCtorReexportName :: Ctor -> String
getCtorReexportName = toHsFnName' . ctorExtName

getMethodReexportName :: Method -> String
getMethodReexportName = toHsFnName' . methodExtName

sayClassEncodingFnReexports :: Class -> Generator ()
sayClassEncodingFnReexports cls = inFunction "sayClassEncodingFnReexports" $ do
  let conv = getClassHaskellConversion cls

  forM_ (classHaskellConversionToCppFn conv) $ \_ -> do
    hsHsType <- cppTypeToHsTypeAndUse HsHsSide (objT cls)
    let dataTypeName = toHsDataTypeName' Nonconst cls
        ptrHsType = HsTyCon $ UnQual $ HsIdent dataTypeName
        encodeFnType = HsTyFun hsHsType $ HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahP.IO") ptrHsType
    addImports $ mconcat [importForPrelude, importForRuntime]
    ln
    saysLn [classEncodeReexportName, " :: ", prettyPrint encodeFnType]
    saysLn [classEncodeReexportName, " = QtahFHR.encodeAs (QtahP.undefined :: ", dataTypeName, ")"]

  forM_ (classHaskellConversionFromCppFn conv) $ \_ -> do
    hsHsType <- cppTypeToHsTypeAndUse HsHsSide (objT cls)
    let constPtrClassName = toHsPtrClassName' Const cls
        thisTyVar = HsTyVar $ HsIdent "this"
        decodeFnType = HsQualType [(UnQual $ HsIdent constPtrClassName, [thisTyVar])] $
                       HsTyFun thisTyVar $
                       HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahP.IO") hsHsType
    addImports $ mconcat [importForPrelude, importForRuntime]
    ln
    saysLn [classDecodeReexportName, " :: ", prettyPrint decodeFnType]
    saysLn [classDecodeReexportName, " = QtahFHR.decode QtahP.. ", toHsCastMethodName' Const cls]

handleEventKind :: [String] -> String -> Class -> Generator ()
handleEventKind path eventKind cls = do
  let typeName = toHsDataTypeName' Nonconst cls
  addImports $ hsImport1 "Prelude" "($)"
  ln
  saysLn ["instance Qtah", eventKind, ".", eventKind, " ", typeName, " where"]
  indent $ do
    saysLn ["on", eventKind, " receiver' handler' = Qtah", eventKind,
              ".onAny", eventKind, " receiver' $ \\_ qevent' ->"]
    indent $
      if path == ["Core", "QEvent"]
      then sayLn "handler' qevent'"
      else do
        addImports $ mconcat [hsImport1 "Prelude" "(==)",
                              importForPrelude,
                              importForRuntime]
        saysLn ["let event' = ", classDownCastReexportName, " qevent'"]
        sayLn "in if event' == QtahFHR.nullptr then QtahP.return QtahP.False else handler' event'"

sayQtExport :: [String] -> QtExport -> Generator ()
sayQtExport path qtExport = case qtExport of
  QtExport export -> doExport export

  QtExportFnRenamed fn rename -> do
    addExport rename
    sayBind rename $ getFnImportName fn

  QtExportClassAndSignals cls sigs -> do
    sayExportClass cls
    mapM_ sayExportSignal sigs

  QtExportEvent cls -> do
    sayExportClass cls

    addImports $ mconcat [importForEvent, importForSceneEvent]
    handleEventKind path "Event" cls
    handleEventKind path "SceneEvent" cls

  QtExportSceneEvent cls -> do
    sayExportClass cls

    addImports importForSceneEvent
    handleEventKind path "SceneEvent" cls

  QtExportSpecials -> do
    -- Generate a type synonym for qreal.
    addImports importForPrelude
    addExport "QReal"
    ln
    saysLn ["type QReal = ", if qrealFloat then "QtahP.Float" else "QtahP.Double"]

  where doExport export = case castExport export of
          Just c -> doExportClass c
          Nothing -> case castExport export of
            Just e -> doExportEnum e
            Nothing -> case castExport export of
              Just flags -> doExportFlags flags
              Nothing -> case castExport export of
                Just f -> doExportFunction f
                Nothing -> case castExport export of
                  Just v -> doExportVariable v
                  Nothing -> return ()

        doExportClass cls = sayExportClass cls

        doExportEnum e = do
          let spec = toHsEnumTypeName' e ++ " (..)"
          addExport spec

        doExportFlags flags = do
          let enum = flagsEnum flags
              typeName = toHsFlagsTypeName' flags
              typeclassName = toHsFlagsTypeclassName' flags
          -- Re-export the data type and typeclass.
          addExport typeName
          addExport' typeclassName
          -- Re-export the entries' bindings.
          forM_ (enumValueMapNames $ enumValues enum) $ \words -> do
            let words' = enumGetOverriddenEntryName Haskell enum words
            addExport $ toHsFlagsBindingName' flags words'

        doExportFunction f = addExport $ getFnReexportName f

        doExportVariable v = do
          addExport $ toHsFnName' $ varGetterExtName v
          unless (varIsConst v) $ addExport $ toHsFnName' $ varSetterExtName v

sayExportClass :: Class -> Generator ()
sayExportClass cls = do
  addExports $
    (toHsValueClassName' cls ++ " (..)") :
    (toHsPtrClassName' Const cls ++ " (..)") :
    (toHsPtrClassName' Nonconst cls ++ " (..)") :
    toHsDataTypeName' Const cls :
    toHsDataTypeName' Nonconst cls :
    classUpCastConstReexportName :
    classUpCastReexportName :
    classDownCastConstReexportName :
    classDownCastReexportName :
    concat [ if isJust $ classHaskellConversionToCppFn $ getClassHaskellConversion cls
             then [classEncodeReexportName]
             else []
           , if isJust $ classHaskellConversionFromCppFn $ getClassHaskellConversion cls
             then [classDecodeReexportName]
             else []
           , sort $ map getCtorReexportName $ classCtors cls
           , sort $ map getMethodReexportName $ classMethods cls
           ]

  ln
  sayBind classUpCastConstReexportName $ toHsCastMethodName' Const cls
  sayBind classUpCastReexportName $ toHsCastMethodName' Nonconst cls
  sayBind classDownCastConstReexportName $ toHsDownCastMethodName' Const cls
  sayBind classDownCastReexportName $ toHsDownCastMethodName' Nonconst cls
  sayClassEncodingFnReexports cls
  -- Class constructors and methods don't need to be rebound, because their
  -- names don't change.

-- | Generates and exports a @Signal@ definition.  We create the signal from
-- scratch in this module, rather than reexporting it from somewhere else.
sayExportSignal :: Signal -> Generator ()
sayExportSignal signal = inFunction "sayExportSignal" $ do
  let name = signalCName signal
      cls = signalClass signal
      ptrClassName = toHsPtrClassName' Nonconst cls
      varName = toSignalBindingName signal
  addExport varName

  let listenerClass = signalListenerClass signal
  importWholeModuleForExtName $ classExtName listenerClass

  -- Find the listener constructor that only takes a callback.
  listenerCtor <-
    fromMaybeM (throwError $ concat
                ["Couldn't find an appropriate ",
                show (fromExtName $ classExtName listenerClass),
                " constructor for signal ", show name]) $
    flip find (classCtors listenerClass) $ \ctor -> fromExtName (ctorExtName ctor) == "new"
  let callback = signalCallback signal
      paramTypes = map parameterType $ callbackParams callback

  -- Also find the 'isValid' method.
  isValidMethod <-
    fromMaybeM (throwError $ concat
                ["Couldn't find the isValid method in ",
                 show listenerClass, " for signal ", show name]) $
    find ((RealMethod (FnName "isValid") ==) . methodImpl) $ classMethods listenerClass

  callbackHsType <- cppTypeToHsTypeAndUse HsHsSide $ callbackT callback

  let varType = HsQualType [(UnQual $ HsIdent ptrClassName, [HsTyVar $ HsIdent "object"])] $
                HsTyApp (HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahSignal.Signal") $
                         HsTyVar $ HsIdent "object")
                callbackHsType
      internalName = concat
                     [ fromExtName $ classExtName cls
                     , "::"
                     , name
                     , " ("
                     , fromExtName $ classExtName listenerClass
                     , ")"
                     ]

  addImports $ mconcat [hsImports "Prelude" ["($)", "(>>)"],
                        importForPrelude,
                        importForRuntime,
                        importForSignal]
  ln
  saysLn [varName, " :: ", prettyPrint varType]
  saysLn [varName, " = QtahSignal.Signal"]
  indent $ do
    sayLn "{ QtahSignal.internalConnectSignal = \\object' fn' -> do"
    indent $ do
      saysLn ["listener' <- ",
              toHsFnName' $ classEntityForeignName listenerClass listenerCtor,
              " object' ",
              show (toSignalConnectName signal paramTypes),
              " fn'"]
      saysLn ["valid' <- ",
              toHsFnName' $ classEntityForeignName listenerClass isValidMethod,
              " listener'"]
      sayLn "if valid'"
      indent $ do
        sayLn "then QtahP.fmap QtahP.Just $ QtahSignal.internalMakeConnection listener'"
        sayLn "else QtahFHR.delete listener' >> QtahP.return QtahP.Nothing"
    saysLn [", QtahSignal.internalName = ", show internalName]
    sayLn "}"

sayBind :: String -> String -> Generator ()
sayBind name value = saysLn [name, " = ", value]

toSignalBindingName :: Signal -> String
toSignalBindingName = (++ "Signal") . signalHaskellName

toSignalConnectName :: Signal -> [Type] -> String
toSignalConnectName signal paramTypes =
  concat $
  "2" :  -- This is a magic code added by the SIGNAL() macro.
  signalCName signal :
  "(" :
  intersperse "," (map (chunkContents . execChunkWriter . sayType Nothing) paramTypes) ++
  [")"]

importWholeModuleForExtName :: ExtName -> Generator ()
importWholeModuleForExtName extName = do
  iface <- askInterface
  addImports . hsWholeModuleImport . getModuleName iface =<< getModuleForExtName extName
