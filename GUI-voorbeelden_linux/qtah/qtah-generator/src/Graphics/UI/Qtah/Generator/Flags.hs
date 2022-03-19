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

module Graphics.UI.Qtah.Generator.Flags (
  -- * Data type
  Flags, flagsT,
  -- * Construction
  makeFlags,
  -- * Properties
  flagsExtName,
  flagsIdentifier,
  flagsEnum,
  flagsReqs,
  flagsAddendum,
  -- * Haskell generator
  -- ** Names
  toHsFlagsTypeName',
  toHsFlagsTypeclassName',
  toHsFlagsBindingName,
  toHsFlagsBindingName',
  ) where

import Control.Monad (forM_, when)
import Control.Monad.Except (throwError)
import qualified Data.Map as M
import qualified Foreign.Hoppy.Generator.Language.Cpp as LC
import qualified Foreign.Hoppy.Generator.Language.Haskell as LH
import Foreign.Hoppy.Generator.Spec (
  Addendum,
  Constness (Nonconst),
  ConversionMethod (CustomConversion),
  ConversionSpec,
  Exportable,
  ExtName,
  ForeignLanguage (Haskell),
  HasAddendum,
  HasExtNames,
  HasReqs,
  Identifier,
  Reqs,
  Type,
  conversionSpecCppConversionFromCppExpr,
  conversionSpecCppConversionToCppExpr,
  conversionSpecCppConversionType,
  conversionSpecHaskell,
  conversionSpecHaskellHsArgType,
  evaluatedEnumNumericType,
  evaluatedEnumValueMap,
  getAddendum,
  getPrimaryExtName,
  getReqs,
  hsImport1,
  hsImports,
  identifierParts,
  idPartBase,
  makeConversionSpec,
  makeConversionSpecCpp,
  makeConversionSpecHaskell,
  makeIdentifier,
  makeIdPart,
  modifyAddendum,
  modifyReqs,
  numType,
  sayExportCpp,
  sayExportHaskell,
  setAddendum,
  setReqs,
  toExtName,
  )
import qualified Foreign.Hoppy.Generator.Spec.Enum as Enum
import Foreign.Hoppy.Generator.Types (manualT)
import Graphics.UI.Qtah.Generator.Common (lowerFirst, replaceLast)
import Graphics.UI.Qtah.Generator.Interface.Imports (
  importForBits,
  importForFlags,
  importForPrelude,
  importForRuntime,
  )
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsQualType (HsQualType),
  HsType (HsTyCon, HsTyVar),
  )

-- | This is an exportable wrapper around a 'Enum.CppEnum' that also generates
-- support for a @QFlags\<Enum\>@ typedef.
--
-- This does not export any ExtNames of its own.
--
-- In generated Haskell code, in addition to what is generated for the
-- 'Enum.CppEnum', we generate a newtype wrapper around an enum value to
-- represent a combination of flags, and an @IsXXX@ typeclass for converting
-- various types (flags type, enum type, raw number) to a newtype'd value.
data Flags = Flags
  { flagsExtName :: ExtName
  , flagsIdentifier :: Identifier
  , flagsEnum :: Enum.CppEnum
  , flagsReqs :: Reqs
  , flagsAddendum :: Addendum
  }

instance Show Flags where
  show flags =
    "<Flags " ++
    show (flagsExtName flags) ++ " " ++
    LC.renderIdentifier (flagsIdentifier flags) ++ ">"

instance HasAddendum Flags where
  getAddendum = flagsAddendum
  setAddendum a flags = flags { flagsAddendum = a }
  modifyAddendum f flags = flags { flagsAddendum = f $ flagsAddendum flags }

instance HasExtNames Flags where
  getPrimaryExtName = flagsExtName

instance HasReqs Flags where
  getReqs = flagsReqs
  setReqs r flags = flags { flagsReqs = r }
  modifyReqs f flags = flags { flagsReqs = f $ flagsReqs flags }

instance Exportable Flags where
  -- Nothing to generate for flags here.  (Enums don't have any generated C++
  -- code here either.)
  sayExportCpp _ _ = return ()

  sayExportHaskell mode flags = sayHsExport mode flags

makeFlags :: Enum.CppEnum -> String -> Flags
makeFlags enum flagsName =
  let identifierWords =
        replaceLast flagsName $ map idPartBase $ identifierParts $ Enum.enumIdentifier enum
      identifier = makeIdentifier $ map (\s -> makeIdPart s Nothing) identifierWords
  in Flags
     { flagsExtName = toExtName $ concat identifierWords
     , flagsIdentifier = identifier
     , flagsEnum = enum
     , flagsReqs = Enum.enumReqs enum  -- Copy reqs from the underlying enum.
     , flagsAddendum = mempty
     }

flagsT :: Flags -> Type
flagsT = manualT . makeConversion

makeConversion :: Flags -> ConversionSpec
makeConversion flags =
  (makeConversionSpec (show flags) cpp)
  { conversionSpecHaskell = Just hs }
  where extName = flagsExtName flags
        identifier = flagsIdentifier flags
        identifierStr = LC.renderIdentifier identifier
        enum = flagsEnum flags

        cpp =
          (makeConversionSpecCpp identifierStr (return $ Enum.enumReqs enum))
          { conversionSpecCppConversionType =
              Just . numType . evaluatedEnumNumericType <$>
              Enum.cppGetEvaluatedEnumData (Enum.enumExtName enum)

          , conversionSpecCppConversionToCppExpr = Just $ \fromVar maybeToVar -> case maybeToVar of
              Just toVar ->
                LC.says [identifierStr, " "] >> toVar >> LC.say "(" >> fromVar >> LC.say ");\n"
              Nothing -> LC.says [identifierStr, "("] >> fromVar >> LC.say ")"

          , conversionSpecCppConversionFromCppExpr = Just $ \fromVar maybeToVar -> do
              t <-
                numType . evaluatedEnumNumericType <$>
                Enum.cppGetEvaluatedEnumData (Enum.enumExtName enum)
              forM_ maybeToVar $ \toVar -> do
                LC.sayType Nothing t
                LC.say " "
                toVar
                LC.say " = "
              LC.say "static_cast<"
              LC.sayType Nothing t
              LC.say ">("
              fromVar
              LC.say $ case maybeToVar of
                Just _ -> ");\n"
                Nothing -> ")"
          }

        hs =
          (makeConversionSpecHaskell
             (HsTyCon . UnQual . HsIdent <$> LH.toHsTypeName Nonconst extName)
             (Just $ do evaluatedData <- Enum.hsGetEvaluatedEnumData $ Enum.enumExtName enum
                        LH.cppTypeToHsTypeAndUse LH.HsCSide $
                          numType $ evaluatedEnumNumericType evaluatedData)
             (CustomConversion $ do
                LH.addImports $ mconcat [hsImport1 "Prelude" "(.)",
                                         importForFlags,
                                         importForPrelude]
                convertFn <- toHsFlagsConvertFnName flags
                LH.saysLn ["QtahP.return . QtahFlags.flagsToNum . ", convertFn])
             (CustomConversion $ do
                LH.addImports $ mconcat [hsImport1 "Prelude" "(.)",
                                         importForFlags,
                                         importForPrelude]
                LH.sayLn "QtahP.return . QtahFlags.numToFlags"))
          { conversionSpecHaskellHsArgType = Just $ \typeVar -> do
              typeclassName <- toHsFlagsTypeclassName flags
              return $
                HsQualType [(UnQual $ HsIdent typeclassName, [HsTyVar typeVar])] $
                HsTyVar typeVar
          }

sayHsExport :: LH.SayExportMode -> Flags -> LH.Generator ()
sayHsExport mode flags =
  LH.withErrorContext ("generating " ++ show flags) $ do

  -- Ensure that the flags is exported from the same module as its underlying
  -- enum.  We always want this to be the case.
  checkInFlagsEnumModule

  case mode of
    LH.SayExportForeignImports -> return ()

    LH.SayExportDecls -> do
      typeName <- toHsFlagsTypeName flags
      typeclassName <- toHsFlagsTypeclassName flags
      convertFnName <- toHsFlagsConvertFnName flags
      -- We'll use the type name as the data constructor name as well:
      let ctorName = typeName
          enum = flagsEnum flags
      enumTypeName <- Enum.toHsEnumTypeName enum
      enumData <- Enum.hsGetEvaluatedEnumData $ Enum.enumExtName enum
      numericType <-
        LH.cppTypeToHsTypeAndUse LH.HsCSide $ numType $ evaluatedEnumNumericType enumData
      let numericTypeStr = LH.prettyPrint numericType

      -- Emit the newtype wrapper.
      LH.addExport typeName
      LH.addImports $ mconcat [hsImports "Prelude" ["($)", "(.)"],
                               hsImports "Data.Bits" ["(.&.)", "(.|.)"],
                               importForBits,
                               importForFlags,
                               importForPrelude,
                               importForRuntime]
      LH.ln
      LH.saysLn ["newtype ", typeName, " = ", ctorName, " (", numericTypeStr,
                 ") deriving (QtahP.Eq, QtahP.Ord, QtahP.Show)"]

      -- Emit the Flags instance.
      LH.ln
      LH.saysLn ["instance QtahFlags.Flags (", numericTypeStr, ") ",
                 enumTypeName, " ", typeName, " where"]
      LH.indent $ do
        LH.saysLn ["enumToFlags = ", ctorName, " . QtahFHR.fromCppEnum"]
        LH.saysLn ["flagsToEnum (", ctorName, " x') = QtahFHR.toCppEnum x'"]

      -- Emit an IsXXX typeclass with a method to convert arguments to flag
      -- values.
      LH.addExport' typeclassName
      LH.ln
      LH.saysLn ["class ", typeclassName, " a where"]
      LH.indent $ do
        LH.saysLn [convertFnName, " :: a -> ", typeName]

      -- Emit IsXXX instances for the flags, enum, and numeric types.
      LH.ln
      LH.saysLn ["instance ", typeclassName, " ", typeName,
                 " where ", convertFnName, " = QtahP.id"]
      LH.saysLn ["instance ", typeclassName, " ", enumTypeName,
                 " where ", convertFnName, " = QtahFlags.enumToFlags"]
      LH.saysLn ["instance ", typeclassName, " (", numericTypeStr,
                 ") where ", convertFnName, " = QtahFlags.numToFlags"]

      -- Emit Haskell bindings for flags entries.
      forM_ (M.toList $ evaluatedEnumValueMap enumData) $ \(words, num) -> do
        let words' = Enum.enumGetOverriddenEntryName Haskell enum words
        bindingName <- toHsFlagsBindingName flags words'
        LH.addExport bindingName
        LH.ln
        LH.saysLn [bindingName, " :: ", typeName]
        LH.saysLn [bindingName, " = ", ctorName, " (", show num, ")"]

      -- Emit the Bits instance.  This code is the same as what Hoppy uses to
      -- emit enum Bits instances.
      LH.ln
      LH.saysLn ["instance QtahDB.Bits ", typeName, " where"]
      LH.indent $ do
        let fun1 f =
              LH.saysLn [f, " x = QtahFlags.numToFlags $ QtahDB.",
                         f, " $ QtahFlags.flagsToNum x"]
            fun1Int f =
              LH.saysLn [f, " x i = QtahFlags.numToFlags $ QtahDB.",
                         f, " (QtahFlags.flagsToNum x) i"]
            fun2 f =
              LH.saysLn [f, " x y = QtahFlags.numToFlags $ QtahDB.",
                         f, " (QtahFlags.flagsToNum x) (QtahFlags.flagsToNum y)"]
            op2 op =
              LH.saysLn ["x ", op, " y = QtahFlags.numToFlags ",
                         "(QtahFlags.flagsToNum x ", op, " QtahFlags.flagsToNum y)"]
        op2 ".&."
        op2 ".|."
        fun2 "xor"
        fun1 "complement"
        fun1Int "shift"
        fun1Int "rotate"
        LH.sayLn "bitSize x = case QtahDB.bitSizeMaybe x of"
        LH.indent $ do
          LH.sayLn "  QtahP.Just n -> n"
          -- Same error message as the prelude here:
          LH.sayLn "  QtahP.Nothing -> QtahP.error \"bitSize is undefined\""
        LH.sayLn "bitSizeMaybe = QtahDB.bitSizeMaybe . QtahFlags.flagsToNum"
        LH.sayLn "isSigned = QtahDB.isSigned . QtahFlags.flagsToNum"
        LH.sayLn "testBit x i = QtahDB.testBit (QtahFlags.flagsToNum x) i"
        LH.sayLn "bit = QtahFlags.numToFlags . QtahDB.bit"
        LH.sayLn "popCount = QtahDB.popCount . QtahFlags.flagsToNum"

    LH.SayExportBoot -> do
      -- Emit a minimal version of the regular binding code.
      typeName <- toHsFlagsTypeName flags
      typeclassName <- toHsFlagsTypeclassName flags
      convertFnName <- toHsFlagsConvertFnName flags
      -- We'll use the type name as the data constructor name as well:
      let ctorName = typeName
          enum = flagsEnum flags
      enumTypeName <- Enum.toHsEnumTypeName enum
      enumData <- Enum.hsGetEvaluatedEnumData $ Enum.enumExtName enum
      numericType <-
        LH.cppTypeToHsTypeAndUse LH.HsCSide $ numType $ evaluatedEnumNumericType enumData
      let numericTypeStr = LH.prettyPrint numericType

      LH.addImports $ mconcat [importForBits, importForFlags, importForPrelude]
      LH.ln
      LH.addExport typeName
      LH.saysLn ["newtype ", typeName, " = ", ctorName, " (", numericTypeStr, ")"]
      LH.saysLn ["instance QtahDB.Bits ", typeName]
      LH.saysLn ["instance QtahP.Eq ", typeName]
      LH.saysLn ["instance QtahP.Ord ", typeName]
      LH.saysLn ["instance QtahP.Show ", typeName]
      LH.ln
      LH.saysLn ["instance QtahFlags.Flags (", numericTypeStr, ") ", enumTypeName, " ", typeName]
      LH.ln
      LH.addExport' typeclassName
      LH.saysLn ["class ", typeclassName, " a where"]
      LH.indent $ do
        LH.saysLn [convertFnName, " :: a -> ", typeName]
      LH.ln
      LH.saysLn ["instance ", typeclassName, " ", typeName]
      LH.saysLn ["instance ", typeclassName, " ", enumTypeName]
      LH.saysLn ["instance ", typeclassName, " ", numericTypeStr]
      LH.ln
      forM_ (M.toList $ evaluatedEnumValueMap enumData) $ \(words, _) -> do
        let words' = Enum.enumGetOverriddenEntryName Haskell enum words
        bindingName <- toHsFlagsBindingName flags words'
        LH.addExport bindingName
        LH.saysLn [bindingName, " :: ", typeName]

    where checkInFlagsEnumModule = do
            currentModule <- LH.askModule
            enumModule <- LH.getExtNameModule $ Enum.enumExtName $ flagsEnum flags
            when (currentModule /= enumModule) $
              throwError $ show flags ++ " and " ++ show (flagsEnum flags) ++
              "are not exported from the same module."

-- | Imports and returns the Haskell type name for a 'Flags'.
toHsFlagsTypeName :: Flags -> LH.Generator String
toHsFlagsTypeName flags =
  LH.inFunction "toHsFlagsTypeName" $
  LH.addExtNameModule (flagsExtName flags) $ toHsFlagsTypeName' flags

-- | Pure version of 'toHsTypeName' that doesn't create a qualified name.
toHsFlagsTypeName' :: Flags -> String
toHsFlagsTypeName' = LH.toHsTypeName' Nonconst . flagsExtName

-- | Imports and returns the Haskell \"IsFooFlags\" typeclass for a 'Flags'.
toHsFlagsTypeclassName :: Flags -> LH.Generator String
toHsFlagsTypeclassName flags =
  LH.inFunction "toHsFlagsTypeclassName" $
  LH.addExtNameModule (flagsExtName flags) $ toHsFlagsTypeclassName' flags

-- | Pure version of 'toHsFlagsTypeclassName' that doesn't create a qualified
-- name.
toHsFlagsTypeclassName' :: Flags -> String
toHsFlagsTypeclassName' flags = 'I':'s':toHsFlagsTypeName' flags

-- | Imports and returns the Haskell \"toFooFlags\" typeclass method for a
-- 'Flags', in the typeclass named with 'toHsFlagsTypeclassName'.
toHsFlagsConvertFnName :: Flags -> LH.Generator String
toHsFlagsConvertFnName flags =
  LH.inFunction "toHsFlagsConvertFnName" $
  LH.addExtNameModule (flagsExtName flags) $ toHsFlagsConvertFnName' flags

-- | Pure version of 'toHsFlagsConvertFnName' that doesn't create a qualified
-- name.
toHsFlagsConvertFnName' :: Flags -> String
toHsFlagsConvertFnName' flags = 't':'o':toHsFlagsTypeName' flags

-- | Constructs the name of the binding for a specific flags entry.
--
-- This is the equivalent enum data constructor name, converted to a valid
-- binding name by lower-casing the first letter.
toHsFlagsBindingName :: Flags -> [String] -> LH.Generator String
toHsFlagsBindingName flags words =
  LH.inFunction "toHsFlagsBindingName" $
  LH.addExtNameModule (flagsExtName flags) $ toHsFlagsBindingName' flags words

-- | Pure version of 'toHsFlagsBindingName' that doesn't create a qualified
-- name.
toHsFlagsBindingName' :: Flags -> [String] -> String
toHsFlagsBindingName' flags words =
  lowerFirst $ Enum.toHsEnumCtorName' (flagsEnum flags) words
