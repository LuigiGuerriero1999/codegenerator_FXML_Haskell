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

module Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamReader (
  aModule,
  c_QXmlStreamReader,
  e_Error,
  e_ReadElementTextBehaviour,
  e_TokenType,
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
  mkMethod',
  mkMethod,
  mkProp,
  np,
  )
--import Graphics.UI.Qtah.Generator.Interface.Core.QStringRef (c_QStringRef)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice)
import Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamNamespaceDeclaration (c_QXmlStreamNamespaceDeclaration, qXmlStreamNamespaceDeclarations)
import Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamEntityDeclaration (qXmlStreamEntityDeclarations)
import Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamEntityResolver (c_QXmlStreamEntityResolver)
import Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamNotationDeclaration (qXmlStreamNotationDeclarations)
import Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamAttributes (c_QXmlStreamAttributes)
import Foreign.Hoppy.Generator.Types (boolT, charT, voidT, enumT, constT, objT, ptrT, refT, toGcT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qint64)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QXmlStreamReader"]
  [ qtExport c_QXmlStreamReader
  , qtExport e_Error
  , qtExport e_ReadElementTextBehaviour
  , qtExport e_TokenType
  ]

c_QXmlStreamReader =
  addReqIncludes [ includeStd "QXmlStreamReader" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QXmlStreamReader") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithPtrChar" [ptrT $ constT charT]
  , just $ mkCtor "newWithString" [refT $ constT $ objT c_QString]
  , just $ mkCtor "newWithByteArray" [refT $ constT $ objT c_QByteArray]
  , just $ mkCtor "newWithIODevice" [ptrT $ objT c_QIODevice]
  , just $ mkProp "namespaceProcessing" boolT
  , just $ mkMethod' "addData" "addDataWithByteArray" [refT $ constT $ objT c_QByteArray] voidT
  , just $ mkMethod' "addData" "addDataWithString" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "addData" "addDataWithPtrChar" [ptrT $ constT charT] voidT
  , just $ mkMethod "addExtraNamespaceDeclaration" [refT $ constT $ objT c_QXmlStreamNamespaceDeclaration] voidT
  , just $ mkMethod "addExtraNamespaceDeclarations" [refT $ constT $ objT qXmlStreamNamespaceDeclarations] voidT
  , just $ mkConstMethod "atEnd" np boolT
  , just $ mkConstMethod "attributes" np $ toGcT $ objT c_QXmlStreamAttributes
  , just $ mkConstMethod "characterOffset" np qint64
  , just $ mkMethod "clear" np voidT
  , just $ mkConstMethod "columnNumber" np qint64
  , just $ mkProp "device" $ ptrT $ objT c_QIODevice
  --, just $ mkConstMethod "documentEncoding" np $ objT c_QStringRef
  --, just $ mkConstMethod "documentVersion" np $ objT c_QStringRef
  --, just $ mkConstMethod "dtdName" np $ objT c_QStringRef
  --, just $ mkConstMethod "dtdPublicId" np $ objT c_QStringRef
  --, just $ mkConstMethod "dtdSystemId" np $ objT c_QStringRef
  , just $ mkConstMethod "entityDeclarations" np $ toGcT $ objT qXmlStreamEntityDeclarations
  , just $ mkProp "entityResolver" $ ptrT $ objT c_QXmlStreamEntityResolver
  , just $ mkConstMethod "error" np $ enumT e_Error
  , just $ mkConstMethod "errorString" np $ objT c_QString
  , just $ mkConstMethod "hasError" np boolT
  , just $ mkConstMethod "isCDATA" np boolT
  , just $ mkConstMethod "isCharacters" np boolT
  , just $ mkConstMethod "isComment" np boolT
  , just $ mkConstMethod "isDTD" np boolT
  , just $ mkConstMethod "isEndDocument" np boolT
  , just $ mkConstMethod "isEndElement" np boolT
  , just $ mkConstMethod "isEntityReference" np boolT
  , just $ mkConstMethod "isProcessingInstruction" np boolT
  , just $ mkConstMethod "isStandaloneDocument" np boolT
  , just $ mkConstMethod "isStartDocument" np boolT
  , just $ mkConstMethod "isStartElement" np boolT
  , just $ mkConstMethod "isWhitespace" np boolT
  , just $ mkConstMethod "lineNumber" np qint64
--  , just $ mkConstMethod "name" np $ objT c_QStringRef
  , just $ mkConstMethod "namespaceDeclarations" np $ toGcT $ objT qXmlStreamNamespaceDeclarations
  --, just $ mkConstMethod "namespaceUri" np $ objT c_QStringRef
  , just $ mkConstMethod "notationDeclarations" np $ toGcT $ objT qXmlStreamNotationDeclarations
  --, just $ mkConstMethod "prefix" np $ objT c_QStringRef
  --, just $ mkConstMethod "processingInstructionData" np $ objT c_QStringRef
  --, just $ mkConstMethod "processingInstructionTarget" np $ objT c_QStringRef
  --, just $ mkConstMethod "qualifiedName" np $ objT c_QStringRef
  , just $ mkMethod' "raiseError" "raiseError" np voidT
  , just $ mkMethod' "raiseError" "raiseErrorWithMessage" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "readElementText" "readElementText" np $ objT c_QString
  , just $ mkMethod' "readElementText" "readElementTextWithBehaviour" [enumT e_ReadElementTextBehaviour] $ objT c_QString
  , just $ mkMethod "readNext" np $ enumT e_TokenType
  , just $ mkMethod "readNextStartElement" np boolT
  , just $ mkMethod "skipCurrentElement" np voidT
  --, just $ mkConstMethod "text" np $ objT c_QStringRef
  , just $ mkConstMethod "tokenString" np $ objT c_QString
  , just $ mkConstMethod "tokenType" np $ enumT e_TokenType
  ]

e_Error =
  makeQtEnum (ident1 "QXmlStreamReader" "Error") [includeStd "QXmlStreamReader"]
  [ "NoError"
  , "UnexpectedElementError"
  , "CustomError"
  , "NotWellFormedError"
  , "PrematureEndOfDocumentError"
  ]

e_ReadElementTextBehaviour =
  makeQtEnum (ident1 "QXmlStreamReader" "ReadElementTextBehaviour") [includeStd "QXmlStreamReader"]
  [ "ErrorOnUnexpectedElement"
  , "IncludeChildElements"
  , "SkipChildElements"
  ]

e_TokenType =
  makeQtEnum (ident1 "QXmlStreamReader" "TokenType") [includeStd "QXmlStreamReader"]
  [ "NoToken"
  , "Invalid"
  , "StartDocument"
  , "EndDocument"
  , "StartElement"
  , "EndElement"
  , "Characters"
  , "Comment"
  , "DTD"
  , "EntityReference"
  , "ProcessingInstruction"
  ]
