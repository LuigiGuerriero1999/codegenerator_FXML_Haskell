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

module Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamWriter (
  aModule,
  c_QXmlStreamWriter,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod',
  mkMethod,
  mkProp,
  np,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QTextCodec (c_QTextCodec)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamReader (c_QXmlStreamReader)
import Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamAttributes (c_QXmlStreamAttributes)
import Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamAttribute (c_QXmlStreamAttribute)
import Foreign.Hoppy.Generator.Types (intT, boolT, charT, voidT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QXmlStreamWriter"]
  [qtExport c_QXmlStreamWriter]

c_QXmlStreamWriter =
  addReqIncludes [ includeStd "QXmlStreamWriter" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QXmlStreamWriter") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithString" [ptrT $ objT c_QString]
  , just $ mkCtor "newWithByteArray" [ptrT $ objT c_QByteArray]
  , just $ mkCtor "newWithIODevice" [ptrT $ objT c_QIODevice]
  , just $ mkProp "autoFormatting" boolT
  , just $ mkProp "autoFormattingIndent" intT
  , just $ mkConstMethod "codec" np $ ptrT $ objT c_QTextCodec
  , just $ mkProp "device" $ ptrT $ objT c_QIODevice
  , just $ mkConstMethod "hasError" np boolT
  , just $ mkMethod' "setCodec" "setCodec" [ptrT $ objT c_QTextCodec] voidT
  , just $ mkMethod' "setCodec" "setCodecWithPtrChar" [ptrT $ constT charT] voidT
  , just $ mkMethod' "writeAttribute" "writeAttributeWithNamespaceUriAndNameAndValue" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeAttribute" "writeAttributeWithQualifiedNameAndValue" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeAttribute" "writeAttribute" [refT $ constT $ objT c_QXmlStreamAttribute] voidT
  , just $ mkMethod "writeAttributes" [refT $ constT $ objT c_QXmlStreamAttributes] voidT
  , just $ mkMethod "writeCDATA" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "writeCharacters" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "writeComment" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "writeCurrentToken" [refT $ constT $ objT c_QXmlStreamReader] voidT
  , just $ mkMethod "writeDTD" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "writeDefaultNamespace" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeEmptyElement" "writeEmptyElementWithNamespaceUriAndName" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeEmptyElement" "writeEmptyElement" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "writeEndDocument" np voidT
  , just $ mkMethod "writeEndElement" np voidT
  , just $ mkMethod "writeEntityReference" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeNamespace" "writeNamespace" [refT $ constT $ objT c_QString ] voidT
  , just $ mkMethod' "writeNamespace" "writeNamespaceWithNamespaceUriAndPrefix" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeProcessingInstruction" "writeProcessingInstruction" [refT $ constT $ objT c_QString ] voidT
  , just $ mkMethod' "writeProcessingInstruction" "writeProcessingInstructionWithTargetAndData" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeStartDocument" "writeStartDocumentWithVersion" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeStartDocument" "writeStartDocument" np voidT
  , just $ mkMethod' "writeStartDocument" "writeStartDocumentWithVersionAndStandalone" [refT $ constT $ objT c_QString, boolT] voidT
  , just $ mkMethod' "writeStartElement" "writeStartElementWithNamespaceUriAndName" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeStartElement" "writeStartElement" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeTextElement" "writeTextElementWithNamespaceUriAndNameAndText" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeTextElement" "writeTextElementWithQualifiedNameAndText" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  ]
