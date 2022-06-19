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

module Graphics.UI.Qtah.Generator.Interface.Core (modules) where

import qualified Graphics.UI.Qtah.Generator.Interface.Core.QAbstractAnimation as QAbstractAnimation
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel as QAbstractItemModel
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QAbstractListModel as QAbstractListModel
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QAbstractTableModel as QAbstractTableModel
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QAnimationGroup as QAnimationGroup
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QByteArray as QByteArray
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QBuffer as QBuffer
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QChar as QChar
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QChildEvent as QChildEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QDate as QDate
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QDateTime as QDateTime
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QDebug as QDebug
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QDebugStateSaver as QDebugStateSaver
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QDir as QDir
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QDirIterator as QDirIterator
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QFile as QFile
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QFileDevice as QFileDevice
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QFileInfo as QFileInfo
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QFileSelector as QFileSelector
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QFileSystemWatcher as QFileSystemWatcher
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QCryptographicHash as QCryptographicHash
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QIODevice as QIODevice
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QItemSelection as QItemSelection
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionModel as QItemSelectionModel
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionRange as QItemSelectionRange
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QLatin1Char as QLatin1Char
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QLatin1String as QLatin1String
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QLibrary as QLibrary
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QLibraryInfo as QLibraryInfo
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QList as QList
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QLockFile as QLockFile
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QLoggingCategory as QLoggingCategory
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMargins as QMargins
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMarginsF as QMarginsF
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMessageAuthenticationCode as QMessageAuthenticationCode
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMessageLogContext as QMessageLogContext
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMessageLogger as QMessageLogger
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMetaClassInfo as QMetaClassInfo
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMetaEnum as QMetaEnum
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMetaMethod as QMetaMethod
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMetaObject as QMetaObject
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMetaObject.Connection as QMetaObjectConnection
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMetaProperty as QMetaProperty
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QMimeData as QMimeData
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex as QModelIndex
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QOperatingSystemVersion as QOperatingSystemVersion
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QPair as QPair
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QParallelAnimationGroup as QParallelAnimationGroup
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QPauseAnimation as QPauseAnimation
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QPalette as QPalette
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QPersistentModelIndex as QPersistentModelIndex
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QPluginLoader as QPluginLoader
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QProcess as QProcess
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QProcessEnvironment as QProcessEnvironment
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QPropertyAnimation as QPropertyAnimation
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QPoint as QPoint
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QPointF as QPointF
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QRandomGenerator as QRandomGenerator
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QRandomGenerator64 as QRandomGenerator64
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QRect as QRect
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QRectF as QRectF
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QResource as QResource
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QSaveFile as QSaveFile
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QSequentialAnimationGroup as QSequentialAnimationGroup
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QSettings as QSettings
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QSize as QSize
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QSizeF as QSizeF
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QStandardPaths as QStandardPaths
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QStaticPlugin as QStaticPlugin
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QString as QString
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QStringList as QStringList
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QStringListModel as QStringListModel
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QSysInfo as QSysInfo
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QTextCodec as QTextCodec
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QTextDecoder as QTextDecoder
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QTextEncoder as QTextEncoder
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QThread as QThread
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QTime as QTime
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QTimer as QTimer
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QTimerEvent as QTimerEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QTimeZone as QTimeZone
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QTranslator as QTranslator
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QVariant as QVariant
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QVariantAnimation as QVariantAnimation
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QVector as QVector
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QVersionNumber as QVersionNumber
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamAttribute as QXmlStreamAttribute
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamAttributes as QXmlStreamAttributes
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamEntityDeclaration as QXmlStreamEntityDeclaration
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamEntityResolver as QXmlStreamEntityResolver
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamNamespaceDeclaration as QXmlStreamNamespaceDeclaration
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamNotationDeclaration as QXmlStreamNotationDeclaration
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamReader as QXmlStreamReader
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamWriter as QXmlStreamWriter
import qualified Graphics.UI.Qtah.Generator.Interface.Core.Types as Types
import Graphics.UI.Qtah.Generator.Module (AModule)

{-# ANN module "HLint: ignore Use camelCase" #-}

modules :: [AModule]
modules =
  concat
  [ [ QAbstractAnimation.aModule
    , QAbstractItemModel.aModule
    , QAbstractListModel.aModule
    , QAbstractTableModel.aModule
    , QAnimationGroup.aModule
    , QByteArray.aModule
    , QBuffer.aModule
    , QChar.aModule
    , QChildEvent.aModule
    , QCoreApplication.aModule
    , QDate.aModule
    , QDateTime.aModule
    , QDebug.aModule
    , QDebugStateSaver.aModule
    , QDir.aModule
    , QDirIterator.aModule
    , QEvent.aModule
    , QFile.aModule
    , QFileDevice.aModule
    , QFileInfo.aModule
    , QFileSelector.aModule
    , QFileSystemWatcher.aModule
    , QCryptographicHash.aModule
    , QIODevice.aModule
    , QItemSelection.aModule
    , QItemSelectionModel.aModule
    , QItemSelectionRange.aModule
    , QLatin1Char.aModule
    , QLatin1String.aModule
    , QLibrary.aModule
    , QLibraryInfo.aModule
    , QLockFile.aModule
    , QLoggingCategory.aModule
    , QMargins.aModule
    , QMarginsF.aModule
    , QMessageAuthenticationCode.aModule
    , QMessageLogContext.aModule
    , QMessageLogger.aModule
    , QMetaClassInfo.aModule
    , QMetaEnum.aModule
    , QMetaMethod.aModule
    , QMetaObject.aModule
    , QMetaObjectConnection.aModule
    , QMetaProperty.aModule
    , QMimeData.aModule
    , QModelIndex.aModule
    , QObject.aModule
    , QOperatingSystemVersion.aModule
    , QPalette.aModule
    , QParallelAnimationGroup.aModule
    , QPauseAnimation.aModule
    , QPersistentModelIndex.aModule
    , QProcess.aModule
    , QProcessEnvironment.aModule
    , QPropertyAnimation.aModule
    , QPluginLoader.aModule
    , QPoint.aModule
    , QPointF.aModule
    , QRandomGenerator.aModule
    , QRandomGenerator64.aModule
    , QRect.aModule
    , QRectF.aModule
    , QResource.aModule
    , QSaveFile.aModule
    , QSequentialAnimationGroup.aModule
    , QSettings.aModule
    , QSize.aModule
    , QSizeF.aModule
    , QStandardPaths.aModule
    , QStaticPlugin.aModule
    , QString.aModule
    , QStringList.aModule
    , QStringListModel.aModule
    , QSysInfo.aModule
    , QTextCodec.aModule
    , QTextDecoder.aModule
    , QTextEncoder.aModule
    , QThread.aModule
    , QTime.aModule
    , QTimer.aModule
    , QTimerEvent.aModule
    , QTimeZone.aModule
    , QTranslator.aModule
    , QVariant.aModule
    , QVariantAnimation.aModule
    , QVersionNumber.aModule
    , QXmlStreamAttribute.aModule
    , QXmlStreamAttributes.aModule
    , QXmlStreamEntityDeclaration.aModule
    , QXmlStreamEntityResolver.aModule
    , QXmlStreamNamespaceDeclaration.aModule
    , QXmlStreamNotationDeclaration.aModule
    , QXmlStreamReader.aModule
    , QXmlStreamWriter.aModule
    , Types.aModule
    ]
  , QList.allModules
  , QVector.allModules
  , QPair.allModules
  ]
