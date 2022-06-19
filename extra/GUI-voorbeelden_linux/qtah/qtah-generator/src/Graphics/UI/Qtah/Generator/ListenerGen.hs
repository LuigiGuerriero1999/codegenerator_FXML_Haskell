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

module Graphics.UI.Qtah.Generator.ListenerGen (
  generateListenerCpp,
  ) where

import Data.List (intercalate)
import Graphics.UI.Qtah.Generator.Common (writeFileIfDifferent)
import Graphics.UI.Qtah.Generator.Config (Version)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (ListenerDef (..), listeners)
import System.FilePath ((</>))

generateListenerCpp :: FilePath -> IO ()
generateListenerCpp cppDirPath = do
  writeFileIfDifferent (cppDirPath </> "listener.hpp") hppSource
  writeFileIfDifferent (cppDirPath </> "listener.cpp") cppSource

-- TODO Generate this from the listener definitions themselves.
cppIncludes :: [String]
cppIncludes =
  [ "#include <QAbstractAnimation>"
  , "#include <QAbstractButton>"
  , "#include <QAbstractSlider>"
  , "#include <QAction>"
  , "#include <QClipboard>"
  , "#include <QDockWidget>"
  , "#include <QIcon>"
  , "#include <QItemSelection>"
  , "#include <QMdiSubWindow>"
  , "#include <QMetaObject>"
  , "#include <QModelIndex>"
  , "#include <QObject>"
  , "#include <QPoint>"
  , "#include <QProcess>"
  , "#include <QSize>"
  , "#include <QSystemTrayIcon>"
  , "#include <QTreeWidgetItem>"
  , "#include <QVariant>"
  , "#include <QVector>"
  , "#include <QWidget>"
  , "#include <Qt>"
  , "#include <QtGlobal>"
  , "#include <string>"
  , "#if QT_VERSION >= 0x050000"
  , "#include <QWindow>"
  , "#endif"
  , "#include \"b_callback.hpp\""
  ]

hppSource :: String
hppSource = (++ "\n") $ intercalate "\n" $
  [ "////////// GENERATED FILE, EDITS WILL BE LOST //////////"
  , ""
  , "#ifndef QTAH_LISTENERS_HPP"
  , "#define QTAH_LISTENERS_HPP"
  , ""
  ] ++ cppIncludes ++
  flip concatMap listeners (\l ->
    let cn = listenerClassName l
        ccn = listenerCallbackClassName l
        pl = listenerCppParamList l
    in [ "" ] ++
       maybe [] (\v -> ["#if QT_VERSION >= " ++ renderVersionCppHex v, ""]) (listenerMinVersion l) ++
       [ "class " ++ cn ++ " : public QObject {"
       , "    Q_OBJECT"
       , ""
       , "public:"
       , "    typedef " ++ ccn ++ " callback;"
       , ""
       , "    " ++ cn ++ "(QObject* source, const std::string& signal, callback f);"
       , "    ~" ++ cn ++ "();"
       , "    bool isValid() const;"
       , ""
       , "public slots:"
       , "    void invoke(" ++ pl ++ ");"
       , ""
       , "private:"
       , "    callback f_;"
       , "    QMetaObject::Connection connection_;"
       , "};"
       ] ++
       maybe [] (\_ -> ["", "#endif"]) (listenerMinVersion l)
  ) ++
  [ ""
  , "#endif"
  ]

cppSource :: String
cppSource = (++ "\n") $ intercalate "\n" $
  [ "////////// GENERATED FILE, EDITS WILL BE LOST //////////"
  , ""
  , "#include \"listener.hpp\""
  , ""
  , "#include <iostream>"
  ] ++
  flip concatMap listeners (\l ->
    let cn = listenerClassName l
        pl = listenerCppParamList l
        ptl = listenerCppParamTypeList l
        pnl = listenerCppParamNameList l
    in [ "" ] ++
       maybe [] (\v -> ["#if QT_VERSION >= " ++ renderVersionCppHex v, ""]) (listenerMinVersion l) ++
       [ cn ++ "::" ++ cn ++ "(QObject* source, const std::string& signal, " ++ cn ++ "::callback f) :"
       , "    QObject(source), f_(f) {"
       , "    connection_ = connect(source, signal.c_str(), this, SLOT(invoke(" ++ ptl ++ ")));"
       , "}"
       , ""
       , cn ++ "::~" ++ cn ++ "() {"
       , "    QObject::disconnect(connection_);"
       , "}"
       , ""
       , "bool " ++ cn ++ "::isValid() const {"
       , "    return static_cast<bool>(connection_);"
       , "}"
       , ""
       , "void " ++ cn ++ "::invoke(" ++ pl ++ ") {"
       , "    f_(" ++ pnl ++ ");"
       , "}"
       ] ++
       maybe [] (\_ -> ["", "#endif"]) (listenerMinVersion l)
  )

renderVersionCppHex :: Version -> String
renderVersionCppHex version = "0x" ++ a' ++ b' ++ c'
  where [a, b, c] = take 3 $ version ++ repeat 0
        a' = pad a
        b' = pad b
        c' = pad c
        pad n =
          if 0 <= n && n < 10
          then '0' : show n
          else if 10 <= n && n < 100
               then show n
               else error $ "renderVersionCppHex expects 0 <= n < 100, n is " ++ show n ++ "."
