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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QOpenGLWidget (
  aModule,
  c_QOpenGLWidget,
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
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (fl_WindowFlags, gluint)
import Graphics.UI.Qtah.Generator.Interface.Gui.QImage (c_QImage)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listener)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [5, 4]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Widgets", "QOpenGLWidget"] minVersion $
  collect
  [ just $ QtExportClassAndSignals c_QOpenGLWidget signals
  , test (qtVersion >= [5, 5]) $ qtExport e_UpdateBehavior
  ]

(c_QOpenGLWidget, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QOpenGLWidget"] $
  classSetEntityPrefix "" $
  makeClass (ident "QOpenGLWidget") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QOpenGLWidget]
  , just $ mkCtor "newWithParentAndFlags" [ptrT $ objT c_QOpenGLWidget, flagsT fl_WindowFlags]
    -- TODO mkConstMethod "context" np $ ptrT $ objT c_QOpenGLContext
  , just $ mkConstMethod "defaultFramebufferObject" np gluint
  , just $ mkMethod "doneCurrent" np voidT
    -- TODO mkProp "format" $ objT c_QSurfaceFormat
  , just $ mkMethod "grabFramebuffer" np $ objT c_QImage
  , just $ mkConstMethod "isValid" np boolT
  , just $ mkMethod "makeCurrent" np voidT
  , test (qtVersion >= [5, 5]) $ mkProp "updateBehavior" $ enumT e_UpdateBehavior
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "aboutToCompose" listener
  , makeSignal "aboutToResize" listener
  , makeSignal "frameSwapped" listener
  , makeSignal "resized" listener
  ]

e_UpdateBehavior =
  makeQtEnum (ident1 "QOpenGLWidget" "UpdateBehavior") [includeStd "QOpenGLWidget"]
  [ "NoPartialUpdate"
  , "PartialUpdate"
  ]
