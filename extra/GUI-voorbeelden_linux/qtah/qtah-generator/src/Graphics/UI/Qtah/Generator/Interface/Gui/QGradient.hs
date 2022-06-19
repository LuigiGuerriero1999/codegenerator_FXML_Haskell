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

module Graphics.UI.Qtah.Generator.Interface.Gui.QGradient (
  aModule,
  c_QGradient,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (
  enumT,
  constT,
  objT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
import Graphics.UI.Qtah.Generator.Interface.Gui.QColor (c_QColor)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QGradient"] $
  collect
  [ just $ qtExport c_QGradient
  , test (qtVersion >= [4, 4]) $ qtExport e_CoordinateMode
  , test (qtVersion >= [5, 12]) $ qtExport e_Preset
  , just $ qtExport e_Spread
  , just $ qtExport e_Type
  ]

c_QGradient =
  addReqIncludes [includeStd "QGradient"] $
  classSetConversionToGc $
  classAddFeatures [Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QGradient") Nothing [] $
  collect
  [ test (qtVersion >= [5, 12]) $ mkCtor "new" [enumT e_Preset]
  , test (qtVersion >= [4, 4]) $
    mkProp "coordinateMode" $ enumT e_CoordinateMode
  , just $ mkMethod "setColorAt" [qreal, constT $ objT c_QColor] voidT
  , just $ mkProp "spread" $ enumT e_Spread
  -- TODO void setStops(const QGradientStops &stopPoints)
  -- TODO QGradientStops stops() const
  , just $ mkConstMethod' "type" "getType" np $ enumT e_Type
  ]

e_CoordinateMode =
  makeQtEnum (ident1 "QGradient" "CoordinateMode") [includeStd "QGradient"] $
  [ "LogicalMode"
  , "StretchToDeviceMode"
  , "ObjectBoundingMode"
  ] ++
  ["ObjectMode" | qtVersion >= [5, 12]]

e_Preset =
  makeQtEnum (ident1 "QGradient" "Preset") [includeStd "QGradient"]
  [ "WarmFlame"
    {- TODO Fix off-by-one.  Also it looks like some of these are missing from
     the Qt headers (Arielle's Smile, Above Clouds)?
  , (1, ["night", "fade"])
  , (2, ["spring", "warmth"])
  , (3, ["juicy", "peach"])
  , (4, ["young", "passion"])
  , (5, ["lady", "lips"])
  , (6, ["sunny", "morning"])
  , (7, ["rainy", "ashville"])
  , (8, ["frozen", "dreams"])
  , (9, ["winter", "neva"])
  , (10, ["dusty", "grass"])
  , (11, ["tempting", "azure"])
  , (12, ["heavy", "rain"])
  , (13, ["amy", "crisp"])
  , (14, ["mean", "fruit"])
  , (15, ["deep", "blue"])
  , (16, ["ripe", "malinka"])
  , (17, ["cloudy", "knoxville"])
  , (18, ["malibu", "beach"])
  , (19, ["new", "life"])
  , (20, ["true", "sunset"])
  , (21, ["morpheus", "den"])
  , (22, ["rare", "wind"])
  , (23, ["near", "moon"])
  , (24, ["wild", "apple"])
  , (25, ["saint", "petersburg"])
  , (26, ["arielle's", "smile"])
  , (27, ["plum", "plate"])
  , (28, ["everlasting", "sky"])
  , (29, ["happy", "fisher"])
  , (30, ["blessing"])
  , (31, ["sharpeye", "eagle"])
  , (32, ["ladoga", "bottom"])
  , (33, ["lemon", "gate"])
  , (34, ["itmeo", "branding"])
  , (35, ["zeus", "miracle"])
  , (36, ["old", "hat"])
  , (37, ["star", "wine"])
  , (38, ["deep", "bluee"])
  , (39, ["coup", "de", "grace"])
  , (40, ["happy", "acid"])
  , (41, ["awesome", "pine"])
  , (42, ["new", "york"])
  , (43, ["shy", "rainbow"])
  , (44, ["loon", "crest"])
  , (45, ["mixed", "hopes"])
  , (46, ["fly", "high"])
  , (47, ["strong", "bliss"])
  , (48, ["fresh", "milk"])
  , (49, ["snow", "again"])
  , (50, ["february", "ink"])
  , (51, ["kind", "steel"])
  , (52, ["soft", "grass"])
  , (53, ["grown", "early"])
  , (54, ["sharp", "blues"])
  , (55, ["shady", "water"])
  , (56, ["dirty", "beauty"])
  , (57, ["great", "whale"])
  , (58, ["teen", "notebook"])
  , (59, ["polite", "rumors"])
  , (60, ["sweet", "period"])
  , (61, ["wide", "matrix"])
  , (62, ["soft", "cherish"])
  , (63, ["red", "salvation"])
  , (64, ["burning", "spring"])
  , (65, ["night", "party"])
  , (66, ["sky", "glider"])
  , (67, ["heaven", "peach"])
  , (68, ["purple", "division"])
  , (69, ["aqua", "splash"])
  , (70, ["above", "clouds"])
  , (71, ["spiky", "naga"])
  , (72, ["love", "kiss"])
  , (73, ["sharp", "glass"])
  , (74, ["clean", "mirror"])
  , (75, ["premium", "dark"])
  , (76, ["cold", "evening"])
  , (77, ["cochiti", "lake"])
  , (78, ["summer", "games"])
  , (79, ["passionate", "bed"])
  , (80, ["mountain", "rock"])
  , (81, ["desert", "hump"])
  , (82, ["jungle", "day"])
  , (83, ["phoenix", "start"])
  , (84, ["october", "silence"])
  , (85, ["faraway", "river"])
  , (86, ["alchemist", "lab"])
  , (87, ["over", "sun"])
  , (88, ["premium", "white"])
  , (89, ["mars", "party"])
  , (90, ["eternal", "constance"])
  , (91, ["japan", "blush"])
  , (92, ["smiling", "rain"])
  , (93, ["cloudy", "apple"])
  , (94, ["big", "mango"])
  , (95, ["healthy", "water"])
  , (96, ["amour", "amour"])
  , (97, ["risky", "concrete"])
  , (98, ["strong", "stick"])
  , (99, ["vicious", "stance"])
  , (100, ["palo", "alto"])
  , (101, ["happy", "memories"])
  , (102, ["midnight", "bloom"])
  , (103, ["crystalline"])
  , (104, ["raccoon", "back"])
  , (105, ["party", "bliss"])
  , (106, ["confident", "cloud"])
  , (107, ["le", "cocktail"])
  , (108, ["river", "city"])
  , (109, ["frozen", "berry"])
  , (110, ["elegance"])
  , (111, ["child", "care"])
  , (112, ["flying", "lemon"])
  , (113, ["new", "retrowave"])
  , (114, ["hidden", "jaguar"])
  , (115, ["above", "the", "sky"])
  , (116, ["nega"])
  , (117, ["dense", "water"])
  , (118, ["chemic", "aqua"])
  , (119, ["seashore"])
  , (120, ["marble", "wall"])
  , (121, ["cheerful", "caramel"])
  , (122, ["night", "sky"])
  , (123, ["magic", "lake"])
  , (124, ["young", "grass"])
  , (125, ["colorful", "peach"])
  , (126, ["gentle", "care"])
  , (127, ["plum", "bath"])
  , (128, ["happy", "unicorn"])
  , (129, ["full", "metall"])
  , (130, ["african", "field"])
  , (131, ["solid", "stone"])
  , (132, ["orange", "juice"])
  , (133, ["glass", "water"])
  , (134, ["slick", "carbon"])
  , (135, ["north", "miracle"])
  , (136, ["fruit", "blend"])
  , (137, ["millennium", "pine"])
  , (138, ["high", "flight"])
  , (139, ["mole", "hall"])
  , (140, ["earl", "gray"])
  , (141, ["space", "shift"])
  , (142, ["forest", "inei"])
  , (143, ["royal", "garden"])
  , (144, ["rich", "metal"])
  , (145, ["juicy", "cake"])
  , (146, ["smart", "indigo"])
  , (147, ["sand", "strike"])
  , (148, ["norse", "beauty"])
  , (149, ["aqua", "guidance"])
  , (150, ["sun", "veggie"])
  , (151, ["sea", "lord"])
  , (152, ["black", "sea"])
  , (153, ["grass", "shampoo"])
  , (154, ["landing", "aircraft"])
  , (155, ["witch", "dance"])
  , (156, ["sleepless", "night"])
  , (157, ["angel", "care"])
  , (158, ["crystal", "river"])
  , (159, ["soft", "lipstick"])
  , (160, ["salt", "mountain"])
  , (161, ["perfect", "white"])
  , (162, ["fresh", "oasis"])
  , (163, ["strict", "november"])
  , (164, ["morning", "salad"])
  , (165, ["deep", "relief"])
  , (166, ["sea", "strike"])
  , (167, ["night", "call"])
  , (168, ["supreme", "sky"])
  , (169, ["light", "blue"])
  , (170, ["mind", "crawl"])
  , (171, ["lily", "meadow"])
  , (172, ["sugar", "lollipop"])
  , (173, ["sweet", "dessert"])
  , (174, ["magic", "ray"])
  , (175, ["teen", "party"])
  , (176, ["frozen", "heat"])
  , (177, ["gagarin", "view"])
  , (178, ["fabled", "sunset"])
  , (179, ["perfect", "blue"])
  -}
  ]

e_Spread =
  makeQtEnum (ident1 "QGradient" "Spread") [includeStd "QGradient"]
  [ "PadSpread"
  , "ReflectSpread"
  , "RepeatSpread"
  ]

e_Type =
  makeQtEnum (ident1 "QGradient" "Type") [includeStd "QGradient"]
  [ "LinearGradient"
  , "RadialGradient"
  , "ConicalGradient"
  , "NoGradient"
  ]
