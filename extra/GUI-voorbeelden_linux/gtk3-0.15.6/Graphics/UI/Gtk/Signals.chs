{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ------------
--  Callback installers for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Created: 1 July 2000
--
--  Copyright (C) 2000-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- #hide

-- These functions are used to connect signals to widgets. They are auto-
-- matically created through HookGenerator.hs which takes a list of possible
-- function signatures that are included in the GTK sources (gtkmarshal.list).
--
-- The object system in the second version of GTK is based on GObject from
-- GLIB. This base class is rather primitive in that it only implements
-- ref and unref methods (and others that are not interesting to us). If
-- the marshall list mentions OBJECT it refers to an instance of this
-- GObject which is automatically wrapped with a ref and unref call.
-- Structures which are not derived from GObject have to be passed as
-- BOXED which gives the signal connect function a possibility to do the
-- conversion into a proper ForeignPtr type. In special cases the signal
-- connect function use a PTR type which will then be mangled in the
-- user function directly. The latter is needed if a signal delivers a
-- pointer to a string and its length in a separate integer.
--
module Graphics.UI.Gtk.Signals (
  module System.Glib.Signals,

  connect_BOXED__BOOL,
  connect_PTR__BOOL,
  connect_BOXED_BOXED__BOOL,
  connect_ENUM__BOOL,
  connect_ENUM_DOUBLE__BOOL,
  connect_INT__BOOL,
  connect_OBJECT_BOXED__BOOL,
  connect_INT_INT__BOOL,
  connect_INT_INT_INT__BOOL,
  connect_WORD__BOOL,
  connect_NONE__BOOL,
  connect_BOOL__BOOL,
  connect_NONE__ENUM,
  connect_ENUM__ENUM,
  connect_PTR__INT,
  connect_BOOL__NONE,
  connect_INT__NONE,
  connect_INT_INT__NONE,
  connect_NONE__NONE,
  connect_DOUBLE__NONE,
  connect_BOXED__NONE,
  connect_BOXED_BOXED__NONE,
  connect_BOXED_BOXED_PTR__NONE,
  connect_BOXED_OBJECT__NONE,
  connect_BOXED_WORD__NONE,
  connect_ENUM__NONE,
  connect_ENUM_INT__NONE,
  connect_ENUM_INT_BOOL__NONE,
  connect_OBJECT__NONE,
  connect_MOBJECT__NONE,
  connect_OBJECT_BOXED_BOXED__NONE,
  connect_OBJECT_OBJECT__NONE,
  connect_PTR__NONE,
  connect_PTR_WORD__NONE,
  connect_GLIBSTRING__NONE,
  connect_GLIBSTRING_GLIBSTRING__NONE,
  connect_WORD_WORD__NONE,
  connect_WORD_GLIBSTRING__NONE,
  connect_BOXED_PTR_INT__NONE,
  connect_INT_BOOL__NONE,
  connect_OBJECT_GLIBSTRING__NONE,
  connect_GLIBSTRING__BOOL,
  connect_OBJECT_PTR_BOXED__BOOL,
  connect_PTR_BOXED_BOXED__BOOL,
  connect_PTR_INT_PTR__NONE,
  connect_PTR_WORD_WORD__NONE,
  connect_OBJECT_PTR_WORD_WORD__NONE,
  connect_OBJECT_INT_INT_PTR_WORD_WORD__NONE,
  connect_OBJECT_INT_INT_WORD__BOOL,
  connect_OBJECT_WORD__NONE,
  connect_OBJECT_ENUM__BOOL,
  connect_BOXED_GLIBSTRING__NONE,
  connect_OBJECT_INT__NONE,
  connect_ENUM_BOOL__BOOL,
  connect_BOXED_INT__NONE,
  connect_OBJECT_INT_INT_BOOL_OBJECT__BOOL,
  connect_INT_GLIBSTRING_INT__NONE,
  connect_GLIBSTRING_INT_ENUM_INT__NONE,
  connect_OBJECT__BOOL,
  connect_OBJECT_INT_OBJECT__NONE,
  connect_OBJECT_OBJECT_OBJECT__NONE,
  connect_OBJECT_OBJECT_OBJECT__BOOL,
  connect_ENUM_PTR__NONE,
  
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString   (peekUTFString,maybePeekUTFString,newUTFString)
import qualified System.Glib.UTFString as Glib
import System.Glib.GError      (failOnGError)
{#import System.Glib.Signals#}
{#import System.Glib.GObject#}
import Graphics.UI.Gtk.General.Threading


{#context lib="gtk" prefix="gtk" #}


-- Here are the generators that turn a Haskell function into
-- a C function pointer. The fist Argument is always the widget,
-- the last one is the user g_pointer. Both are ignored.


connect_BOXED__BOOL :: 
  GObjectClass obj => SignalName ->
  (Ptr a' -> IO a) -> 
  ConnectAfter -> obj ->
  (a -> IO Bool) ->
  IO (ConnectId obj)
connect_BOXED__BOOL signal boxedPre1 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> IO Bool
        action _ box1 =
          failOnGError $
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1'

connect_PTR__BOOL :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Ptr a -> IO Bool) ->
  IO (ConnectId obj)
connect_PTR__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> IO Bool
        action _ ptr1 =
          failOnGError $
          user (castPtr ptr1)

connect_BOXED_BOXED__BOOL :: 
  GObjectClass obj => SignalName ->
  (Ptr a' -> IO a) -> (Ptr b' -> IO b) -> 
  ConnectAfter -> obj ->
  (a -> b -> IO Bool) ->
  IO (ConnectId obj)
connect_BOXED_BOXED__BOOL signal boxedPre1 boxedPre2 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Ptr () -> IO Bool
        action _ box1 box2 =
          failOnGError $
          boxedPre2 (castPtr box2) >>= \box2' ->
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1' box2'

connect_ENUM__BOOL :: 
  (Enum a, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a -> IO Bool) ->
  IO (ConnectId obj)
connect_ENUM__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> IO Bool
        action _ enum1 =
          failOnGError $
          user (toEnum enum1)

connect_ENUM_DOUBLE__BOOL :: 
  (Enum a, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a -> Double -> IO Bool) ->
  IO (ConnectId obj)
connect_ENUM_DOUBLE__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> Double -> IO Bool
        action _ enum1 double2 =
          failOnGError $
          user (toEnum enum1) double2

connect_INT__BOOL :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Int -> IO Bool) ->
  IO (ConnectId obj)
connect_INT__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> IO Bool
        action _ int1 =
          failOnGError $
          user int1

connect_OBJECT_BOXED__BOOL :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  (Ptr b' -> IO b) -> 
  ConnectAfter -> obj ->
  (a' -> b -> IO Bool) ->
  IO (ConnectId obj)
connect_OBJECT_BOXED__BOOL signal boxedPre2 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Ptr () -> IO Bool
        action _ obj1 box2 =
          failOnGError $
          boxedPre2 (castPtr box2) >>= \box2' ->
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') box2'

connect_INT_INT__BOOL :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Int -> Int -> IO Bool) ->
  IO (ConnectId obj)
connect_INT_INT__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> Int -> IO Bool
        action _ int1 int2 =
          failOnGError $
          user int1 int2

connect_INT_INT_INT__BOOL :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Int -> Int -> Int -> IO Bool) ->
  IO (ConnectId obj)
connect_INT_INT_INT__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> Int -> Int -> IO Bool
        action _ int1 int2 int3 =
          failOnGError $
          user int1 int2 int3

connect_WORD__BOOL :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Word -> IO Bool) ->
  IO (ConnectId obj)
connect_WORD__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Word -> IO Bool
        action _ int1 =
          failOnGError $
          user int1

connect_NONE__BOOL :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (IO Bool) ->
  IO (ConnectId obj)
connect_NONE__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> IO Bool
        action _ =
          failOnGError $
          user

connect_BOOL__BOOL :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Bool -> IO Bool) ->
  IO (ConnectId obj)
connect_BOOL__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Bool -> IO Bool
        action _ bool1 =
          failOnGError $
          user bool1

connect_NONE__ENUM :: 
  (Enum a, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (IO a) ->
  IO (ConnectId obj)
connect_NONE__ENUM signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> IO Int
        action _ =
          failOnGError $
          liftM fromEnum $ 
          user

connect_ENUM__ENUM :: 
  (Enum a, Enum b, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a -> IO b) ->
  IO (ConnectId obj)
connect_ENUM__ENUM signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> IO Int
        action _ enum1 =
          failOnGError $
          liftM fromEnum $ 
          user (toEnum enum1)

connect_PTR__INT :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Ptr a -> IO Int) ->
  IO (ConnectId obj)
connect_PTR__INT signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> IO Int
        action _ ptr1 =
          failOnGError $
          user (castPtr ptr1)

connect_BOOL__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Bool -> IO ()) ->
  IO (ConnectId obj)
connect_BOOL__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Bool -> IO ()
        action _ bool1 =
          failOnGError $
          user bool1

connect_INT__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Int -> IO ()) ->
  IO (ConnectId obj)
connect_INT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> IO ()
        action _ int1 =
          failOnGError $
          user int1

connect_INT_INT__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Int -> Int -> IO ()) ->
  IO (ConnectId obj)
connect_INT_INT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> Int -> IO ()
        action _ int1 int2 =
          failOnGError $
          user int1 int2

connect_NONE__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (IO ()) ->
  IO (ConnectId obj)
connect_NONE__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> IO ()
        action _ =
          failOnGError $
          user

connect_DOUBLE__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Double -> IO ()) ->
  IO (ConnectId obj)
connect_DOUBLE__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Double -> IO ()
        action _ double1 =
          failOnGError $
          user double1

connect_BOXED__NONE :: 
  GObjectClass obj => SignalName ->
  (Ptr a' -> IO a) -> 
  ConnectAfter -> obj ->
  (a -> IO ()) ->
  IO (ConnectId obj)
connect_BOXED__NONE signal boxedPre1 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> IO ()
        action _ box1 =
          failOnGError $
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1'

connect_BOXED_BOXED__NONE :: 
  GObjectClass obj => SignalName ->
  (Ptr a' -> IO a) -> (Ptr b' -> IO b) -> 
  ConnectAfter -> obj ->
  (a -> b -> IO ()) ->
  IO (ConnectId obj)
connect_BOXED_BOXED__NONE signal boxedPre1 boxedPre2 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Ptr () -> IO ()
        action _ box1 box2 =
          failOnGError $
          boxedPre2 (castPtr box2) >>= \box2' ->
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1' box2'

connect_BOXED_BOXED_PTR__NONE :: 
  GObjectClass obj => SignalName ->
  (Ptr a' -> IO a) -> (Ptr b' -> IO b) -> 
  ConnectAfter -> obj ->
  (a -> b -> Ptr c -> IO ()) ->
  IO (ConnectId obj)
connect_BOXED_BOXED_PTR__NONE signal boxedPre1 boxedPre2 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Ptr () -> Ptr () -> IO ()
        action _ box1 box2 ptr3 =
          failOnGError $
          boxedPre2 (castPtr box2) >>= \box2' ->
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1' box2' (castPtr ptr3)

connect_BOXED_OBJECT__NONE :: 
  (GObjectClass b', GObjectClass obj) => SignalName ->
  (Ptr a' -> IO a) -> 
  ConnectAfter -> obj ->
  (a -> b' -> IO ()) ->
  IO (ConnectId obj)
connect_BOXED_OBJECT__NONE signal boxedPre1 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Ptr GObject -> IO ()
        action _ box1 obj2 =
          failOnGError $
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj2) >>= \obj2' ->
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1' (unsafeCastGObject obj2')

connect_BOXED_WORD__NONE :: 
  GObjectClass obj => SignalName ->
  (Ptr a' -> IO a) -> 
  ConnectAfter -> obj ->
  (a -> Word -> IO ()) ->
  IO (ConnectId obj)
connect_BOXED_WORD__NONE signal boxedPre1 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Word -> IO ()
        action _ box1 int2 =
          failOnGError $
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1' int2

connect_ENUM__NONE :: 
  (Enum a, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a -> IO ()) ->
  IO (ConnectId obj)
connect_ENUM__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> IO ()
        action _ enum1 =
          failOnGError $
          user (toEnum enum1)

connect_ENUM_INT__NONE :: 
  (Enum a, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a -> Int -> IO ()) ->
  IO (ConnectId obj)
connect_ENUM_INT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> Int -> IO ()
        action _ enum1 int2 =
          failOnGError $
          user (toEnum enum1) int2

connect_ENUM_INT_BOOL__NONE :: 
  (Enum a, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a -> Int -> Bool -> IO ()) ->
  IO (ConnectId obj)
connect_ENUM_INT_BOOL__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> Int -> Bool -> IO ()
        action _ enum1 int2 bool3 =
          failOnGError $
          user (toEnum enum1) int2 bool3

connect_OBJECT__NONE :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> IO ()
        action _ obj1 =
          failOnGError $
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1')

connect_MOBJECT__NONE :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (Maybe a' -> IO ()) ->
  IO (ConnectId obj)
connect_MOBJECT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> IO ()
        action _ obj1 =
          failOnGError $
          maybeNull (makeNewGObject (GObject, objectUnrefFromMainloop)) (return obj1) >>= \obj1' ->
          user (liftM unsafeCastGObject obj1')

connect_OBJECT_BOXED_BOXED__NONE :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  (Ptr b' -> IO b) -> (Ptr c' -> IO c) -> 
  ConnectAfter -> obj ->
  (a' -> b -> c -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT_BOXED_BOXED__NONE signal boxedPre2 boxedPre3 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Ptr () -> Ptr () -> IO ()
        action _ obj1 box2 box3 =
          failOnGError $
          boxedPre3 (castPtr box3) >>= \box3' ->
          boxedPre2 (castPtr box2) >>= \box2' ->
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') box2' box3'

connect_OBJECT_OBJECT__NONE :: 
  (GObjectClass a', GObjectClass b', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> b' -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT_OBJECT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Ptr GObject -> IO ()
        action _ obj1 obj2 =
          failOnGError $
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj2) >>= \obj2' ->
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') (unsafeCastGObject obj2')

connect_PTR__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Ptr a -> IO ()) ->
  IO (ConnectId obj)
connect_PTR__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> IO ()
        action _ ptr1 =
          failOnGError $
          user (castPtr ptr1)

connect_PTR_WORD__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Ptr a -> Word -> IO ()) ->
  IO (ConnectId obj)
connect_PTR_WORD__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Word -> IO ()
        action _ ptr1 int2 =
          failOnGError $
          user (castPtr ptr1) int2

connect_GLIBSTRING__NONE :: 
  (Glib.GlibString a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> IO ()) ->
  IO (ConnectId obj)
connect_GLIBSTRING__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> CString -> IO ()
        action _ str1 =
          failOnGError $
          peekUTFString str1 >>= \str1' ->
          user str1'

connect_GLIBSTRING_GLIBSTRING__NONE :: 
  (Glib.GlibString a', Glib.GlibString b', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> b' -> IO ()) ->
  IO (ConnectId obj)
connect_GLIBSTRING_GLIBSTRING__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> CString -> CString -> IO ()
        action _ str1 str2 =
          failOnGError $
          peekUTFString str2 >>= \str2' ->
          peekUTFString str1 >>= \str1' ->
          user str1' str2'

connect_WORD_WORD__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Word -> Word -> IO ()) ->
  IO (ConnectId obj)
connect_WORD_WORD__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Word -> Word -> IO ()
        action _ int1 int2 =
          failOnGError $
          user int1 int2

connect_WORD_GLIBSTRING__NONE :: 
  (Glib.GlibString b', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (Word -> b' -> IO ()) ->
  IO (ConnectId obj)
connect_WORD_GLIBSTRING__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Word -> CString -> IO ()
        action _ int1 str2 =
          failOnGError $
          peekUTFString str2 >>= \str2' ->
          user int1 str2'

connect_BOXED_PTR_INT__NONE :: 
  GObjectClass obj => SignalName ->
  (Ptr a' -> IO a) -> 
  ConnectAfter -> obj ->
  (a -> Ptr b -> Int -> IO ()) ->
  IO (ConnectId obj)
connect_BOXED_PTR_INT__NONE signal boxedPre1 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Ptr () -> Int -> IO ()
        action _ box1 ptr2 int3 =
          failOnGError $
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1' (castPtr ptr2) int3

connect_INT_BOOL__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Int -> Bool -> IO ()) ->
  IO (ConnectId obj)
connect_INT_BOOL__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> Bool -> IO ()
        action _ int1 bool2 =
          failOnGError $
          user int1 bool2

connect_OBJECT_GLIBSTRING__NONE :: 
  (GObjectClass a', Glib.GlibString b', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> b' -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT_GLIBSTRING__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> CString -> IO ()
        action _ obj1 str2 =
          failOnGError $
          peekUTFString str2 >>= \str2' ->
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') str2'

connect_GLIBSTRING__BOOL :: 
  (Glib.GlibString a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> IO Bool) ->
  IO (ConnectId obj)
connect_GLIBSTRING__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> CString -> IO Bool
        action _ str1 =
          failOnGError $
          peekUTFString str1 >>= \str1' ->
          user str1'

connect_OBJECT_PTR_BOXED__BOOL :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  (Ptr c' -> IO c) -> 
  ConnectAfter -> obj ->
  (a' -> Ptr b -> c -> IO Bool) ->
  IO (ConnectId obj)
connect_OBJECT_PTR_BOXED__BOOL signal boxedPre3 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Ptr () -> Ptr () -> IO Bool
        action _ obj1 ptr2 box3 =
          failOnGError $
          boxedPre3 (castPtr box3) >>= \box3' ->
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') (castPtr ptr2) box3'

connect_PTR_BOXED_BOXED__BOOL :: 
  GObjectClass obj => SignalName ->
  (Ptr b' -> IO b) -> (Ptr c' -> IO c) -> 
  ConnectAfter -> obj ->
  (Ptr a -> b -> c -> IO Bool) ->
  IO (ConnectId obj)
connect_PTR_BOXED_BOXED__BOOL signal boxedPre2 boxedPre3 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Ptr () -> Ptr () -> IO Bool
        action _ ptr1 box2 box3 =
          failOnGError $
          boxedPre3 (castPtr box3) >>= \box3' ->
          boxedPre2 (castPtr box2) >>= \box2' ->
          user (castPtr ptr1) box2' box3'

connect_PTR_INT_PTR__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Ptr a -> Int -> Ptr c -> IO ()) ->
  IO (ConnectId obj)
connect_PTR_INT_PTR__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Int -> Ptr () -> IO ()
        action _ ptr1 int2 ptr3 =
          failOnGError $
          user (castPtr ptr1) int2 (castPtr ptr3)

connect_PTR_WORD_WORD__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Ptr a -> Word -> Word -> IO ()) ->
  IO (ConnectId obj)
connect_PTR_WORD_WORD__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Word -> Word -> IO ()
        action _ ptr1 int2 int3 =
          failOnGError $
          user (castPtr ptr1) int2 int3

connect_OBJECT_PTR_WORD_WORD__NONE :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> Ptr b -> Word -> Word -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT_PTR_WORD_WORD__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Ptr () -> Word -> Word -> IO ()
        action _ obj1 ptr2 int3 int4 =
          failOnGError $
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') (castPtr ptr2) int3 int4

connect_OBJECT_INT_INT_PTR_WORD_WORD__NONE :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> Int -> Int -> Ptr d -> Word -> Word -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT_INT_INT_PTR_WORD_WORD__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Int -> Int -> Ptr () -> Word -> Word -> IO ()
        action _ obj1 int2 int3 ptr4 int5 int6 =
          failOnGError $
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') int2 int3 (castPtr ptr4) int5 int6

connect_OBJECT_INT_INT_WORD__BOOL :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> Int -> Int -> Word -> IO Bool) ->
  IO (ConnectId obj)
connect_OBJECT_INT_INT_WORD__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Int -> Int -> Word -> IO Bool
        action _ obj1 int2 int3 int4 =
          failOnGError $
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') int2 int3 int4

connect_OBJECT_WORD__NONE :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> Word -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT_WORD__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Word -> IO ()
        action _ obj1 int2 =
          failOnGError $
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') int2

connect_OBJECT_ENUM__BOOL :: 
  (GObjectClass a', Enum b, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> b -> IO Bool) ->
  IO (ConnectId obj)
connect_OBJECT_ENUM__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Int -> IO Bool
        action _ obj1 enum2 =
          failOnGError $
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') (toEnum enum2)

connect_BOXED_GLIBSTRING__NONE :: 
  (Glib.GlibString b', GObjectClass obj) => SignalName ->
  (Ptr a' -> IO a) -> 
  ConnectAfter -> obj ->
  (a -> b' -> IO ()) ->
  IO (ConnectId obj)
connect_BOXED_GLIBSTRING__NONE signal boxedPre1 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> CString -> IO ()
        action _ box1 str2 =
          failOnGError $
          peekUTFString str2 >>= \str2' ->
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1' str2'

connect_OBJECT_INT__NONE :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> Int -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT_INT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Int -> IO ()
        action _ obj1 int2 =
          failOnGError $
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') int2

connect_ENUM_BOOL__BOOL :: 
  (Enum a, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a -> Bool -> IO Bool) ->
  IO (ConnectId obj)
connect_ENUM_BOOL__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> Bool -> IO Bool
        action _ enum1 bool2 =
          failOnGError $
          user (toEnum enum1) bool2

connect_BOXED_INT__NONE :: 
  GObjectClass obj => SignalName ->
  (Ptr a' -> IO a) -> 
  ConnectAfter -> obj ->
  (a -> Int -> IO ()) ->
  IO (ConnectId obj)
connect_BOXED_INT__NONE signal boxedPre1 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Int -> IO ()
        action _ box1 int2 =
          failOnGError $
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1' int2

connect_OBJECT_INT_INT_BOOL_OBJECT__BOOL :: 
  (GObjectClass a', GObjectClass e', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> Int -> Int -> Bool -> e' -> IO Bool) ->
  IO (ConnectId obj)
connect_OBJECT_INT_INT_BOOL_OBJECT__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Int -> Int -> Bool -> Ptr GObject -> IO Bool
        action _ obj1 int2 int3 bool4 obj5 =
          failOnGError $
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj5) >>= \obj5' ->
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') int2 int3 bool4 (unsafeCastGObject obj5')

connect_INT_GLIBSTRING_INT__NONE :: 
  (Glib.GlibString b', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (Int -> b' -> Int -> IO ()) ->
  IO (ConnectId obj)
connect_INT_GLIBSTRING_INT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> CString -> Int -> IO ()
        action _ int1 str2 int3 =
          failOnGError $
          peekUTFString str2 >>= \str2' ->
          user int1 str2' int3

connect_GLIBSTRING_INT_ENUM_INT__NONE :: 
  (Glib.GlibString a', Enum c, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> Int -> c -> Int -> IO ()) ->
  IO (ConnectId obj)
connect_GLIBSTRING_INT_ENUM_INT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> CString -> Int -> Int -> Int -> IO ()
        action _ str1 int2 enum3 int4 =
          failOnGError $
          peekUTFString str1 >>= \str1' ->
          user str1' int2 (toEnum enum3) int4

connect_OBJECT__BOOL :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> IO Bool) ->
  IO (ConnectId obj)
connect_OBJECT__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> IO Bool
        action _ obj1 =
          failOnGError $
          makeNewGObject (GObject, objectUnref) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1')

connect_OBJECT_INT_OBJECT__NONE :: 
  (GObjectClass a', GObjectClass c', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> Int -> c' -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT_INT_OBJECT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Int -> Ptr GObject -> IO ()
        action _ obj1 int2 obj3 =
          failOnGError $
          makeNewGObject (GObject, objectUnref) (return obj3) >>= \obj3' ->
          makeNewGObject (GObject, objectUnref) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') int2 (unsafeCastGObject obj3')

connect_OBJECT_OBJECT_OBJECT__NONE :: 
  (GObjectClass a', GObjectClass b', GObjectClass c', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> b' -> c' -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT_OBJECT_OBJECT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Ptr GObject -> Ptr GObject -> IO ()
        action _ obj1 obj2 obj3 =
          failOnGError $
          makeNewGObject (GObject, objectUnref) (return obj3) >>= \obj3' ->
          makeNewGObject (GObject, objectUnref) (return obj2) >>= \obj2' ->
          makeNewGObject (GObject, objectUnref) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') (unsafeCastGObject obj2') (unsafeCastGObject obj3')

connect_OBJECT_OBJECT_OBJECT__BOOL :: 
  (GObjectClass a', GObjectClass b', GObjectClass c', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> b' -> c' -> IO Bool) ->
  IO (ConnectId obj)
connect_OBJECT_OBJECT_OBJECT__BOOL signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Ptr GObject -> Ptr GObject -> IO Bool
        action _ obj1 obj2 obj3 =
          failOnGError $
          makeNewGObject (GObject, objectUnref) (return obj3) >>= \obj3' ->
          makeNewGObject (GObject, objectUnref) (return obj2) >>= \obj2' ->
          makeNewGObject (GObject, objectUnref) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') (unsafeCastGObject obj2') (unsafeCastGObject obj3')

connect_ENUM_PTR__NONE :: 
  (Enum a, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a -> Ptr b -> IO ()) ->
  IO (ConnectId obj)
connect_ENUM_PTR__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> Ptr () -> IO ()
        action _ enum1 ptr2 =
          failOnGError $
          user (toEnum enum1) (castPtr ptr2)

