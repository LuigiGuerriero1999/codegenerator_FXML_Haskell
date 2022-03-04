/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * glade-clipboard.c - An object for handling Cut/Copy/Paste.
 *
 * Copyright (C) 2005 The GNOME Foundation.
 *
 * Author(s):
 *      Tristan Van Berkom <tvb@gnome.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
 * USA.
 */

#include "config.h"

#include "glade-accumulators.h"

#include <glib-object.h>


gboolean
glade_single_object_accumulator (GSignalInvocationHint *ihint,
				 GValue                *return_accu,
				 const GValue          *handler_return,
				 gpointer               dummy)
{
	GObject  *object = g_value_get_object (handler_return);
	g_value_set_object (return_accu, object);
	
	return (object == NULL);
}

gboolean
glade_integer_handled_accumulator (GSignalInvocationHint *ihint,
				   GValue                *return_accu,
				   const GValue          *handler_return,
				   gpointer               dummy)
{
	gboolean continue_emission;
	gint retval;
	
	retval = g_value_get_int (handler_return);
	g_value_set_int (return_accu, retval >> 1);
	continue_emission = !(retval & 1);
	
	return continue_emission;
}

/* From gtkmain.c */
gboolean
glade_boolean_handled_accumulator (GSignalInvocationHint *ihint,
				   GValue                *return_accu,
				   const GValue          *handler_return,
				   gpointer               dummy)
{
	gboolean continue_emission;
	gboolean signal_handled;
	
	signal_handled = g_value_get_boolean (handler_return);
	g_value_set_boolean (return_accu, signal_handled);
	continue_emission = !signal_handled;
	
	return continue_emission;
}

gboolean
glade_string_accumulator (GSignalInvocationHint *ihint,
			  GValue                *return_accu,
			  const GValue          *handler_return,
			  gpointer               dummy)
{
	const gchar *handler_str;
	
	g_free ((void *)g_value_get_string (return_accu));
	
	handler_str = g_value_get_string (handler_return);
	g_value_set_string (return_accu, handler_str);
	
	return (handler_str == NULL);
}

gboolean
glade_stop_emission_accumulator (GSignalInvocationHint *ihint,
				 GValue                *return_accu,
				 const GValue          *handler_return,
				 gpointer               dummy)
{
	g_value_copy (handler_return, return_accu);
	
	return FALSE;
}
