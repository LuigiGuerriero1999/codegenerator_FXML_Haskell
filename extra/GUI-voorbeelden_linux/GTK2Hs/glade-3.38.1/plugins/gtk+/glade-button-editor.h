/*
 * Copyright (C) 2008 Tristan Van Berkom.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Authors:
 *   Tristan Van Berkom <tvb@gnome.org>
 */
#ifndef _GLADE_BUTTON_EDITOR_H_
#define _GLADE_BUTTON_EDITOR_H_

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GLADE_TYPE_BUTTON_EDITOR            (glade_button_editor_get_type ())
#define GLADE_BUTTON_EDITOR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GLADE_TYPE_BUTTON_EDITOR, GladeButtonEditor))
#define GLADE_BUTTON_EDITOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GLADE_TYPE_BUTTON_EDITOR, GladeButtonEditorClass))
#define GLADE_IS_BUTTON_EDITOR(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GLADE_TYPE_BUTTON_EDITOR))
#define GLADE_IS_BUTTON_EDITOR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GLADE_TYPE_BUTTON_EDITOR))
#define GLADE_BUTTON_EDITOR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GLADE_TYPE_BUTTON_EDITOR, GladeButtonEditorClass))

typedef struct _GladeButtonEditor        GladeButtonEditor;
typedef struct _GladeButtonEditorClass   GladeButtonEditorClass;
typedef struct _GladeButtonEditorPrivate GladeButtonEditorPrivate;

struct _GladeButtonEditor
{
  GladeEditorSkeleton  parent;

  GladeButtonEditorPrivate *priv;
};

struct _GladeButtonEditorClass
{
  GladeEditorSkeletonClass parent;
};

GType            glade_button_editor_get_type (void) G_GNUC_CONST;
GtkWidget       *glade_button_editor_new      (void);

G_END_DECLS

#endif  /* _GLADE_BUTTON_EDITOR_H_ */
