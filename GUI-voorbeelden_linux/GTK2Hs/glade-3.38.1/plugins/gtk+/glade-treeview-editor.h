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
#ifndef _GLADE_TREE_VIEW_EDITOR_H_
#define _GLADE_TREE_VIEW_EDITOR_H_

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GLADE_TYPE_TREE_VIEW_EDITOR            (glade_tree_view_editor_get_type ())
#define GLADE_TREE_VIEW_EDITOR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GLADE_TYPE_TREE_VIEW_EDITOR, GladeTreeViewEditor))
#define GLADE_TREE_VIEW_EDITOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GLADE_TYPE_TREE_VIEW_EDITOR, GladeTreeViewEditorClass))
#define GLADE_IS_TREE_VIEW_EDITOR(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GLADE_TYPE_TREE_VIEW_EDITOR))
#define GLADE_IS_TREE_VIEW_EDITOR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GLADE_TYPE_TREE_VIEW_EDITOR))
#define GLADE_TREE_VIEW_EDITOR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GLADE_TYPE_TREE_VIEW_EDITOR, GladeTreeViewEditorClass))

typedef struct _GladeTreeViewEditor        GladeTreeViewEditor;
typedef struct _GladeTreeViewEditorClass   GladeTreeViewEditorClass;

struct _GladeTreeViewEditor
{
  GtkBox  parent;

  GtkWidget *embed;
  GtkWidget *embed_list_store;
  GtkWidget *embed_tree_store;
  GtkWidget *no_model_message;
};

struct _GladeTreeViewEditorClass
{
  GtkBoxClass parent;
};

GType            glade_tree_view_editor_get_type (void) G_GNUC_CONST;
GtkWidget       *glade_tree_view_editor_new      (GladeWidgetAdaptor *adaptor,
                                                  GladeEditable      *editable);

G_END_DECLS

#endif  /* _GLADE_TREE_VIEW_EDITOR_H_ */
