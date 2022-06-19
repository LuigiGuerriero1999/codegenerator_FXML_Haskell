/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
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

#include <config.h>

#include <gladeui/glade.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <glib/gi18n-lib.h>
#include <string.h>

#include "glade-model-data.h"
#include "glade-column-types.h"

GladeModelData *
glade_model_data_new (GType type, const gchar *column_name)
{
	GladeModelData *data = g_new0 (GladeModelData, 1);
	
	if (type != 0)
		g_value_init (&data->value, type);

	if (type == G_TYPE_STRING)
		data->i18n_translatable = TRUE;

	data->name = g_strdup (column_name);

	return data;
}

GladeModelData *
glade_model_data_copy (GladeModelData *data)
{
	if (!data)
		return NULL;

	GladeModelData *dup = g_new0 (GladeModelData, 1);
	
	if (G_VALUE_TYPE (&data->value) != 0)
	{
		g_value_init (&dup->value, G_VALUE_TYPE (&data->value));
		g_value_copy (&data->value, &dup->value);
	}

	dup->name              = g_strdup (data->name);

	dup->i18n_translatable = data->i18n_translatable;
	dup->i18n_context      = g_strdup (data->i18n_context);
	dup->i18n_comment      = g_strdup (data->i18n_comment);

	return dup;
}

void
glade_model_data_free (GladeModelData *data)
{
	if (data)
	{
		if (G_VALUE_TYPE (&data->value) != 0)
			g_value_unset (&data->value);
	
		g_free (data->name);
		g_free (data->i18n_context);
		g_free (data->i18n_comment);
		g_free (data);
	}
}

GNode *
glade_model_data_tree_copy (GNode *node)
{
	return g_node_copy_deep (node, (GCopyFunc)glade_model_data_copy, NULL);
}

static void
model_data_traverse_free (GNode *node,
			  gpointer data)
{
	glade_model_data_free ((GladeModelData *)node->data);
}

void
glade_model_data_tree_free (GNode *node)
{
	if (node)
	{
		g_node_traverse (node, G_IN_ORDER, G_TRAVERSE_ALL, -1, 
				 (GNodeTraverseFunc)model_data_traverse_free, NULL);
		g_node_destroy (node);
	}
}

GladeModelData *
glade_model_data_tree_get_data (GNode *data_tree, gint row, gint colnum)
{
	GNode *node;

	g_return_val_if_fail (data_tree != NULL, NULL);

	if ((node = g_node_nth_child (data_tree, row)) != NULL)
		if ((node = g_node_nth_child (node, colnum)) != NULL)
			return (GladeModelData *)node->data;

	return NULL;
}

void
glade_model_data_insert_column (GNode          *node,
				GType           type,
				const gchar    *column_name,
				gint            nth)
{
	GNode *row, *item;
	GladeModelData *data;

	g_return_if_fail (node != NULL);

	for (row = node->children; row; row = row->next)
	{
		g_return_if_fail (nth >= 0 && nth <= g_node_n_children (row));

		data = glade_model_data_new (type, column_name);
		item = g_node_new (data);
		g_node_insert (row, nth, item);
	}
}

void
glade_model_data_remove_column (GNode          *node,
				gint            nth)
{
	GNode *row, *item;
	GladeModelData *data;

	g_return_if_fail (node != NULL);

	for (row = node->children; row; row = row->next)
	{
		g_return_if_fail (nth >= 0 && nth < g_node_n_children (row));

		item = g_node_nth_child (row, nth);
		data = item->data;

		glade_model_data_free (data);
		g_node_destroy (item);
	}
}

void
glade_model_data_reorder_column (GNode          *node,
				 gint            column,
				 gint            nth)
{
	GNode *row, *item;

	g_return_if_fail (node != NULL);

	for (row = node->children; row; row = row->next)
	{
		g_return_if_fail (nth >= 0 && nth < g_node_n_children (row));

		item = g_node_nth_child (row, column);

		g_node_unlink (item);
		g_node_insert (row, nth, item);
	}
}

gint
glade_model_data_column_index (GNode          *node,
			       const gchar    *column_name)
{
	gint i;
	GNode *item;
	GladeModelData *data;

	g_return_val_if_fail (node != NULL, -1);

	for (i = 0, item = node->children->children; item; i++, item = item->next)
	{
		data = item->data;
		if (strcmp (data->name, column_name) == 0)
			return i;
	}
	return -1;
}

void
glade_model_data_column_rename (GNode          *node,
				const gchar    *column_name,
				const gchar    *new_name)
{
	gint idx;
	GNode *row, *iter;
	GladeModelData *data;

	g_return_if_fail (node != NULL);

	if ((idx = glade_model_data_column_index (node, column_name)) < 0)
		return;

	for (row = node->children; row; row = row->next)
	{
		iter = g_node_nth_child (row, idx);
		data = iter->data;
		g_free (data->name);
		data->name = g_strdup (new_name);
	}
}

GType
glade_model_data_tree_get_type (void)
{
	static GType type_id = 0;

	if (!type_id)
		type_id = g_boxed_type_register_static
			("GladeModelDataTree", 
			 (GBoxedCopyFunc) glade_model_data_tree_copy,
			 (GBoxedFreeFunc) glade_model_data_tree_free);
	return type_id;
}

/**************************** GladeEditorProperty *****************************/
enum {
	COLUMN_ROW = 0, /* row number */
	NUM_COLUMNS
};

typedef enum {
	SEQ_NONE,
	SEQ_HORIZONTAL,
	SEQ_VERTICAL
} EditSequence;

typedef struct
{
	GladeEditorProperty parent_instance;

	GtkTreeView  *view;
	GtkListStore *store;
	GtkTreeSelection *selection;
	GNode *pending_data_tree;

	EditSequence         sequence;

	/* Used for setting focus on newly added rows */
	gboolean             adding_row;
	gboolean             want_focus;
	gboolean             want_next_focus;
	gboolean             setting_focus;
	gint                 editing_row;
	gint                 editing_column;

	guint                next_focus_idle;
} GladeEPropModelData;

GLADE_MAKE_EPROP (GladeEPropModelData, glade_eprop_model_data)
#define GLADE_EPROP_MODEL_DATA(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GLADE_TYPE_EPROP_MODEL_DATA, GladeEPropModelData))
#define GLADE_EPROP_MODEL_DATA_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GLADE_TYPE_EPROP_MODEL_DATA, GladeEPropModelDataClass))
#define GLADE_IS_EPROP_MODEL_DATA(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GLADE_TYPE_EPROP_MODEL_DATA))
#define GLADE_IS_EPROP_MODEL_DATA_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GLADE_TYPE_EPROP_MODEL_DATA))
#define GLADE_EPROP_MODEL_DATA_GET_CLASS(o)    (G_TYPE_INSTANCE_GET_CLASS ((o), GLADE_EPROP_MODEL_DATA, GladeEPropModelDataClass))


static void eprop_data_focus_editing_cell (GladeEPropModelData *eprop_data);

static void
append_row (GNode *node, GList *columns)
{
	GladeModelData *data;
	GladeColumnType *column;
	GNode *row;
	GList *list;

	g_assert (node && columns);

	row = g_node_new (NULL);
	g_node_append (node, row);

	for (list = columns; list; list = list->next)
       	{
		column = list->data;
		data = glade_model_data_new (g_type_from_name (column->type_name), column->column_name);
		g_node_append_data (row, data);
	}
}

static void
clear_view (GladeEditorProperty *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);
	GtkTreeViewColumn   *column;
	
	/* Clear columns ... */
	while ((column = gtk_tree_view_get_column (eprop_data->view, 0)) != NULL)
		gtk_tree_view_remove_column (eprop_data->view, column);

	/* Clear store ... (this will unref the old store) */
	gtk_tree_view_set_model (eprop_data->view, NULL);
	
}

static gboolean
update_data_tree_idle (GladeEditorProperty *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);
	GValue               value = { 0, };
	
	g_value_init (&value, GLADE_TYPE_MODEL_DATA_TREE);
	g_value_take_boxed (&value, eprop_data->pending_data_tree);

	/* Only commit the value if it changed, otherwise this
	 * can trigger a load.. which we dont handle well in this
	 * editor 
	 */
	if (!glade_property_equals_value (eprop->property, &value))
		glade_editor_property_commit (eprop, &value);

	g_value_unset (&value);

	eprop_data->pending_data_tree = NULL;
	return FALSE;
}

static gboolean
update_and_focus_data_tree_idle (GladeEditorProperty *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);

	eprop_data->want_focus = TRUE;
	eprop_data->want_next_focus = TRUE;
	
	update_data_tree_idle (eprop);

	/* XXX Have to load it regardless if it changed, this is a slow and redundant way... */
	glade_editor_property_load (eprop, eprop->property);

	eprop_data->want_next_focus = FALSE;
	eprop_data->want_focus = FALSE;

	return FALSE;
}


static gboolean
focus_next_data_tree_idle (GladeEditorProperty *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);

	eprop_data->want_focus = TRUE;
	eprop_data->want_next_focus = TRUE;

	eprop_data_focus_editing_cell (eprop_data);

	eprop_data->want_next_focus = FALSE;
	eprop_data->want_focus = FALSE;

	eprop_data->next_focus_idle = 0;
	
	return FALSE;
}

static gboolean
focus_data_tree_idle (GladeEditorProperty *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);

	eprop_data->want_focus = TRUE;
	eprop_data_focus_editing_cell (eprop_data);
	eprop_data->want_focus = FALSE;

	return FALSE;
}

static void
glade_eprop_model_data_add_row (GladeEditorProperty *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);
	GValue               value = { 0, };
	GNode               *node = NULL;
	GList               *columns = NULL;

	glade_property_get (eprop->property, &node);
	glade_widget_property_get (eprop->property->widget, "columns", &columns);

	if (!columns)
		return;

	clear_view (eprop);

	if (!node)
		node = g_node_new (NULL);
	else
		node = glade_model_data_tree_copy (node);

	append_row (node, columns);

	eprop_data->adding_row = TRUE;

	g_value_init (&value, GLADE_TYPE_MODEL_DATA_TREE);
	g_value_take_boxed (&value, node);
	glade_editor_property_commit (eprop, &value);
	g_value_unset (&value);

	eprop_data->adding_row = FALSE;
}

static void
glade_eprop_model_data_delete_selected (GladeEditorProperty *eprop)
{
	GtkTreeIter iter;
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);
	GNode *data_tree = NULL, *row;
	gint rownum = -1;

	/* NOTE: This will trigger row-deleted below... */
	if (!gtk_tree_selection_get_selected (eprop_data->selection, NULL, &iter))
		return;

	gtk_tree_model_get (GTK_TREE_MODEL (eprop_data->store), &iter,
			    COLUMN_ROW, &rownum,
			    -1);
	g_assert (rownum >= 0);
	
	/* if theres a sected row, theres data... */
	glade_property_get (eprop->property, &data_tree);
	g_assert (data_tree);

	data_tree = glade_model_data_tree_copy (data_tree);
	row = g_node_nth_child (data_tree, rownum);

	g_node_unlink (row);
	glade_model_data_tree_free (row);

	if (eprop_data->pending_data_tree)
		glade_model_data_tree_free (eprop_data->pending_data_tree);

	eprop_data->pending_data_tree = data_tree;
	g_idle_add ((GSourceFunc)update_data_tree_idle, eprop);
}

static void
glade_eprop_model_data_add_clicked (GtkWidget *button, 
				    GladeEditorProperty *eprop)
{
	glade_eprop_model_data_add_row (eprop);
}

static void
glade_eprop_model_data_delete_clicked (GtkWidget *button, 
				       GladeEditorProperty *eprop)
{
	glade_eprop_model_data_delete_selected (eprop);
}

static void
glade_eprop_model_sequence_changed (GtkWidget           *combo, 
				    GladeEditorProperty *eprop)
{
	GladeEPropModelData   *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);

	eprop_data->sequence = gtk_combo_box_get_active (GTK_COMBO_BOX (combo));
}

static gboolean
eprop_treeview_key_press (GtkWidget           *treeview,
			  GdkEventKey         *event,
			  GladeEditorProperty *eprop)
{
	if (event->keyval == GDK_Delete)
	{
		glade_eprop_model_data_delete_selected (eprop);
		return TRUE;
	}
	else if ((event->state & GDK_CONTROL_MASK) != 0 &&
		 (event->keyval == GDK_n || event->keyval == GDK_N))
	{
		glade_eprop_model_data_add_row (eprop);
		return TRUE;
	}

	return FALSE;
} 

static gboolean
data_changed_idle (GladeEditorProperty *eprop)
{
	GladeEPropModelData   *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);
	GNode                 *data_tree = NULL, *new_tree, *row;
	GtkTreeIter            iter;
	gint                   rownum;

	glade_property_get (eprop->property, &data_tree);
	g_assert (data_tree);

	new_tree = g_node_new (NULL);

	if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL (eprop_data->store), &iter))
	{
		do 
		{
			gtk_tree_model_get (GTK_TREE_MODEL (eprop_data->store), &iter,
					    COLUMN_ROW, &rownum, -1);


			if ((row = g_node_nth_child (data_tree, rownum)) != NULL)
			{
				/* Make a new tree by copying row by row... */
				row = glade_model_data_tree_copy (row);
				g_node_append (new_tree, row);
			}
		} 
		while (gtk_tree_model_iter_next (GTK_TREE_MODEL (eprop_data->store), &iter));
	}

	/* Were already in an idle, no need to idle from here...  */
	if (eprop_data->pending_data_tree)
		glade_model_data_tree_free (eprop_data->pending_data_tree);
	eprop_data->pending_data_tree = new_tree;
	update_data_tree_idle (eprop);

	return FALSE;
}

static void
eprop_treeview_row_deleted (GtkTreeModel *tree_model,
			    GtkTreePath  *path,
			    GladeEditorProperty *eprop)
{
	if (eprop->loading) return;

	g_idle_add ((GSourceFunc)data_changed_idle, eprop);
}


static void
glade_eprop_model_data_finalize (GObject *object)
{
	/* Chain up */
	GObjectClass *parent_class = g_type_class_peek_parent (G_OBJECT_GET_CLASS (object));
	//GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (object);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static GtkListStore *
eprop_model_data_generate_store (GladeEditorProperty *eprop)
{
	GtkListStore   *store = NULL;
	GladeModelData *iter_data, *row_data;
	GNode          *data_tree = NULL, *iter_node, *row_node;
	GArray         *gtypes = g_array_new (FALSE, TRUE, sizeof (GType));
	GtkTreeIter     iter;
	gint            column_num, row_num;
	GType           index_type = G_TYPE_INT, string_type = G_TYPE_STRING, pointer_type = G_TYPE_POINTER;

	glade_property_get (eprop->property, &data_tree);

	if (!data_tree || !data_tree->children || !data_tree->children->children)
		return NULL;

	/* Generate store with tailored column types */
	g_array_append_val (gtypes, index_type);
	for (iter_node = data_tree->children->children; iter_node; iter_node = iter_node->next)
	{
		iter_data = iter_node->data;
		if (G_VALUE_TYPE (&iter_data->value) == 0)
			g_array_append_val (gtypes, pointer_type);
		else if (G_VALUE_TYPE (&iter_data->value) == GDK_TYPE_PIXBUF)
			g_array_append_val (gtypes, string_type);
		else
			g_array_append_val (gtypes, G_VALUE_TYPE (&iter_data->value));
	}
	store = gtk_list_store_newv (gtypes->len, (GType *)gtypes->data);
	g_array_free (gtypes, TRUE);

	/* Now populate the store with data */
	for (row_num = 0, row_node = data_tree->children; row_node; 
	     row_num++, row_node = row_node->next)
	{
		row_data = row_node->data;

		gtk_list_store_append (store, &iter);
		gtk_list_store_set (store, &iter, COLUMN_ROW, row_num, -1);

		for (column_num = NUM_COLUMNS, iter_node = row_node->children; iter_node; 
		     column_num++, iter_node = iter_node->next)
		{
			iter_data = iter_node->data;

			if (G_VALUE_TYPE (&iter_data->value) == 0)
				continue;

			/* Special case, show the filename in the cellrenderertext */
			if (G_VALUE_TYPE (&iter_data->value) == GDK_TYPE_PIXBUF)
			{
				GObject *object = g_value_get_object (&iter_data->value);
				gchar *filename = NULL;
				if (object)
					filename = g_object_get_data (object, "GladeFileName");

				gtk_list_store_set (store, &iter, 
						    column_num, filename,
						    -1);
			}
			else
				gtk_list_store_set_value (store, &iter, column_num, &iter_data->value);
		}
	}
	return store;
}

static void
value_toggled (GtkCellRendererToggle *cell,
	       gchar                 *path,
	       GladeEditorProperty   *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);
	GtkTreeIter          iter;
	gint                 colnum = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "column-number"));
	gint                 row;
	GNode               *data_tree = NULL;
	GladeModelData      *data;
	gboolean             active;

	if (!gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL (eprop_data->store), &iter, path))
		return;

	gtk_tree_model_get (GTK_TREE_MODEL (eprop_data->store), &iter,
			    COLUMN_ROW, &row,
			    NUM_COLUMNS + colnum, &active,
			    -1);

	glade_property_get (eprop->property, &data_tree);

	/* if we are editing, then there is data in the datatree */
	g_assert (data_tree);

	data_tree = glade_model_data_tree_copy (data_tree);

	data = glade_model_data_tree_get_data (data_tree, row, colnum);

	g_value_set_boolean (&data->value, !active);

	eprop_data->editing_row    = row;
	eprop_data->editing_column = colnum;
	if (eprop_data->pending_data_tree)
		glade_model_data_tree_free (eprop_data->pending_data_tree);

	eprop_data->pending_data_tree = data_tree;
	g_idle_add ((GSourceFunc)update_and_focus_data_tree_idle, eprop);
}

static void
value_i18n_activate (GladeCellRendererIcon    *cell,
		     const gchar              *path,
		     GladeEditorProperty      *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);
	GtkTreeIter          iter;
	gint                 colnum = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "column-number"));
	gint                 row;
	GNode               *data_tree = NULL;
	GladeModelData      *data;
	gchar               *new_text;
	gboolean             has_context_dummy;

	if (!gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL (eprop_data->store), &iter, path))
		return;


	gtk_tree_model_get (GTK_TREE_MODEL (eprop_data->store), &iter,
			    COLUMN_ROW, &row,
			    -1);

	glade_property_get (eprop->property, &data_tree);

	/* if we are editing, then there is data in the datatree */
	g_assert (data_tree);

	data_tree = glade_model_data_tree_copy (data_tree);

	data = glade_model_data_tree_get_data (data_tree, row, colnum);
	g_assert (G_VALUE_TYPE (&data->value) == G_TYPE_STRING);

	new_text = g_value_dup_string (&data->value);
	
	if (glade_editor_property_show_i18n_dialog (NULL,
						    GLADE_PROJECT_FORMAT_GTKBUILDER,
						    &new_text,
						    &data->i18n_context,
						    &data->i18n_comment,
						    &has_context_dummy,
						    &data->i18n_translatable))
	{
		g_value_set_string (&data->value, new_text);
		
		eprop_data->editing_row    = row;
		eprop_data->editing_column = colnum;
		if (eprop_data->pending_data_tree)
			glade_model_data_tree_free (eprop_data->pending_data_tree);
		
		eprop_data->pending_data_tree = data_tree;
		g_idle_add ((GSourceFunc)update_and_focus_data_tree_idle, eprop);
	}
	else
		glade_model_data_tree_free (data_tree);

	g_free (new_text);
}

static void
value_text_edited (GtkCellRendererText *cell,
		   const gchar         *path,
		   const gchar         *new_text,
		   GladeEditorProperty *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);
	GtkTreeIter          iter;
	gint                 colnum = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "column-number"));
	gint                 row;
	GNode               *data_tree = NULL;
	GladeModelData      *data;
	GValue              *value;

	if (!gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL (eprop_data->store), &iter, path))
		return;

	gtk_tree_model_get (GTK_TREE_MODEL (eprop_data->store), &iter,
			    COLUMN_ROW, &row,
			    -1);

	glade_property_get (eprop->property, &data_tree);

	/* if we are editing, then there is data in the datatree */
	g_assert (data_tree);

	data_tree = glade_model_data_tree_copy (data_tree);

	data = glade_model_data_tree_get_data (data_tree, row, colnum);

	/* Untranslate string and update value in tree. */
	if (G_VALUE_HOLDS_ENUM (&data->value) || G_VALUE_HOLDS_FLAGS (&data->value))
		value = glade_utils_value_from_string (G_VALUE_TYPE (&data->value), 
						       glade_get_value_from_displayable
						       (G_VALUE_TYPE (&data->value), new_text),
						       eprop->property->widget->project,
						       eprop->property->widget);
	else
		value = glade_utils_value_from_string (G_VALUE_TYPE (&data->value), new_text,
						       eprop->property->widget->project,
						       eprop->property->widget);


	g_value_copy (value, &data->value);
	g_value_unset (value);
	g_free (value);
	
	eprop_data->editing_row    = row;
	eprop_data->editing_column = colnum;
	if (eprop_data->pending_data_tree)
		glade_model_data_tree_free (eprop_data->pending_data_tree);

	eprop_data->pending_data_tree = data_tree;
	g_idle_add ((GSourceFunc)update_and_focus_data_tree_idle, eprop);
}


static void 
enum_flags_format_cell_data (GtkCellLayout *cell_layout,
			     GtkCellRenderer *cell,
			     GtkTreeModel *tree_model,
			     GtkTreeIter *iter,
			     gpointer data)
{
	gint    colnum = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "column-number"));
	GValue  value = { 0, };
	gchar  *string;

	gtk_tree_model_get_value (tree_model, iter,
				  NUM_COLUMNS + colnum, &value);

	string = glade_utils_string_from_value (&value, GLADE_PROJECT_FORMAT_GTKBUILDER);

	g_object_set (cell, "text", string && string[0] ? 
		      glade_get_displayable_value (G_VALUE_TYPE (&value), string) : "", NULL);
	
	g_free (string);

	g_value_unset (&value);
}


static void
data_editing_started (GtkCellRenderer       *cell,
		      GtkCellEditable       *editable,
		      gchar                 *path,
		      GladeEditorProperty   *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);
	gint                 colnum = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "column-number"));
	gint                 row;
	GtkTreeIter          iter;

	if (!gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL (eprop_data->store), &iter, path))
		return;

	gtk_tree_model_get (GTK_TREE_MODEL (eprop_data->store), &iter,
			    COLUMN_ROW, &row,
			    -1);

	eprop_data->editing_row    = row;
	eprop_data->editing_column = colnum;
}

static void
data_editing_canceled (GtkCellRenderer       *renderer,
		       GladeEditorProperty   *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);

	if (eprop_data->setting_focus)
		return;

	g_idle_add ((GSourceFunc)focus_data_tree_idle, eprop);
}

static GtkTreeViewColumn *
eprop_model_generate_column (GladeEditorProperty *eprop,
			     gint                 colnum,
			     GladeModelData      *data)
{
	GtkTreeViewColumn *column = gtk_tree_view_column_new ();
	GtkCellRenderer   *renderer = NULL;
	GtkAdjustment     *adjustment;
	GtkListStore      *store;
	GType              type = G_TYPE_INVALID;

	gtk_tree_view_column_set_title (column, data->name);
	gtk_tree_view_column_set_resizable (column, TRUE);
	gtk_tree_view_column_set_expand (column, TRUE);

	type = G_VALUE_TYPE (&data->value);

	/* Support enum and flag types, and a hardcoded list of fundamental types */
	if (type == G_TYPE_CHAR ||
	    type == G_TYPE_UCHAR ||
	    type == G_TYPE_STRING ||
	    type == GDK_TYPE_PIXBUF)
	{
		/* Text renderer */
		renderer = gtk_cell_renderer_text_new ();

		g_object_set (G_OBJECT (renderer), 
			      "editable", TRUE, 
			      "ellipsize", PANGO_ELLIPSIZE_END,
			      "width", 90,
			      NULL);

		gtk_tree_view_column_pack_start (column, renderer, FALSE);
		gtk_tree_view_column_set_attributes (column, renderer, 
						     "text", NUM_COLUMNS + colnum,
						     NULL);

		if (type == G_TYPE_CHAR ||
		    type == G_TYPE_UCHAR)
		{
			/* XXX restrict to 1 char !! */
		}

		g_signal_connect (G_OBJECT (renderer), "edited",
				  G_CALLBACK (value_text_edited), eprop);

		/* Trigger i18n dialog from here */
		if (type == G_TYPE_STRING)
		{
			GtkCellRenderer *icon_renderer = glade_cell_renderer_icon_new ();

			g_object_set (G_OBJECT (icon_renderer), 
				      "activatable", TRUE,
				      "icon-name", GTK_STOCK_EDIT,
				      NULL);

			gtk_tree_view_column_pack_start (column, icon_renderer, FALSE);

			g_object_set_data (G_OBJECT (icon_renderer), "column-number", GINT_TO_POINTER (colnum));
			g_signal_connect (G_OBJECT (icon_renderer), "activate",
					  G_CALLBACK (value_i18n_activate), eprop);
		}

	}
	else if (type == G_TYPE_BOOLEAN)
	{
		/* Toggle renderer */
		renderer = gtk_cell_renderer_toggle_new ();
		g_object_set (G_OBJECT (renderer), "activatable", TRUE, NULL);
		gtk_tree_view_column_pack_start (column, renderer, FALSE);
		gtk_tree_view_column_set_attributes (column, renderer, 
						     "active", NUM_COLUMNS + colnum,
						     NULL);
		g_signal_connect (G_OBJECT (renderer), "toggled",
				  G_CALLBACK (value_toggled), eprop);
	}
		/* Check renderer */
	else if (type == G_TYPE_INT ||
		 type == G_TYPE_UINT ||
		 type == G_TYPE_LONG ||
		 type == G_TYPE_ULONG ||
		 type == G_TYPE_INT64 ||
		 type == G_TYPE_UINT64 ||
		 type == G_TYPE_FLOAT ||
		 type == G_TYPE_DOUBLE)
	{
		/* Spin renderer */
		renderer = gtk_cell_renderer_spin_new ();
		adjustment = (GtkAdjustment *)gtk_adjustment_new (0, -G_MAXDOUBLE, G_MAXDOUBLE, 100, 100, 0);
		g_object_set (G_OBJECT (renderer), 
			      "editable", TRUE, 
			      "adjustment", adjustment, 
			      NULL);

		gtk_tree_view_column_pack_start (column, renderer, TRUE);
		gtk_tree_view_column_set_attributes (column, renderer, 
						     "text", NUM_COLUMNS + colnum,
						     NULL);

		if (type == G_TYPE_FLOAT ||
		    type == G_TYPE_DOUBLE)
			g_object_set (G_OBJECT (renderer), "digits", 2, NULL);

		g_signal_connect (G_OBJECT (renderer), "edited",
				  G_CALLBACK (value_text_edited), eprop);
		
	}
	else if (G_TYPE_IS_ENUM (type))
	{
		/* Combo renderer */
		renderer = gtk_cell_renderer_combo_new ();
		store = glade_utils_liststore_from_enum_type (type, FALSE);
		g_object_set (G_OBJECT (renderer), 
			      "editable", TRUE, 
			      "text-column", 0,
			      "has-entry", FALSE,
			      "model", store,
			      NULL);
		gtk_tree_view_column_pack_start (column, renderer, TRUE);
		gtk_tree_view_column_set_attributes (column, renderer, 
						     "text", NUM_COLUMNS + colnum,
						     NULL);


		gtk_cell_layout_set_cell_data_func  (GTK_CELL_LAYOUT (column),
						     renderer,
						     (GtkCellLayoutDataFunc)enum_flags_format_cell_data,
						     NULL, NULL);

		g_signal_connect (G_OBJECT (renderer), "edited",
				  G_CALLBACK (value_text_edited), eprop);

	}
	else if (G_TYPE_IS_FLAGS (type))
	{
		/* Export a flags dialog from glade-editor-property... */
		renderer = gtk_cell_renderer_text_new ();
		g_object_set (G_OBJECT (renderer), "editable", FALSE, NULL);
		gtk_tree_view_column_pack_start (column, renderer, FALSE);
		gtk_tree_view_column_set_attributes (column, renderer, 
						     "text", NUM_COLUMNS + colnum,
						     NULL);

	}
	else /* All uneditable types at this point (currently we dont do object data here, TODO) */
	{
		/* text renderer and object dialog (or raw text for pixbuf) */
		renderer = gtk_cell_renderer_text_new ();
		g_object_set (G_OBJECT (renderer), "editable", FALSE, NULL);
		gtk_tree_view_column_pack_start (column, renderer, FALSE);
	}

	g_signal_connect (G_OBJECT (renderer), "editing-started",
			  G_CALLBACK (data_editing_started), eprop);

	g_signal_connect (G_OBJECT (renderer), "editing-canceled",
			  G_CALLBACK (data_editing_canceled), eprop);

	g_object_set_data (G_OBJECT (renderer), "column-number", GINT_TO_POINTER (colnum));
	g_object_set_data_full (G_OBJECT (column), "column-type", g_memdup (&type, sizeof (GType)), g_free);

	return column;
}

static void
eprop_model_data_generate_columns (GladeEditorProperty *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);
	GladeModelData      *iter_data;
	GtkTreeViewColumn   *column;
	GNode               *data_tree = NULL, *iter_node;
	gint                 colnum;

	glade_property_get (eprop->property, &data_tree);
	
	if (!data_tree || !data_tree->children || !data_tree->children->children)
		return;

	/* Append new columns */
	for (colnum = 0, iter_node = data_tree->children->children; iter_node; 
	     colnum++, iter_node = iter_node->next)
	{
		iter_data = iter_node->data;

		column = eprop_model_generate_column (eprop, colnum, iter_data);
		gtk_tree_view_append_column (eprop_data->view, column);
	}
}

static void
eprop_data_focus_new (GladeEPropModelData *eprop_data)
{

	/* Focus and edit the first column of a newly added row */
	if (eprop_data->store)
	{
		GtkTreePath *new_item_path;
		GtkTreeIter  iter;
		GtkTreeViewColumn *column;
		gint n_children;

		n_children = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (eprop_data->store), NULL);

		if ((column = gtk_tree_view_get_column (eprop_data->view, eprop_data->editing_column)) != NULL &&
		    n_children > 0 &&  gtk_tree_model_iter_nth_child (GTK_TREE_MODEL (eprop_data->store),
								      &iter, NULL, n_children - 1))
			
		{
			GType *column_type = g_object_get_data (G_OBJECT (column), "column-type");

			new_item_path = gtk_tree_model_get_path (GTK_TREE_MODEL (eprop_data->store), &iter);

			eprop_data->setting_focus = TRUE;

			gtk_widget_grab_focus (GTK_WIDGET (eprop_data->view));
			gtk_tree_view_expand_to_path (eprop_data->view, new_item_path);
			gtk_tree_view_set_cursor (eprop_data->view, new_item_path,
						  column, *column_type != G_TYPE_BOOLEAN);

			eprop_data->setting_focus = FALSE;
			
			gtk_tree_path_free (new_item_path);
		}
	}
}

static void
eprop_data_focus_editing_cell (GladeEPropModelData *eprop_data)
{
	/* Focus and edit the first column of a newly added row */
	if (!eprop_data->setting_focus && eprop_data->store && eprop_data->want_focus && 
	    eprop_data->editing_column >= 0 && eprop_data->editing_row >= 0)
	{
		GtkTreePath *item_path;
		GtkTreeIter  iter;
		GtkTreeViewColumn *column;
		gint row, col, rows, cols;
		GList *column_list;

		column_list = gtk_tree_view_get_columns (eprop_data->view);
		cols = g_list_length (column_list);
		g_list_free (column_list);

		rows = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (eprop_data->store), NULL);

		col = eprop_data->editing_column;	
		row = eprop_data->editing_row;

		if (eprop_data->want_next_focus)
		{
			switch (eprop_data->sequence)
			{
			case SEQ_HORIZONTAL:
				if (++col >= cols)
				{
					col = 0;
					if (++row >= rows)
						row = 0;
				}
				break;
			case SEQ_VERTICAL:
				if (++row >= rows)
				{
					row = 0;
					if (++col >= cols)
						col = 0;
				}
				break;
			case SEQ_NONE:	
			default:
				break;
			}
		}

		if ((column = gtk_tree_view_get_column (eprop_data->view, col)) != NULL &&
		    gtk_tree_model_iter_nth_child (GTK_TREE_MODEL (eprop_data->store), &iter, NULL, row))
		{
			GType *column_type = g_object_get_data (G_OBJECT (column), "column-type");

			item_path = gtk_tree_model_get_path (GTK_TREE_MODEL (eprop_data->store), &iter);

			eprop_data->setting_focus = TRUE;

			gtk_widget_grab_focus (GTK_WIDGET (eprop_data->view));
			gtk_tree_view_expand_to_path (eprop_data->view, item_path);
			gtk_tree_view_set_cursor (eprop_data->view, item_path, column, 
						  eprop_data->want_next_focus && 
						  eprop_data->sequence != SEQ_NONE &&
						  *column_type != G_TYPE_BOOLEAN);

			gtk_tree_path_free (item_path);

			eprop_data->setting_focus = FALSE;
		}
	}
}


static void
glade_eprop_model_data_load (GladeEditorProperty *eprop, 
			     GladeProperty       *property)
{
	GladeEditorPropertyClass *parent_class = 
		g_type_class_peek_parent (GLADE_EDITOR_PROPERTY_GET_CLASS (eprop));
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);

	clear_view (eprop);

	/* Chain up in a clean state... */
	parent_class->load (eprop, property);

	gtk_tree_view_set_model (eprop_data->view, NULL);
	if (!property)
		return;

	if ((eprop_data->store = eprop_model_data_generate_store (eprop)) != NULL)
	{
		eprop_data->selection = gtk_tree_view_get_selection (eprop_data->view);

		/* Pass ownership of the store to the view... */
		gtk_tree_view_set_model (eprop_data->view, GTK_TREE_MODEL (eprop_data->store));
		g_object_unref (G_OBJECT (eprop_data->store));
		
		g_signal_connect (G_OBJECT (eprop_data->store), "row-deleted",
				  G_CALLBACK (eprop_treeview_row_deleted),
				  eprop);
	}

	/* Create new columns with renderers */
	eprop_model_data_generate_columns (eprop);

	if (eprop_data->store)
	{
		if (eprop_data->adding_row)
			eprop_data_focus_new (eprop_data);
		else if (eprop_data->want_focus && 
			 eprop_data->editing_row >= 0 && eprop_data->editing_column >= 0)
		{
			if (eprop_data->want_next_focus && eprop_data->next_focus_idle == 0)
				eprop_data->next_focus_idle = 
					g_idle_add ((GSourceFunc)focus_next_data_tree_idle, eprop);
			else
				eprop_data_focus_editing_cell (eprop_data);
		}
	}
}

static GtkWidget *
glade_eprop_model_data_create_input (GladeEditorProperty *eprop)
{
	GladeEPropModelData *eprop_data = GLADE_EPROP_MODEL_DATA (eprop);
	GtkWidget *vbox, *hbox, *button, *swin, *label, *combo;
	gchar *string;

	vbox = gtk_vbox_new (FALSE, 2);
	
	hbox = gtk_hbox_new (FALSE, 4);

	eprop_data->sequence = SEQ_NONE;

	/* hbox with add/remove row buttons on the right... */
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);

	string = g_strdup_printf ("<b>%s</b>", _("Add and remove rows:"));
	label = gtk_label_new (string);
	g_free (string);
	gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_misc_set_padding (GTK_MISC (label), 2, 0);
	gtk_box_pack_start (GTK_BOX (hbox), label,  FALSE, FALSE, 0);
	
	button = gtk_button_new ();
	gtk_button_set_image (GTK_BUTTON (button),
			      gtk_image_new_from_stock (GTK_STOCK_ADD, GTK_ICON_SIZE_BUTTON));
	gtk_box_pack_start (GTK_BOX (hbox), button,  FALSE, FALSE, 0);

	g_signal_connect (G_OBJECT (button), "clicked",
			  G_CALLBACK (glade_eprop_model_data_add_clicked), 
			  eprop_data);

	button = gtk_button_new ();
	gtk_button_set_image (GTK_BUTTON (button),
			      gtk_image_new_from_stock (GTK_STOCK_REMOVE, GTK_ICON_SIZE_BUTTON));
	gtk_box_pack_start (GTK_BOX (hbox), button,  FALSE, FALSE, 0);

	g_signal_connect (G_OBJECT (button), "clicked",
			  G_CALLBACK (glade_eprop_model_data_delete_clicked), 
			  eprop_data);

	/* separator... */
	label = gtk_label_new ("");
	gtk_box_pack_start (GTK_BOX (hbox), label,  TRUE, TRUE, 0);

	string = g_strdup_printf ("<b>%s</b>", _("Sequential editing:"));
	label = gtk_label_new (string);
	g_free (string);
	gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_misc_set_padding (GTK_MISC (label), 2, 0);
	gtk_box_pack_start (GTK_BOX (hbox), label,  FALSE, FALSE, 0);

	combo = gtk_combo_box_new_text ();
	gtk_combo_box_append_text (GTK_COMBO_BOX (combo), _("Off"));
	gtk_combo_box_append_text (GTK_COMBO_BOX (combo), _("Horizontal"));
	gtk_combo_box_append_text (GTK_COMBO_BOX (combo), _("Vertical"));


	gtk_combo_box_set_active (GTK_COMBO_BOX (combo), eprop_data->sequence);

	gtk_box_pack_start (GTK_BOX (hbox), combo,  FALSE, FALSE, 0);

	g_signal_connect (G_OBJECT (combo), "changed",
			  G_CALLBACK (glade_eprop_model_sequence_changed), 
			  eprop_data);
	

	/* Pack treeview/swindow on the left... */
	swin = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (swin), GTK_SHADOW_IN);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (swin), GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
	gtk_box_pack_start (GTK_BOX (vbox), swin, TRUE, TRUE, 0);

	eprop_data->view = (GtkTreeView *)gtk_tree_view_new ();

	g_signal_connect (eprop_data->view, "key-press-event",
			  G_CALLBACK (eprop_treeview_key_press),
			  eprop);

	gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (eprop_data->view), GTK_TREE_VIEW_GRID_LINES_BOTH);
	gtk_tree_view_set_reorderable (GTK_TREE_VIEW (eprop_data->view), TRUE);
	gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (eprop_data->view), TRUE);
	gtk_container_add (GTK_CONTAINER (swin), GTK_WIDGET (eprop_data->view));

	g_object_set (G_OBJECT (vbox), "height-request", 300, NULL);
	
	gtk_widget_show_all (vbox);
	return vbox;
}
