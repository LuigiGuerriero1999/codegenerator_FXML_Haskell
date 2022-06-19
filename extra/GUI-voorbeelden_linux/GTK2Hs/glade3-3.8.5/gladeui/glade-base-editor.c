/*
 * Copyright (C) 2006 Juan Pablo Ugarte.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Authors:
 *   Juan Pablo Ugarte <juanpablougarte@gmail.com>
 */

#include "config.h"

/**
 * SECTION:glade-base-editor
 * @Short_Description: A customisable editor
 *
 * Convenience object to edit containers where placeholders do not make sense, like GtkMenubar.
 */

#include "glade.h"
#include "glade-marshallers.h"
#include "glade-editor-property.h"
#include "glade-base-editor.h"
#include "glade-app.h"
#include "glade-popup.h"
#include "glade-accumulators.h"

#include <string.h>
#include <glib/gi18n-lib.h>
#include <gdk/gdkkeysyms.h>

typedef enum
{
  GLADE_BASE_EDITOR_GTYPE,
  GLADE_BASE_EDITOR_CLASS_NAME,
  GLADE_BASE_EDITOR_TYPES_N_COLUMNS
} GladeBaseEditorChildEnum;

typedef enum
{
  GLADE_BASE_EDITOR_GWIDGET,
  GLADE_BASE_EDITOR_OBJECT,
  GLADE_BASE_EDITOR_TYPE_NAME,
  GLADE_BASE_EDITOR_NAME,
  GLADE_BASE_EDITOR_CHILD_TYPES,
  GLADE_BASE_EDITOR_N_COLUMNS
} GladeBaseEditorEnum;

typedef enum {
  ADD_ROOT = 0,
  ADD_SIBLING,
  ADD_CHILD
} GladeBaseEditorAddMode;

typedef struct
{
  GType parent_type;
  GtkTreeModel *children;
} ChildTypeTab;

struct _GladeBaseEditorPrivate
{
  GladeWidget *gcontainer;      /* The container we are editing */

  /* Editor UI */
  GtkWidget *paned, *table, *treeview, *main_scroll, *notebook;
  GtkWidget *remove_button;
  GladeSignalEditor *signal_editor;

  GList *child_types;

  GtkTreeModel *model;
  GladeProject *project;

  /* Add button data */
  GType add_type;

  /* Temporal variables */
  GtkTreeIter iter;             /* used in idle functions */
  gint row;

  gboolean updating_treeview;

  GtkSizeGroup *group;

  guint properties_idle;
};

enum
{
  SIGNAL_CHILD_SELECTED,
  SIGNAL_CHANGE_TYPE,
  SIGNAL_GET_DISPLAY_NAME,
  SIGNAL_BUILD_CHILD,
  SIGNAL_DELETE_CHILD,
  SIGNAL_MOVE_CHILD,
  LAST_SIGNAL
};

enum
{
  PROP_0,
  PROP_CONTAINER,
  N_PROPERTIES
};

static GParamSpec *properties[N_PROPERTIES];
static guint glade_base_editor_signals[LAST_SIGNAL] = { 0 };

static GtkVBoxClass *parent_class = NULL;

static void glade_base_editor_set_container (GladeBaseEditor * editor,
                                             GObject * container);
static void glade_base_editor_block_callbacks (GladeBaseEditor * editor,
                                               gboolean block);


static void
reset_child_types (GladeBaseEditor * editor)
{
  GList *l;
  ChildTypeTab *tab;

  for (l = editor->priv->child_types; l; l = l->next)
    {
      tab = l->data;
      g_object_unref (tab->children);
      g_free (tab);
    }
  g_list_free (editor->priv->child_types);
  editor->priv->child_types = NULL;
}


static gint
sort_type_by_hierarchy (ChildTypeTab * a, ChildTypeTab * b)
{
  if (g_type_is_a (a->parent_type, b->parent_type))
    return -1;

  return 1;
}

static GtkTreeModel *
get_children_model_for_type (GladeBaseEditor * editor, GType type)
{
  GList *l;
  for (l = editor->priv->child_types; l; l = l->next)
    {
      ChildTypeTab *tab = l->data;
      if (g_type_is_a (type, tab->parent_type))
        return tab->children;
    }
  return NULL;
}

static GtkTreeModel *
get_children_model_for_child_type (GladeBaseEditor * editor, GType type)
{
  GList *l;
  GtkTreeModel *model = NULL;

  /* Get deep derived classes first and work up the sorted heirarchy */
  for (l = g_list_last (editor->priv->child_types); model == NULL && l;
       l = l->prev)
    {
      ChildTypeTab *tab = l->data;
      GtkTreeIter iter;
      GType iter_type;

      if (!gtk_tree_model_get_iter_first (tab->children, &iter))
        continue;

      do
        {
          gtk_tree_model_get (tab->children, &iter,
                              GLADE_BASE_EDITOR_GTYPE, &iter_type, -1);

          /* Find exact match types in this case */
          if (iter_type == type)
            model = tab->children;

        }
      while (model == NULL && gtk_tree_model_iter_next (tab->children, &iter));
    }

  return model;
}

static gboolean
glade_base_editor_get_type_info (GladeBaseEditor * e,
                                 GtkTreeIter * retiter, GType child_type, ...)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  GType type;

  model = get_children_model_for_child_type (e, child_type);

  if (!model || gtk_tree_model_get_iter_first (model, &iter) == FALSE)
    return FALSE;

  do
    {
      gtk_tree_model_get (model, &iter, GLADE_BASE_EDITOR_GTYPE, &type, -1);

      if (child_type == type)
        {
          va_list args;
          va_start (args, child_type);
          gtk_tree_model_get_valist (model, &iter, args);
          va_end (args);
          if (retiter)
            *retiter = iter;
          return TRUE;
        }
    }
  while (gtk_tree_model_iter_next (model, &iter));

  return FALSE;
}


static gchar *
glade_base_editor_get_display_name (GladeBaseEditor * editor,
                                    GladeWidget * gchild)
{
  gchar *retval;
  g_signal_emit (editor,
                 glade_base_editor_signals[SIGNAL_GET_DISPLAY_NAME],
                 0, gchild, &retval);
  return retval;
}

static void
glade_base_editor_fill_store_real (GladeBaseEditor * e,
                                   GladeWidget * gwidget, GtkTreeIter * parent)
{
  GList *children, *l;
  GtkTreeIter iter;

  children = glade_widget_get_children (gwidget);

  for (l = children; l; l = l->next)
    {
      GladeWidget *gchild;
      GObject     *child = (GObject *) l->data;
      gchar       *type_name = NULL, *name;

      gchild = glade_widget_get_from_gobject (child);

      /* Have to check parents here for compatibility (could be the parenting menuitem of this menu
       * supports a menuitem...) */
      if (glade_base_editor_get_type_info (e, NULL,
					   G_OBJECT_TYPE (child),
					   GLADE_BASE_EDITOR_CLASS_NAME,
					   &type_name, -1))
	{
	  gtk_tree_store_append (GTK_TREE_STORE (e->priv->model), &iter, parent);

	  name = glade_base_editor_get_display_name (e, gchild);

	  gtk_tree_store_set (GTK_TREE_STORE (e->priv->model), &iter,
			      GLADE_BASE_EDITOR_GWIDGET, gchild,
			      GLADE_BASE_EDITOR_OBJECT, child,
			      GLADE_BASE_EDITOR_TYPE_NAME, type_name,
			      GLADE_BASE_EDITOR_NAME, name,
			      GLADE_BASE_EDITOR_CHILD_TYPES,
			      get_children_model_for_child_type (e, G_OBJECT_TYPE (child)),
			      -1);

	  glade_base_editor_fill_store_real (e, gchild, &iter);

	  g_free (name);
	  g_free (type_name);
	}
      else
	glade_base_editor_fill_store_real (e, gchild, parent);
    }

  g_list_free (children);
}

static void
glade_base_editor_fill_store (GladeBaseEditor * e)
{
  gtk_tree_store_clear (GTK_TREE_STORE (e->priv->model));
  gtk_tree_view_set_model (GTK_TREE_VIEW (e->priv->treeview), NULL);
  glade_base_editor_fill_store_real (e, e->priv->gcontainer, NULL);
  gtk_tree_view_set_model (GTK_TREE_VIEW (e->priv->treeview), e->priv->model);

  gtk_tree_view_expand_all (GTK_TREE_VIEW (e->priv->treeview));

}

static gboolean
glade_base_editor_get_child_selected (GladeBaseEditor * e, GtkTreeIter * iter)
{
  GtkTreeSelection *sel =
      gtk_tree_view_get_selection (GTK_TREE_VIEW (e->priv->treeview));
  return gtk_tree_selection_get_selected (sel, NULL, iter);
}

/* Forward declaration for glade_base_editor_project_widget_name_changed */
static void
glade_base_editor_project_widget_name_changed (GladeProject * project,
                                               GladeWidget * widget,
                                               GladeBaseEditor * editor);


static GladeWidget *
glade_base_editor_delegate_build_child (GladeBaseEditor * editor,
                                        GladeWidget * parent, GType type)
{
  GladeWidget *child = NULL;
  g_signal_emit (editor, glade_base_editor_signals[SIGNAL_BUILD_CHILD],
                 0, parent, type, &child);
  return child;
}

static gboolean
glade_base_editor_delegate_delete_child (GladeBaseEditor * editor,
                                         GladeWidget * parent,
                                         GladeWidget * child)
{
  gboolean retval;

  g_signal_emit (editor, glade_base_editor_signals[SIGNAL_DELETE_CHILD],
                 0, parent, child, &retval);

  return retval;
}

static void
glade_base_editor_name_activate (GtkEntry * entry, GladeWidget * gchild)
{
  const gchar *text = gtk_entry_get_text (GTK_ENTRY (entry));
  GladeBaseEditor *editor = g_object_get_data (G_OBJECT (entry), "editor");

  if (text && text[0] && strcmp (glade_widget_get_name (gchild), text))
    {
      g_signal_handlers_block_by_func (glade_widget_get_project (gchild),
                                       glade_base_editor_project_widget_name_changed, editor);
      glade_command_set_name (gchild, text);
      g_signal_handlers_unblock_by_func (glade_widget_get_project (gchild),
                                         glade_base_editor_project_widget_name_changed, editor);
    }
}

static void
glade_base_editor_table_attach (GladeBaseEditor * e,
                                GtkWidget * child1, GtkWidget * child2)
{

  GtkTable *table = GTK_TABLE (e->priv->table);
  gint row = e->priv->row;
       
  if (child1)
    {
      gtk_table_attach (table, child1, 0, 1, row, row + 1,
			GTK_EXPAND | GTK_FILL, GTK_FILL, 2, 0);
      gtk_widget_show (child1);
    }
       
  if (child2)
    {
      gtk_table_attach (table, child2, 1, 2, row, row + 1,
			0, GTK_FILL, 2, 0);
      gtk_widget_show (child2);
      
      gtk_size_group_add_widget (e->priv->group, child2);
    }

  e->priv->row++;
}

static void
glade_base_editor_clear (GladeBaseEditor * editor)
{
  GladeBaseEditorPrivate *e = editor->priv;

  gtk_container_foreach (GTK_CONTAINER (e->table), 
			 (GtkCallback)gtk_widget_destroy, NULL);
  e->row = 0;
  gtk_widget_set_sensitive (e->remove_button, FALSE);
  glade_signal_editor_load_widget (e->signal_editor, NULL);
}

static void
glade_base_editor_treeview_cursor_changed (GtkTreeView * treeview,
                                           GladeBaseEditor * editor)
{
  GladeBaseEditorPrivate *e = editor->priv;
  GtkTreeIter iter;
  GObject *child;
  GladeWidget *gchild;

  g_return_if_fail (GTK_IS_TREE_VIEW (treeview));

  if (!glade_base_editor_get_child_selected (editor, &iter))
    return;

  glade_base_editor_clear (editor);
  gtk_widget_set_sensitive (e->remove_button, TRUE);

  gtk_tree_model_get (e->model, &iter,
                      GLADE_BASE_EDITOR_GWIDGET, &gchild,
                      GLADE_BASE_EDITOR_OBJECT, &child, -1);

  g_object_unref (gchild);
  g_object_unref (child);

  /* Emit child-selected signal and let the user add the properties */
  g_signal_emit (editor, glade_base_editor_signals[SIGNAL_CHILD_SELECTED],
                 0, gchild);

  /* Update Signal Editor */
  glade_signal_editor_load_widget (e->signal_editor, gchild);
}

static gboolean
glade_base_editor_update_properties_idle (gpointer data)
{
  GladeBaseEditor *editor = (GladeBaseEditor *) data;
  glade_base_editor_treeview_cursor_changed
      (GTK_TREE_VIEW (editor->priv->treeview), editor);
  editor->priv->properties_idle = 0;
  return FALSE;
}

/* XXX Can we make it crisper by removing this idle ?? */
static void
glade_base_editor_update_properties (GladeBaseEditor * editor)
{
  g_return_if_fail (GLADE_IS_BASE_EDITOR (editor));

  if (!editor->priv->properties_idle)
    editor->priv->properties_idle =
        g_idle_add (glade_base_editor_update_properties_idle, editor);
}

static void
glade_base_editor_set_cursor (GladeBaseEditor * e, GtkTreeIter * iter)
{
  GtkTreePath *path;
  GtkTreeIter real_iter;

  if (iter == NULL && glade_base_editor_get_child_selected (e, &real_iter))
    iter = &real_iter;

  if (iter && (path = gtk_tree_model_get_path (e->priv->model, iter)))
    {
      gtk_tree_view_set_cursor (GTK_TREE_VIEW (e->priv->treeview), path, NULL,
                                FALSE);
      gtk_tree_path_free (path);
    }
}

static gboolean
glade_base_editor_find_child_real (GladeBaseEditor * e,
                                   GladeWidget * gchild, GtkTreeIter * iter)
{
  GtkTreeModel *model = e->priv->model;
  GtkTreeIter child_iter;
  GladeWidget *child;

  do
    {
      gtk_tree_model_get (model, iter, GLADE_BASE_EDITOR_GWIDGET, &child, -1);
      g_object_unref (child);

      if (child == gchild)
        return TRUE;

      if (gtk_tree_model_iter_children (model, &child_iter, iter))
        if (glade_base_editor_find_child_real (e, gchild, &child_iter))
          {
            *iter = child_iter;
            return TRUE;
          }
    }
  while (gtk_tree_model_iter_next (model, iter));

  return FALSE;
}

static gboolean
glade_base_editor_find_child (GladeBaseEditor * e,
                              GladeWidget * child, GtkTreeIter * iter)
{
  if (gtk_tree_model_get_iter_first (e->priv->model, iter))
    return glade_base_editor_find_child_real (e, child, iter);

  return FALSE;
}

static void
glade_base_editor_select_child (GladeBaseEditor * e, GladeWidget * child)
{
  GtkTreeIter iter;

  if (glade_base_editor_find_child (e, child, &iter))
    glade_base_editor_set_cursor (e, &iter);
}

static void
glade_base_editor_child_change_type (GladeBaseEditor * editor,
                                     GtkTreeIter * iter, GType type)
{
  GladeWidget *gchild, *gparent;
  GObject *child;
  gchar *class_name;
  gboolean retval;

  glade_base_editor_block_callbacks (editor, TRUE);

  /* Get old widget data */
  gtk_tree_model_get (editor->priv->model, iter,
                      GLADE_BASE_EDITOR_GWIDGET, &gchild,
                      GLADE_BASE_EDITOR_OBJECT, &child, -1);

  g_object_unref (gchild);
  g_object_unref (child);

  if (type == G_OBJECT_TYPE (child) || 
      !gchild || !glade_widget_get_parent (gchild))
    {
      glade_base_editor_block_callbacks (editor, FALSE);
      return;
    }

  gparent = glade_widget_get_parent (gchild);

  /* Start of glade-command */

  if (glade_base_editor_get_type_info (editor, NULL,
                                       type,
                                       GLADE_BASE_EDITOR_CLASS_NAME,
                                       &class_name, -1))
    {
      glade_command_push_group (_("Setting object type on %s to %s"),
                                glade_widget_get_name (gchild), class_name);
      g_free (class_name);
    }
  else
    {
      glade_base_editor_block_callbacks (editor, FALSE);
      return;
    }

  g_signal_emit (editor,
                 glade_base_editor_signals[SIGNAL_CHANGE_TYPE],
                 0, gchild, type, &retval);

  /* End of glade-command */
  glade_command_pop_group ();

  /* Update properties */
  glade_base_editor_update_properties (editor);

  glade_base_editor_block_callbacks (editor, FALSE);
}

static void
glade_base_editor_type_changed (GtkComboBox * widget, GladeBaseEditor * e)
{
  GtkTreeIter iter, combo_iter;
  GType type;

  if (!glade_base_editor_get_child_selected (e, &iter))
    return;

  gtk_combo_box_get_active_iter (widget, &combo_iter);

  gtk_tree_model_get (gtk_combo_box_get_model (widget), &combo_iter,
                      GLADE_BASE_EDITOR_GTYPE, &type, -1);

  glade_base_editor_child_change_type (e, &iter, type);
}

static void
glade_base_editor_child_type_edited (GtkCellRendererText * cell,
                                     const gchar * path_string,
                                     const gchar * new_text,
                                     GladeBaseEditor * editor)
{
  GladeBaseEditorPrivate *e = editor->priv;
  GtkTreeModel *child_class;
  GtkTreePath *path;
  GtkTreeIter iter, combo_iter;
  GType type;
  gchar *type_name = NULL;

  path = gtk_tree_path_new_from_string (path_string);
  gtk_tree_model_get_iter (e->model, &iter, path);
  gtk_tree_path_free (path);

  gtk_tree_model_get (e->model, &iter,
                      GLADE_BASE_EDITOR_TYPE_NAME, &type_name,
                      GLADE_BASE_EDITOR_CHILD_TYPES, &child_class, -1);

  if (g_strcmp0 (type_name, new_text) == 0)
    {
      g_free (type_name);
      g_object_unref (child_class);
      return;
    }

  /* Lookup GType */
  if (!gtk_tree_model_get_iter_first (child_class, &combo_iter))
    {
      g_free (type_name);
      g_object_unref (child_class);
      return;
    }

  g_free (type_name);

  do
    {
      gtk_tree_model_get (child_class, &combo_iter,
                          GLADE_BASE_EDITOR_GTYPE, &type,
                          GLADE_BASE_EDITOR_CLASS_NAME, &type_name, -1);

      if (strcmp (type_name, new_text) == 0)
	{
	  g_free (type_name);
	  break;
	}

      g_free (type_name);
    }
  while (gtk_tree_model_iter_next (child_class, &combo_iter));

  glade_base_editor_child_change_type (editor, &iter, type);
}

static void
glade_base_editor_reorder_children (GladeBaseEditor * editor,
                                    GtkTreeIter * child)
{
  GtkTreeModel *model = editor->priv->model;
  GladeWidget *gchild;
  GladeProperty *property;
  GtkTreeIter parent, iter;
  gint position = 0;

  if (gtk_tree_model_iter_parent (model, &parent, child))
    gtk_tree_model_iter_children (model, &iter, &parent);
  else
    gtk_tree_model_get_iter_first (model, &iter);

  do
    {
      gtk_tree_model_get (model, &iter, GLADE_BASE_EDITOR_GWIDGET, &gchild, -1);
      g_object_unref (gchild);

      if ((property = glade_widget_get_property (gchild, "position")) != NULL)
        glade_command_set_property (property, position);
      position++;
    }
  while (gtk_tree_model_iter_next (model, &iter));
}

static void
glade_base_editor_add_child (GladeBaseEditor       *editor,
                             GType                  type, 
			     GladeBaseEditorAddMode add_mode)
{
  GladeBaseEditorPrivate *e = editor->priv;
  GtkTreeIter iter, new_iter;
  GladeWidget *gparent, *gchild_new;
  gchar *name, *class_name;
  gboolean selected_iter = FALSE;

  glade_base_editor_block_callbacks (editor, TRUE);

  gparent = e->gcontainer;

  if (add_mode != ADD_ROOT &&
      (selected_iter = glade_base_editor_get_child_selected (editor, &iter)))
    {
      if (add_mode == ADD_CHILD)
	{
	  gtk_tree_model_get (e->model, &iter,
			      GLADE_BASE_EDITOR_GWIDGET, &gparent, -1);
	  g_object_unref (gparent);
	}
      else if (add_mode == ADD_SIBLING &&
	       gtk_tree_model_iter_parent (e->model, &new_iter, &iter))
        {
          gtk_tree_model_get (e->model, &new_iter,
                              GLADE_BASE_EDITOR_GWIDGET, &gparent, -1);
	  g_object_unref (gparent);
        }
    }

  if (!glade_base_editor_get_type_info (editor, NULL, type,
                                        GLADE_BASE_EDITOR_CLASS_NAME,
                                        &class_name, -1))
    return;

  glade_command_push_group (_("Add a %s to %s"), class_name,
                            glade_widget_get_name (gparent));

  /* Build Child */
  gchild_new = glade_base_editor_delegate_build_child (editor, gparent, type);

  if (gchild_new == NULL)
    {
      glade_command_pop_group ();
      return;
    }

  if (selected_iter)
    {
      if (add_mode == ADD_CHILD)
        gtk_tree_store_append (GTK_TREE_STORE (editor->priv->model), &new_iter,
                               &iter);
      else
        gtk_tree_store_insert_after (GTK_TREE_STORE (editor->priv->model),
                                     &new_iter, NULL, &iter);
    }
  else
    gtk_tree_store_append (GTK_TREE_STORE (editor->priv->model), &new_iter,
                           NULL);

  name = glade_base_editor_get_display_name (editor, gchild_new);

  gtk_tree_store_set (GTK_TREE_STORE (editor->priv->model), &new_iter,
                      GLADE_BASE_EDITOR_GWIDGET, gchild_new,
                      GLADE_BASE_EDITOR_OBJECT,
                      glade_widget_get_object (gchild_new),
                      GLADE_BASE_EDITOR_TYPE_NAME, class_name,
                      GLADE_BASE_EDITOR_NAME, name,
                      GLADE_BASE_EDITOR_CHILD_TYPES,
                      get_children_model_for_type (editor,
                                                   G_OBJECT_TYPE (glade_widget_get_object (gparent))),
		      -1);

  glade_base_editor_reorder_children (editor, &new_iter);

  gtk_tree_view_expand_all (GTK_TREE_VIEW (e->treeview));
  glade_base_editor_set_cursor (editor, &new_iter);

  glade_command_pop_group ();

  glade_base_editor_block_callbacks (editor, FALSE);

  g_free (name);
  g_free (class_name);
}


static void
glade_base_editor_add_item_activate (GtkMenuItem * menuitem,
                                     GladeBaseEditor * e)
{
  GObject *item = G_OBJECT (menuitem);
  GType type = GPOINTER_TO_INT (g_object_get_data (item, "object_type"));
  GladeBaseEditorAddMode add_mode =
      GPOINTER_TO_INT (g_object_get_data (item, "object_add_mode"));

  glade_base_editor_add_child (e, type, add_mode);
}

static GtkWidget *
glade_base_editor_popup (GladeBaseEditor * editor, GladeWidget * widget)
{
  GtkWidget *popup, *item;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GType iter_type;
  gchar *label;
  gchar *class_name;

  if ((model =
       get_children_model_for_child_type (editor,
                                          G_OBJECT_TYPE (glade_widget_get_object (widget)))) == NULL)
    model =
      get_children_model_for_type (editor,
				   G_OBJECT_TYPE (glade_widget_get_object (editor->priv->gcontainer)));

  g_assert (model);

  popup = gtk_menu_new ();

  if (gtk_tree_model_get_iter_first (model, &iter))
    do
      {
        gtk_tree_model_get (model, &iter,
                            GLADE_BASE_EDITOR_GTYPE, &iter_type,
                            GLADE_BASE_EDITOR_CLASS_NAME, &class_name, -1);

        label = g_strdup_printf (_("Add %s"), class_name);

        item = gtk_menu_item_new_with_label (label);
        gtk_widget_show (item);

        g_object_set_data (G_OBJECT (item), "object_type",
                           GINT_TO_POINTER (iter_type));

        g_object_set_data (G_OBJECT (item), "object_add_mode",
                           GINT_TO_POINTER (ADD_SIBLING));

        g_signal_connect (item, "activate",
                          G_CALLBACK (glade_base_editor_add_item_activate),
                          editor);
        gtk_menu_shell_append (GTK_MENU_SHELL (popup), item);

        g_free (label);
        g_free (class_name);

      }
    while (gtk_tree_model_iter_next (model, &iter));


  if ((model =
       get_children_model_for_type (editor, G_OBJECT_TYPE (glade_widget_get_object (widget)))) &&
      gtk_tree_model_get_iter_first (model, &iter))
    do
      {
        gtk_tree_model_get (model, &iter,
                            GLADE_BASE_EDITOR_GTYPE, &iter_type,
                            GLADE_BASE_EDITOR_CLASS_NAME, &class_name, -1);

        label = g_strdup_printf (_("Add child %s"), class_name);

        item = gtk_menu_item_new_with_label (label);
        gtk_widget_show (item);

        g_object_set_data (G_OBJECT (item), "object_type",
                           GINT_TO_POINTER (iter_type));

        g_object_set_data (G_OBJECT (item), "object_add_mode",
                           GINT_TO_POINTER (ADD_CHILD));

        g_signal_connect (item, "activate",
                          G_CALLBACK (glade_base_editor_add_item_activate),
                          editor);
        gtk_menu_shell_append (GTK_MENU_SHELL (popup), item);

        g_free (label);
        g_free (class_name);

      }
    while (gtk_tree_model_iter_next (model, &iter));

  return popup;
}

static gint
glade_base_editor_popup_handler (GtkWidget * treeview,
                                 GdkEventButton * event, GladeBaseEditor * e)
{
  GtkTreePath *path;
  GtkWidget *popup;

  if (glade_popup_is_popup_event (event))
    {
      if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (treeview),
                                         (gint) event->x, (gint) event->y,
                                         &path, NULL, NULL, NULL))
        {
          GtkTreeIter iter;
          GladeWidget *gwidget;

          gtk_tree_view_set_cursor (GTK_TREE_VIEW (treeview), path, NULL,
                                    FALSE);

          gtk_tree_model_get_iter (e->priv->model, &iter, path);
          gtk_tree_model_get (e->priv->model, &iter,
                              GLADE_BASE_EDITOR_GWIDGET, &gwidget, -1);


          popup = glade_base_editor_popup (e, gwidget);

          gtk_tree_path_free (path);

          gtk_menu_popup (GTK_MENU (popup), NULL, NULL, NULL, NULL,
                          event->button, event->time);
        }
      return TRUE;
    }

  return FALSE;
}


static void
glade_base_editor_add_activate (GtkButton * button, GladeBaseEditor * e)
{
  if (e->priv->add_type)
    glade_base_editor_add_child (e, e->priv->add_type, ADD_ROOT);
}

static void
glade_base_editor_delete_child (GladeBaseEditor * e)
{
  GladeWidget *child, *gparent;
  GtkTreeIter iter, parent;

  if (!glade_base_editor_get_child_selected (e, &iter))
    return;

  gtk_tree_model_get (e->priv->model, &iter,
                      GLADE_BASE_EDITOR_GWIDGET, &child, -1);

  if (gtk_tree_model_iter_parent (e->priv->model, &parent, &iter))
    gtk_tree_model_get (e->priv->model, &parent,
                        GLADE_BASE_EDITOR_GWIDGET, &gparent, -1);
  else
    gparent = e->priv->gcontainer;

  glade_command_push_group (_("Delete %s child from %s"),
                            glade_widget_get_name (child),
                            glade_widget_get_name (gparent));

  /* Emit delete-child signal */
  glade_base_editor_delegate_delete_child (e, gparent, child);

  glade_command_pop_group ();
}


static gboolean
glade_base_editor_treeview_key_press_event (GtkWidget * widget,
                                            GdkEventKey * event,
                                            GladeBaseEditor * e)
{
  if (event->keyval == GDK_KEY_Delete)
    glade_base_editor_delete_child (e);

  return FALSE;
}

static void
glade_base_editor_delete_activate (GtkButton * button, GladeBaseEditor * e)
{
  glade_base_editor_delete_child (e);
}

static gboolean
glade_base_editor_is_child (GladeBaseEditor * e,
                            GladeWidget * gchild, gboolean valid_type)
{
  GladeWidget *gcontainer = glade_widget_get_parent (gchild);

  if (!gcontainer)
    return FALSE;

  if (valid_type)
    {
      GObject *child = glade_widget_get_object (gchild);

      if (glade_widget_get_internal (gchild) ||
          glade_base_editor_get_type_info (e, NULL,
                                           G_OBJECT_TYPE (child), -1) == FALSE)
        return FALSE;

      gcontainer = e->priv->gcontainer;
    }
  else
    {
      GtkTreeIter iter;
      if (glade_base_editor_get_child_selected (e, &iter))
        gtk_tree_model_get (e->priv->model, &iter,
                            GLADE_BASE_EDITOR_GWIDGET, &gcontainer, -1);
      else
        return FALSE;
    }

  while ((gchild = glade_widget_get_parent (gchild)))
    if (gchild == gcontainer)
      return TRUE;

  return FALSE;
}

static gboolean
glade_base_editor_update_treeview_idle (gpointer data)
{
  GladeBaseEditor *e = ((GladeBaseEditor *) data);
  GList *selection = glade_project_selection_get (e->priv->project);

  glade_base_editor_block_callbacks (e, TRUE);

  glade_base_editor_fill_store (e);
  glade_base_editor_clear (e);

  gtk_tree_view_expand_all (GTK_TREE_VIEW (e->priv->treeview));

  if (selection)
    {
      GladeWidget *widget =
          glade_widget_get_from_gobject (G_OBJECT (selection->data));
      if (glade_base_editor_is_child (e, widget, TRUE))
        glade_base_editor_select_child (e, widget);
    }

  e->priv->updating_treeview = FALSE;
  glade_base_editor_block_callbacks (e, FALSE);

  return FALSE;
}

static void
glade_base_editor_project_widget_name_changed (GladeProject * project,
                                               GladeWidget * widget,
                                               GladeBaseEditor * editor)
{
  GladeWidget *selected_child;
  GtkTreeIter iter;

  if (glade_base_editor_get_child_selected (editor, &iter))
    {
      gtk_tree_model_get (editor->priv->model, &iter,
                          GLADE_BASE_EDITOR_GWIDGET, &selected_child, -1);
      if (widget == selected_child)
        glade_base_editor_update_properties (editor);

      g_object_unref (G_OBJECT (selected_child));
    }
}

static void
glade_base_editor_project_closed (GladeProject * project, GladeBaseEditor * e)
{
  glade_base_editor_set_container (e, NULL);
}

static void
glade_base_editor_reorder (GladeBaseEditor * editor, GtkTreeIter * iter)
{
  GladeBaseEditorPrivate *e = editor->priv;
  GladeWidget *gchild, *gparent;
  GtkTreeIter parent_iter;
  gboolean retval;

  glade_command_push_group (_("Reorder %s's children"),
                            glade_widget_get_name (e->gcontainer));

  gtk_tree_model_get (e->model, iter, GLADE_BASE_EDITOR_GWIDGET, &gchild, -1);
  g_object_unref (G_OBJECT (gchild));

  if (gtk_tree_model_iter_parent (e->model, &parent_iter, iter))
    {
      gtk_tree_model_get (e->model, &parent_iter,
                          GLADE_BASE_EDITOR_GWIDGET, &gparent, -1);
      g_object_unref (G_OBJECT (gparent));
    }
  else
    gparent = e->gcontainer;

  g_signal_emit (editor, glade_base_editor_signals[SIGNAL_MOVE_CHILD],
                 0, gparent, gchild, &retval);

  if (retval)
    glade_base_editor_reorder_children (editor, iter);
  else
    {
      glade_base_editor_clear (editor);
      glade_base_editor_fill_store (editor);
      glade_base_editor_find_child (editor, gchild, &editor->priv->iter);
    }

  glade_command_pop_group ();
}

static gboolean
glade_base_editor_drag_and_drop_idle (gpointer data)
{
  GladeBaseEditor *e = (GladeBaseEditor *) data;

  glade_base_editor_reorder (e, &e->priv->iter);
  gtk_tree_view_expand_all (GTK_TREE_VIEW (e->priv->treeview));
  glade_base_editor_set_cursor (e, &e->priv->iter);
  glade_base_editor_block_callbacks (e, FALSE);

  return FALSE;
}

static void
glade_base_editor_row_inserted (GtkTreeModel * model,
                                GtkTreePath * path,
                                GtkTreeIter * iter, GladeBaseEditor * e)
{
  e->priv->iter = *iter;
  glade_base_editor_block_callbacks (e, TRUE);
  g_idle_add (glade_base_editor_drag_and_drop_idle, e);
}

static void
glade_base_editor_project_remove_widget (GladeProject * project,
                                         GladeWidget * widget,
                                         GladeBaseEditor * e)
{
  if (widget == e->priv->gcontainer)
    {
      glade_base_editor_set_container (e, NULL);
      return;
    }

  if (glade_base_editor_is_child (e, widget, TRUE))
    {
      GtkTreeIter iter;
      if (glade_base_editor_find_child (e, widget, &iter))
        {
          gtk_tree_store_remove (GTK_TREE_STORE (e->priv->model), &iter);
          glade_base_editor_clear (e);
        }
    }

  if (glade_widget_get_internal (widget) && 
      glade_base_editor_is_child (e, widget, FALSE))
    glade_base_editor_update_properties (e);
}

static void
glade_base_editor_project_add_widget (GladeProject * project,
                                      GladeWidget * widget, GladeBaseEditor * e)
{
  if (e->priv->updating_treeview)
    return;

  if (glade_base_editor_is_child (e, widget, TRUE))
    {
      e->priv->updating_treeview = TRUE;
      g_idle_add (glade_base_editor_update_treeview_idle, e);
    }

  if (glade_widget_get_internal (widget) && 
      glade_base_editor_is_child (e, widget, FALSE))
    glade_base_editor_update_properties (e);
}

static gboolean
glade_base_editor_update_display_name (GtkTreeModel * model,
                                       GtkTreePath * path,
                                       GtkTreeIter * iter, gpointer data)
{
  GladeBaseEditor *editor = (GladeBaseEditor *) data;
  GladeWidget *gchild;
  gchar *name;

  gtk_tree_model_get (model, iter, GLADE_BASE_EDITOR_GWIDGET, &gchild, -1);

  name = glade_base_editor_get_display_name (editor, gchild);

  gtk_tree_store_set (GTK_TREE_STORE (editor->priv->model), iter,
                      GLADE_BASE_EDITOR_NAME, name, -1);
  g_free (name);
  g_object_unref (G_OBJECT (gchild));

  return FALSE;
}

static void
glade_base_editor_project_changed (GladeProject * project,
                                   GladeCommand * command,
                                   gboolean forward, GladeBaseEditor * editor)
{
  gtk_tree_model_foreach (editor->priv->model,
                          glade_base_editor_update_display_name, editor);
}



static void
glade_base_editor_project_disconnect (GladeBaseEditor * editor)
{
  GladeBaseEditorPrivate *e = editor->priv;

  if (e->project == NULL)
    return;

  g_signal_handlers_disconnect_by_func (e->project,
                                        glade_base_editor_project_closed,
                                        editor);

  g_signal_handlers_disconnect_by_func (e->project,
                                        glade_base_editor_project_remove_widget,
                                        editor);

  g_signal_handlers_disconnect_by_func (e->project,
                                        glade_base_editor_project_add_widget,
                                        editor);

  g_signal_handlers_disconnect_by_func (e->project,
                                        glade_base_editor_project_widget_name_changed,
                                        editor);

  g_signal_handlers_disconnect_by_func (e->project,
                                        glade_base_editor_project_changed,
                                        editor);


  if (e->properties_idle)
    g_source_remove (e->properties_idle);
  e->properties_idle = 0;
}

static void
glade_base_editor_set_container (GladeBaseEditor * editor, GObject * container)
{
  GladeBaseEditorPrivate *e = editor->priv;

  glade_base_editor_project_disconnect (editor);

  if (container == NULL)
    {
      reset_child_types (editor);

      e->gcontainer = NULL;
      e->project = NULL;
      glade_base_editor_block_callbacks (editor, TRUE);
      glade_base_editor_clear (editor);

      gtk_tree_view_set_model (GTK_TREE_VIEW (editor->priv->treeview), NULL);
      gtk_tree_store_clear (GTK_TREE_STORE (editor->priv->model));
      gtk_tree_view_set_model (GTK_TREE_VIEW (editor->priv->treeview),
                               editor->priv->model);

      gtk_widget_set_sensitive (e->paned, FALSE);
      glade_base_editor_block_callbacks (editor, FALSE);

      glade_signal_editor_load_widget (e->signal_editor, NULL);

      g_object_notify_by_pspec (G_OBJECT (editor), properties[PROP_CONTAINER]);
      return;
    }

  gtk_widget_set_sensitive (e->paned, TRUE);

  e->gcontainer = glade_widget_get_from_gobject (container);

  e->project = glade_widget_get_project (e->gcontainer);

  g_signal_connect (e->project, "close",
                    G_CALLBACK (glade_base_editor_project_closed), editor);

  g_signal_connect (e->project, "remove-widget",
                    G_CALLBACK (glade_base_editor_project_remove_widget),
                    editor);

  g_signal_connect (e->project, "add-widget",
                    G_CALLBACK (glade_base_editor_project_add_widget), editor);

  g_signal_connect (e->project, "widget-name-changed",
                    G_CALLBACK (glade_base_editor_project_widget_name_changed),
                    editor);

  g_signal_connect (e->project, "changed",
                    G_CALLBACK (glade_base_editor_project_changed), editor);

  g_object_notify_by_pspec (G_OBJECT (editor), properties[PROP_CONTAINER]);
}

/*************************** GladeBaseEditor Class ****************************/
static void
glade_base_editor_finalize (GObject * object)
{
  GladeBaseEditor *cobj = GLADE_BASE_EDITOR (object);

  g_free (cobj->priv);

  G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
glade_base_editor_dispose (GObject * object)
{
  GladeBaseEditor *cobj = GLADE_BASE_EDITOR (object);

  reset_child_types (cobj);

  glade_base_editor_project_disconnect (cobj);
  cobj->priv->project = NULL;

  if (cobj->priv->group)
    {
      g_object_unref (cobj->priv->group);
      cobj->priv->group = NULL;
    }

  G_OBJECT_CLASS (parent_class)->dispose (object);
}

static void
glade_base_editor_set_property (GObject * object,
                                guint prop_id,
                                const GValue * value, GParamSpec * pspec)
{
  GladeBaseEditor *editor = GLADE_BASE_EDITOR (object);

  switch (prop_id)
    {
      case PROP_CONTAINER:
        glade_base_editor_set_container (editor, g_value_get_object (value));
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}

static void
glade_base_editor_get_property (GObject * object,
                                guint prop_id,
                                GValue * value, GParamSpec * pspec)
{
  GladeBaseEditor *editor = GLADE_BASE_EDITOR (object);

  switch (prop_id)
    {
      case PROP_CONTAINER:
        g_value_set_object (value, glade_widget_get_object (editor->priv->gcontainer));
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}


/* Default handlers */
static gboolean
glade_base_editor_change_type (GladeBaseEditor * editor,
                               GladeWidget * gchild, GType type)
{
  GladeWidget *parent, *gchild_new;
  GList *children, *l;
  GObject *child, *child_new;
  GtkTreeIter iter;
  gchar *name, *class_name;

  parent = glade_widget_get_parent (gchild);

  if (glade_base_editor_get_type_info (editor, NULL, type,
                                       GLADE_BASE_EDITOR_CLASS_NAME,
                                       &class_name, -1) == FALSE)
    return TRUE;

  child = glade_widget_get_object (gchild);
  name = g_strdup (glade_widget_get_name (gchild));
  glade_base_editor_find_child (editor, gchild, &iter);

  /* Delete old widget first, we cant assume the old and new widget can live in 
   * the same parent simultaniously */
  glade_base_editor_delegate_delete_child (editor, parent, gchild);

  /* Create new widget */
  gchild_new = glade_base_editor_delegate_build_child (editor, parent, type);

  child_new = glade_widget_get_object (gchild_new);

  /* Cut and Paste childrens */
  if ((children = glade_widget_get_children (gchild)) != NULL)
    {
      GList *gchildren = NULL;

      l = children;
      while (l)
        {
          GladeWidget *w = glade_widget_get_from_gobject (l->data);

          if (w && !glade_widget_get_internal (w))
            gchildren = g_list_prepend (gchildren, w);

          l = g_list_next (l);
        }

      if (gchildren)
        {
          glade_command_dnd (gchildren, gchild_new, NULL);

          g_list_free (children);
          g_list_free (gchildren);
        }
    }

  /* Copy properties */
  glade_widget_copy_properties (gchild_new, gchild, TRUE, TRUE);

  /* Apply packing properties to the new object 
   * 
   * No need to use GladeCommand here on the newly created widget,
   * they just become the initial state for this object.
   */
  l = gchild->packing_properties;
  while (l)
    {
      GladeProperty      *orig_prop = (GladeProperty *) l->data;
      GladePropertyClass *pclass = orig_prop->klass;
      GladeProperty      *dup_prop = glade_widget_get_property (gchild_new, pclass->id);
      glade_property_set_value (dup_prop, orig_prop->value);
      l = g_list_next (l);
    }

  /* Set the name */
  glade_command_set_name (gchild_new, name);

  if (GTK_IS_WIDGET (child_new))
    gtk_widget_show_all (GTK_WIDGET (child_new));

  /* XXX We should update the widget name in the visible tree here too */
  gtk_tree_store_set (GTK_TREE_STORE (editor->priv->model), &iter,
                      GLADE_BASE_EDITOR_GWIDGET, gchild_new,
                      GLADE_BASE_EDITOR_OBJECT, child_new,
                      GLADE_BASE_EDITOR_TYPE_NAME, class_name, -1);
  g_free (class_name);
  g_free (name);

  return TRUE;
}

static gchar *
glade_base_editor_get_display_name_impl (GladeBaseEditor * editor,
                                         GladeWidget * gchild)
{
  return g_strdup (glade_widget_get_name (gchild));
}

static GladeWidget *
glade_base_editor_build_child (GladeBaseEditor * editor,
                               GladeWidget * gparent, GType type)
{
  return glade_command_create (glade_widget_adaptor_get_by_type (type),
                               gparent, NULL,
                               glade_widget_get_project (gparent));
}

static gboolean
glade_base_editor_move_child (GladeBaseEditor * editor,
                              GladeWidget * gparent, GladeWidget * gchild)
{
  GList list = { 0, };

  if (gparent != glade_widget_get_parent (gchild))
    {
      list.data = gchild;
      glade_command_dnd (&list, gparent, NULL);
    }

  return TRUE;
}

static gboolean
glade_base_editor_delete_child_impl (GladeBaseEditor * editor,
                                     GladeWidget * gparent,
                                     GladeWidget * gchild)
{
  GList list = { 0, };

  list.data = gchild;
  glade_command_delete (&list);

  return TRUE;
}

static void
glade_base_editor_class_init (GladeBaseEditorClass * klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  parent_class = g_type_class_peek_parent (klass);

  object_class->finalize = glade_base_editor_finalize;
  object_class->dispose = glade_base_editor_dispose;
  object_class->set_property = glade_base_editor_set_property;
  object_class->get_property = glade_base_editor_get_property;

  klass->change_type = glade_base_editor_change_type;
  klass->get_display_name = glade_base_editor_get_display_name_impl;
  klass->build_child = glade_base_editor_build_child;
  klass->delete_child = glade_base_editor_delete_child_impl;
  klass->move_child = glade_base_editor_move_child;

  properties[PROP_CONTAINER] =
    g_param_spec_object ("container",
                         _("Container"),
                         _("The container object this editor is currently editing"),
                         G_TYPE_OBJECT,
                         G_PARAM_READWRITE);
  
  /* Install all properties */
  g_object_class_install_properties (object_class, N_PROPERTIES, properties);

  /**
   * GladeBaseEditor::child-selected:
   * @gladebaseeditor: the #GladeBaseEditor which received the signal.
   * @gchild: the selected #GladeWidget.
   *
   * Emited when the user selects a child in the editor's treeview.
   * You can add the relevant child properties here using 
   * glade_base_editor_add_default_properties() and glade_base_editor_add_properties() 
   * You can also add labels with glade_base_editor_add_label to make the
   * editor look pretty.
   */
  glade_base_editor_signals[SIGNAL_CHILD_SELECTED] =
      g_signal_new ("child-selected",
                    G_TYPE_FROM_CLASS (object_class),
                    G_SIGNAL_RUN_LAST,
                    G_STRUCT_OFFSET (GladeBaseEditorClass, child_selected),
                    NULL, NULL,
                    glade_marshal_VOID__OBJECT, G_TYPE_NONE, 1, G_TYPE_OBJECT);

  /**
   * GladeBaseEditor::child-change-type:
   * @gladebaseeditor: the #GladeBaseEditor which received the signal.
   * @child: the #GObject being changed.
   * @type: the new type for @child.
   *
   * Returns: TRUE to stop signal emision.
   */
  glade_base_editor_signals[SIGNAL_CHANGE_TYPE] =
      g_signal_new ("change-type",
                    G_TYPE_FROM_CLASS (object_class),
                    G_SIGNAL_RUN_LAST,
                    G_STRUCT_OFFSET (GladeBaseEditorClass, change_type),
                    glade_boolean_handled_accumulator, NULL,
                    glade_marshal_BOOLEAN__OBJECT_UINT,
                    G_TYPE_BOOLEAN, 2, G_TYPE_OBJECT, G_TYPE_UINT);

  /**
   * GladeBaseEditor::get-display-name:
   * @gladebaseeditor: the #GladeBaseEditor which received the signal.
   * @gchild: the child to get display name string to show in @gladebaseeditor
   * treeview.
   *
   * Returns: a newly allocated string.
   */
  glade_base_editor_signals[SIGNAL_GET_DISPLAY_NAME] =
      g_signal_new ("get-display-name",
                    G_TYPE_FROM_CLASS (object_class),
                    G_SIGNAL_RUN_LAST,
                    G_STRUCT_OFFSET (GladeBaseEditorClass, get_display_name),
                    glade_string_accumulator, NULL,
                    glade_marshal_STRING__OBJECT,
                    G_TYPE_STRING, 1, G_TYPE_OBJECT);

  /**
   * GladeBaseEditor::build-child:
   * @gladebaseeditor: the #GladeBaseEditor which received the signal.
   * @gparent: the parent of the new child
   * @type: the #GType of the child
   *
   * Create a child widget here if something else must be done other than
   * calling glade_command_create() such as creating an intermediate parent.
   *
   * Returns: the newly created #GladeWidget or NULL if child cant be created
   */
  glade_base_editor_signals[SIGNAL_BUILD_CHILD] =
      g_signal_new ("build-child",
                    G_TYPE_FROM_CLASS (object_class),
                    G_SIGNAL_RUN_LAST,
                    G_STRUCT_OFFSET (GladeBaseEditorClass, build_child),
                    glade_stop_emission_accumulator, NULL,
                    glade_marshal_OBJECT__OBJECT_UINT,
                    G_TYPE_OBJECT, 2, G_TYPE_OBJECT, G_TYPE_UINT);

  /**
   * GladeBaseEditor::delete-child:
   * @gladebaseeditor: the #GladeBaseEditor which received the signal.
   * @gparent: the parent
   * @gchild: the child to delete
   */
  glade_base_editor_signals[SIGNAL_DELETE_CHILD] =
      g_signal_new ("delete-child",
                    G_TYPE_FROM_CLASS (object_class),
                    G_SIGNAL_RUN_LAST,
                    G_STRUCT_OFFSET (GladeBaseEditorClass, delete_child),
                    glade_boolean_handled_accumulator, NULL,
                    glade_marshal_BOOLEAN__OBJECT_OBJECT,
                    G_TYPE_BOOLEAN, 2, G_TYPE_OBJECT, G_TYPE_OBJECT);

  /**
   * GladeBaseEditor::move-child:
   * @gladebaseeditor: the #GladeBaseEditor which received the signal.
   * @gparent: the new parent of @gchild
   * @gchild: the #GladeWidget to move
   *
   * Move child here if something else must be done other than cut & paste.
   *
   * Returns: wheater child has been sucessfully moved or not.
   */
  glade_base_editor_signals[SIGNAL_MOVE_CHILD] =
      g_signal_new ("move-child",
                    G_TYPE_FROM_CLASS (object_class),
                    G_SIGNAL_RUN_LAST,
                    G_STRUCT_OFFSET (GladeBaseEditorClass, move_child),
                    glade_stop_emission_accumulator, NULL,
                    glade_marshal_BOOLEAN__OBJECT_OBJECT,
                    G_TYPE_BOOLEAN, 2, G_TYPE_OBJECT, G_TYPE_OBJECT);
}

static void
glade_base_editor_block_callbacks (GladeBaseEditor * editor, gboolean block)
{
  GladeBaseEditorPrivate *e = editor->priv;
  if (block)
    {
      g_signal_handlers_block_by_func (e->model, glade_base_editor_row_inserted,
                                       editor);
      if (e->project)
        {
          g_signal_handlers_block_by_func (e->project,
                                           glade_base_editor_project_remove_widget,
                                           editor);
          g_signal_handlers_block_by_func (e->project,
                                           glade_base_editor_project_add_widget,
                                           editor);
          g_signal_handlers_block_by_func (e->project,
                                           glade_base_editor_project_changed,
                                           editor);
        }
    }
  else
    {
      g_signal_handlers_unblock_by_func (e->model,
                                         glade_base_editor_row_inserted,
                                         editor);
      if (e->project)
        {
          g_signal_handlers_unblock_by_func (e->project,
                                             glade_base_editor_project_remove_widget,
                                             editor);
          g_signal_handlers_unblock_by_func (e->project,
                                             glade_base_editor_project_add_widget,
                                             editor);
          g_signal_handlers_unblock_by_func (e->project,
                                             glade_base_editor_project_changed,
                                             editor);
        }
    }
}

static void
glade_base_editor_realize_callback (GtkWidget * widget, gpointer user_data)
{
  GladeBaseEditor *editor = GLADE_BASE_EDITOR (widget);

  glade_base_editor_block_callbacks (editor, TRUE);

  glade_base_editor_fill_store (editor);
  gtk_tree_view_expand_all (GTK_TREE_VIEW (editor->priv->treeview));

  glade_base_editor_block_callbacks (editor, FALSE);
}


static void
glade_base_editor_switch_page (GtkNotebook * notebook,
                               GtkWidget * page,
                               guint page_num, GladeBaseEditor * editor)
{
  GladeBaseEditorPrivate *e = editor->priv;

  if (page_num == 0)
    glade_signal_editor_load_widget (e->signal_editor, e->gcontainer);
  else
    {
      GtkTreeIter iter;
      GladeWidget *gchild = NULL;

      if (glade_base_editor_get_child_selected (editor, &iter))
        {
          gtk_tree_model_get (e->model, &iter,
                              GLADE_BASE_EDITOR_GWIDGET, &gchild, -1);
          g_object_unref (G_OBJECT (gchild));
        }

      if (gchild)
        glade_signal_editor_load_widget (e->signal_editor, gchild);
      else
        glade_signal_editor_load_widget (e->signal_editor, NULL);
    }
}


static void
glade_base_editor_init (GladeBaseEditor * editor)
{
  GladeBaseEditorPrivate *e;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkWidget *paned, *hbox, *vbox, *tree_vbox, *scroll, *button_table, *button,
      *label;

  gtk_box_set_spacing (GTK_BOX (editor), 8);

  e = editor->priv = g_new0 (GladeBaseEditorPrivate, 1);

  e->group = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);

  /* Paned */
  e->paned = paned = gtk_vpaned_new ();
  gtk_widget_show (paned);
  gtk_box_pack_start (GTK_BOX (editor), e->paned, TRUE, TRUE, 0);

  /* Notebook */
  e->notebook = gtk_notebook_new ();
  gtk_widget_show (e->notebook);
  gtk_paned_pack1 (GTK_PANED (paned), e->notebook, TRUE, FALSE);

  /* ScrolledWindow */
  e->main_scroll = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_show (e->main_scroll);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (e->main_scroll),
                                       GTK_SHADOW_NONE);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (e->main_scroll),
                                  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  label = gtk_label_new (_("General"));
  gtk_widget_show (label);
  gtk_notebook_append_page (GTK_NOTEBOOK (e->notebook), e->main_scroll, label);

  /* Hbox */
  hbox = gtk_hbox_new (FALSE, 8);
  gtk_widget_show (hbox);

  label = gtk_label_new (_("Hierarchy"));
  gtk_widget_show (label);
  gtk_notebook_append_page (GTK_NOTEBOOK (e->notebook), hbox, label);

  /* TreeView Vbox */
  tree_vbox = gtk_vbox_new (FALSE, 8);
  gtk_widget_show (tree_vbox);
  gtk_box_pack_start (GTK_BOX (hbox), tree_vbox, FALSE, TRUE, 0);

  /* ScrolledWindow */
  scroll = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_show (scroll);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scroll),
                                       GTK_SHADOW_NONE);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroll),
                                  GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
  gtk_box_pack_start (GTK_BOX (tree_vbox), scroll, TRUE, TRUE, 0);

  /* TreeView */
  e->treeview = gtk_tree_view_new ();
  gtk_widget_show (e->treeview);
  gtk_tree_view_set_rules_hint (GTK_TREE_VIEW (e->treeview), TRUE);
  gtk_tree_view_set_reorderable (GTK_TREE_VIEW (e->treeview), TRUE);

  gtk_widget_add_events (e->treeview, GDK_KEY_PRESS_MASK);
  g_signal_connect (e->treeview, "key-press-event",
                    G_CALLBACK (glade_base_editor_treeview_key_press_event),
                    editor);

  g_signal_connect (e->treeview, "cursor-changed",
                    G_CALLBACK (glade_base_editor_treeview_cursor_changed),
                    editor);

  g_signal_connect (e->treeview, "button-press-event",
                    G_CALLBACK (glade_base_editor_popup_handler), editor);


  renderer = gtk_cell_renderer_text_new ();
  column = gtk_tree_view_column_new_with_attributes (_("Label"), renderer,
                                                     "text",
                                                     GLADE_BASE_EDITOR_NAME,
                                                     NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (e->treeview), column);

  renderer = gtk_cell_renderer_combo_new ();
  g_object_set (renderer,
                "has-entry", FALSE,
                "text-column", GLADE_BASE_EDITOR_CLASS_NAME,
                "editable", TRUE, NULL);

  g_signal_connect (G_OBJECT (renderer), "edited",
                    G_CALLBACK (glade_base_editor_child_type_edited), editor);

  column = gtk_tree_view_column_new_with_attributes (_("Type"), renderer,
                                                     "text",
                                                     GLADE_BASE_EDITOR_TYPE_NAME,
                                                     "model",
                                                     GLADE_BASE_EDITOR_CHILD_TYPES,
                                                     NULL);

  gtk_tree_view_append_column (GTK_TREE_VIEW (e->treeview), column);

  gtk_container_add (GTK_CONTAINER (scroll), e->treeview);

  /* Add/Remove buttons */
  button_table = gtk_table_new (1, 2, TRUE);
  gtk_widget_show (button_table);
  gtk_table_set_col_spacings (GTK_TABLE (button_table), 8);
  gtk_box_pack_start (GTK_BOX (tree_vbox), button_table, FALSE, TRUE, 0);

  button = gtk_button_new_from_stock (GTK_STOCK_ADD);
  gtk_widget_show (button);
  g_signal_connect (button, "clicked",
                    G_CALLBACK (glade_base_editor_add_activate), editor);
  gtk_table_attach_defaults (GTK_TABLE (button_table), button, 0, 1, 0, 1);

  e->remove_button = button = gtk_button_new_from_stock (GTK_STOCK_REMOVE);
  gtk_widget_show (button);
  g_signal_connect (button, "clicked",
                    G_CALLBACK (glade_base_editor_delete_activate), editor);
  gtk_table_attach_defaults (GTK_TABLE (button_table), button, 1, 2, 0, 1);

  /* Properties Vbox */
  vbox = gtk_vbox_new (FALSE, 8);
  gtk_widget_show (vbox);
  gtk_box_pack_start (GTK_BOX (hbox), vbox, TRUE, TRUE, 0);

  /* ScrolledWindow */
  scroll = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_show (scroll);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scroll),
                                       GTK_SHADOW_NONE);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroll),
                                  GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start (GTK_BOX (vbox), scroll, TRUE, TRUE, 0);

  /* Tables */
  e->table = gtk_table_new (1, 2, FALSE);
  gtk_widget_show (e->table);
  gtk_table_set_row_spacings (GTK_TABLE (e->table), 4);
  gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (scroll), e->table);

  /* Signal Editor */
  e->signal_editor = glade_signal_editor_new (NULL);
  gtk_widget_show (GTK_WIDGET (e->signal_editor));
  gtk_widget_set_size_request (GTK_WIDGET (e->signal_editor), -1, 96);
  gtk_paned_pack2 (GTK_PANED (paned), GTK_WIDGET (e->signal_editor), FALSE,
                   FALSE);

  /* Update the treeview on realize event */
  g_signal_connect (editor, "realize",
                    G_CALLBACK (glade_base_editor_realize_callback), NULL);

  g_signal_connect (G_OBJECT (e->notebook), "switch-page",
                    G_CALLBACK (glade_base_editor_switch_page), editor);

}

/********************************* Public API *********************************/
GType
glade_base_editor_get_type ()
{
  static GType type = 0;

  if (type == 0)
    {
      static const GTypeInfo our_info = {
        sizeof (GladeBaseEditorClass),
        NULL,
        NULL,
        (GClassInitFunc) glade_base_editor_class_init,
        NULL,
        NULL,
        sizeof (GladeBaseEditor),
        0,
        (GInstanceInitFunc) glade_base_editor_init,
      };

      type = g_type_register_static (GTK_TYPE_VBOX, "GladeBaseEditor",
                                     &our_info, 0);
    }

  return type;
}

/**
 * glade_base_editor_new:
 * @container: a container this new editor will edit.
 * @main_editable: the custom #GladeEditable for @container, or %NULL
 * @... A NULL terminated list of gchar *, GType
 * 
 * Creates a new GladeBaseEditor with @container toplevel 
 * support for all the object types indicated in the variable argument list.
 * Argument List:
 *   o The type name
 *   o The GType the editor will support
 *
 * Returns: a new GladeBaseEditor.
 */
GladeBaseEditor *
glade_base_editor_new (GObject * container, GladeEditable * main_editable, ...)
{
  ChildTypeTab *child_type;
  GladeWidget *gcontainer;
  GladeBaseEditor *editor;
  GladeBaseEditorPrivate *e;
  GtkTreeIter iter;
  GType iter_type;
  gchar *name;
  va_list args;

  gcontainer = glade_widget_get_from_gobject (container);
  g_return_val_if_fail (GLADE_IS_WIDGET (gcontainer), NULL);

  editor = GLADE_BASE_EDITOR (g_object_new (GLADE_TYPE_BASE_EDITOR, NULL));
  e = editor->priv;

  /* Store */
  e->model = (GtkTreeModel *) gtk_tree_store_new (GLADE_BASE_EDITOR_N_COLUMNS,
                                                  G_TYPE_OBJECT,
                                                  G_TYPE_OBJECT,
                                                  G_TYPE_STRING,
                                                  G_TYPE_STRING,
                                                  GTK_TYPE_TREE_MODEL);

  gtk_tree_view_set_model (GTK_TREE_VIEW (e->treeview), e->model);
  gtk_tree_view_expand_all (GTK_TREE_VIEW (e->treeview));

  g_signal_connect (e->model, "row-inserted",
                    G_CALLBACK (glade_base_editor_row_inserted), editor);

  /* Invent one if not provided */
  if (!main_editable)
    main_editable =
      glade_widget_adaptor_create_editable (glade_widget_get_adaptor (gcontainer),
					    GLADE_PAGE_GENERAL);

  glade_editable_load (main_editable, gcontainer);
  gtk_widget_show (GTK_WIDGET (main_editable));
  gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (e->main_scroll),
                                         GTK_WIDGET (main_editable));

  child_type = g_new0 (ChildTypeTab, 1);
  child_type->parent_type = G_OBJECT_TYPE (container);
  child_type->children =
      (GtkTreeModel *) gtk_list_store_new (GLADE_BASE_EDITOR_TYPES_N_COLUMNS,
                                           G_TYPE_GTYPE, G_TYPE_STRING);

  va_start (args, main_editable);
  while ((name = va_arg (args, gchar *)))
    {
      iter_type = va_arg (args, GType);

      gtk_list_store_append (GTK_LIST_STORE (child_type->children), &iter);
      gtk_list_store_set (GTK_LIST_STORE (child_type->children), &iter,
                          GLADE_BASE_EDITOR_GTYPE, iter_type,
                          GLADE_BASE_EDITOR_CLASS_NAME, name, -1);

      if (editor->priv->add_type == 0)
	editor->priv->add_type = iter_type;
    }
  va_end (args);

  e->child_types = g_list_prepend (e->child_types, child_type);

  glade_base_editor_set_container (editor, container);

  glade_signal_editor_load_widget (e->signal_editor, e->gcontainer);

  return editor;
}



/**
 * glade_base_editor_append_types:
 * @editor: A #GladeBaseEditor
 * @parent_type: the parent type these child types will apply to
 * @... A NULL terminated list of gchar *, GType
 * 
 * Appends support for all the object types indicated in the variable argument list.
 * Argument List:
 *   o The type name
 *   o The GType the editor will support for parents of type @type
 *
 */
void
glade_base_editor_append_types (GladeBaseEditor * editor, GType parent_type,
                                ...)
{
  ChildTypeTab *child_type;
  GtkTreeIter iter;
  gchar *name;
  va_list args;

  g_return_if_fail (GLADE_IS_BASE_EDITOR (editor));
  g_return_if_fail (get_children_model_for_type (editor, parent_type) == NULL);

  child_type = g_new0 (ChildTypeTab, 1);
  child_type->parent_type = parent_type;
  child_type->children =
      (GtkTreeModel *) gtk_list_store_new (GLADE_BASE_EDITOR_TYPES_N_COLUMNS,
                                           G_TYPE_GTYPE, G_TYPE_STRING);

  va_start (args, parent_type);
  while ((name = va_arg (args, gchar *)))
    {
      gtk_list_store_append (GTK_LIST_STORE (child_type->children), &iter);
      gtk_list_store_set (GTK_LIST_STORE (child_type->children), &iter,
                          GLADE_BASE_EDITOR_GTYPE, va_arg (args, GType),
                          GLADE_BASE_EDITOR_CLASS_NAME, name, 
			  -1);
    }
  va_end (args);

  editor->priv->child_types =
      g_list_insert_sorted (editor->priv->child_types, child_type,
                            (GCompareFunc) sort_type_by_hierarchy);
}

/**
 * glade_base_editor_add_default_properties:
 * @editor: a #GladeBaseEditor
 * @gchild: a #GladeWidget
 *
 * Add @gchild name and type property to @editor
 * 
 * NOTE: This function is intended to be used in "child-selected" callbacks
 */
void
glade_base_editor_add_default_properties (GladeBaseEditor * editor,
                                          GladeWidget * gchild)
{
  GtkTreeIter combo_iter;
  GtkWidget *label, *entry;
  GtkTreeModel *child_class;
  GtkCellRenderer *renderer;
  GObject *child;

  g_return_if_fail (GLADE_IS_BASE_EDITOR (editor));
  g_return_if_fail (GLADE_IS_WIDGET (gchild));
  g_return_if_fail (GLADE_IS_WIDGET (glade_widget_get_parent (gchild)));

  child = glade_widget_get_object (gchild);

  child_class =
      get_children_model_for_child_type (editor, G_OBJECT_TYPE (child));

  /* Name */
  label = gtk_label_new (_("Name:"));
  gtk_misc_set_alignment (GTK_MISC (label), 1.0, 0.0);

  entry = gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (entry), glade_widget_get_name (gchild));
  g_object_set_data (G_OBJECT (entry), "editor", editor);
  g_signal_connect (entry, "activate",
                    G_CALLBACK (glade_base_editor_name_activate), gchild);
  g_signal_connect (entry, "changed",
                    G_CALLBACK (glade_base_editor_name_activate), gchild);
  glade_base_editor_table_attach (editor, label, entry);

  if (child_class && gtk_tree_model_iter_n_children (child_class, NULL) > 1)
    {
      /* Type */
      label = gtk_label_new (_("Type:"));
      gtk_misc_set_alignment (GTK_MISC (label), 1.0, 0.0);

      entry = gtk_combo_box_new ();
      gtk_combo_box_set_model (GTK_COMBO_BOX (entry), child_class);

      renderer = gtk_cell_renderer_text_new ();
      gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (entry), renderer, FALSE);
      gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (entry), renderer, "text",
                                      GLADE_BASE_EDITOR_CLASS_NAME, NULL);

      if (glade_base_editor_get_type_info (editor, &combo_iter,
                                           G_OBJECT_TYPE (child), -1))
        gtk_combo_box_set_active_iter (GTK_COMBO_BOX (entry), &combo_iter);

      g_signal_connect (entry, "changed",
                        G_CALLBACK (glade_base_editor_type_changed), editor);
      glade_base_editor_table_attach (editor, label, entry);
    }
}

/**
 * glade_base_editor_add_properties:
 * @editor: a #GladeBaseEditor
 * @gchild: a #GladeWidget
 * @packing: whether we are adding packing properties or not
 * @...: A NULL terminated list of properties names.
 * 
 * Add @gchild properties to @editor
 *
 * NOTE: This function is intended to be used in "child-selected" callbacks
 */
void
glade_base_editor_add_properties (GladeBaseEditor * editor,
                                  GladeWidget * gchild, gboolean packing, ...)
{
  GladeEditorProperty *eprop;
  va_list args;
  gchar *property;

  g_return_if_fail (GLADE_IS_BASE_EDITOR (editor));
  g_return_if_fail (GLADE_IS_WIDGET (gchild));

  va_start (args, packing);
  property = va_arg (args, gchar *);

  while (property)
    {
      eprop =
          glade_widget_create_editor_property (gchild, property, packing, TRUE);
      if (eprop)
        glade_base_editor_table_attach (editor, eprop->item_label, GTK_WIDGET (eprop));
      property = va_arg (args, gchar *);
    }
  va_end (args);
}


/**
 * glade_base_editor_add_editable:
 * @editor: a #GladeBaseEditor
 * @gchild: the #GladeWidget
 * @page: the #GladeEditorPageType of the desired page for @gchild
 * 
 * Add @gchild editor of type @page to the base editor
 *
 * NOTE: This function is intended to be used in "child-selected" callbacks
 */
void
glade_base_editor_add_editable (GladeBaseEditor * editor,
                                GladeWidget * gchild, GladeEditorPageType page)
{
  GladeEditable *editable;
  gint row;

  g_return_if_fail (GLADE_IS_BASE_EDITOR (editor));
  g_return_if_fail (GLADE_IS_WIDGET (gchild));

  editable = glade_widget_adaptor_create_editable (glade_widget_get_adaptor (gchild), page);
  glade_editable_set_show_name (editable, FALSE);
  glade_editable_load (editable, gchild);
  gtk_widget_show (GTK_WIDGET (editable));

  row = editor->priv->row;

  gtk_table_attach (GTK_TABLE (editor->priv->table), GTK_WIDGET (editable), 0, 2, row, row + 1,
		    GTK_FILL, GTK_EXPAND | GTK_FILL, 2, 0);

  editor->priv->row++;

}



/**
 * glade_base_editor_add_label:
 * @editor: a #GladeBaseEditor
 * @str: the label string
 *
 * Adds a new label to @editor
 *
 * NOTE: This function is intended to be used in "child-selected" callbacks
 */
void
glade_base_editor_add_label (GladeBaseEditor * editor, gchar * str)
{
  GtkWidget *label;
  gchar *markup;
  gint row;

  g_return_if_fail (GLADE_IS_BASE_EDITOR (editor));
  g_return_if_fail (str != NULL);

  label = gtk_label_new (NULL);
  markup = g_strdup_printf ("<span rise=\"-20000\"><b>%s</b></span>", str);
  row = editor->priv->row;

  gtk_label_set_markup (GTK_LABEL (label), markup);
  gtk_misc_set_alignment (GTK_MISC (label), 0.0, 0.0);
  gtk_misc_set_padding (GTK_MISC (label), 0, 6);

  gtk_table_attach (GTK_TABLE (editor->priv->table), label, 0, 2, row, row + 1,
		    GTK_FILL, GTK_FILL, 2, 0);
  gtk_widget_show (label);
  editor->priv->row++;

  g_free (markup);
}

/**
 * glade_base_editor_set_show_signal_editor:
 * @editor: a #GladeBaseEditor
 * @val:
 *
 * Shows/hide @editor 's signal editor
 */
void
glade_base_editor_set_show_signal_editor (GladeBaseEditor * editor,
                                          gboolean val)
{
  g_return_if_fail (GLADE_IS_BASE_EDITOR (editor));

  if (val)
    gtk_widget_show (GTK_WIDGET (editor->priv->signal_editor));
  else
    gtk_widget_hide (GTK_WIDGET (editor->priv->signal_editor));
}

/* Convenience functions */

static void
glade_base_editor_help (GtkButton * button, gchar * markup)
{
  GtkWidget *dialog;

  dialog = gtk_message_dialog_new (GTK_WINDOW (glade_app_get_window ()),
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, " ");

  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog), markup);

  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}

/**
 * glade_base_editor_pack_new_window:
 * @editor: a #GladeBaseEditor
 * @title: the window title
 * @help_markup: the help text
 *
 * This convenience function create a new dialog window and packs @editor in it.
 *
 * Returns: the newly created window
 */
GtkWidget *
glade_base_editor_pack_new_window (GladeBaseEditor * editor,
                                   gchar * title, gchar * help_markup)
{
  GtkWidget *window, *buttonbox, *button;
  gchar *real_title;

  g_return_val_if_fail (GLADE_IS_BASE_EDITOR (editor), NULL);

  /* Window */
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_type_hint (GTK_WINDOW (window), GDK_WINDOW_TYPE_HINT_DIALOG);

  if (title)
    {
      real_title = g_strdup_printf ("%s - %s", title,
                                    glade_widget_get_name (editor->priv->
                                                           gcontainer));
      gtk_window_set_title (GTK_WINDOW (window), real_title);
      g_free (real_title);
    }

  g_signal_connect_swapped (G_OBJECT (editor), "notify::container",
                            G_CALLBACK (gtk_widget_destroy), window);

  /* Button Box */
  buttonbox = gtk_hbutton_box_new ();
  gtk_widget_show (buttonbox);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (buttonbox), GTK_BUTTONBOX_END);
  gtk_box_set_spacing (GTK_BOX (buttonbox), 8);
  gtk_box_pack_start (GTK_BOX (editor), buttonbox, FALSE, TRUE, 0);

  button = gtk_button_new_from_stock (GTK_STOCK_HELP);
  gtk_widget_show (button);
  g_signal_connect (button, "clicked",
                    G_CALLBACK (glade_base_editor_help),
                    help_markup ? help_markup :
                    _("<big><b>Tips:</b></big>\n"
                      "  * Right-click over the treeview to add items.\n"
                      "  * Press Delete to remove the selected item.\n"
                      "  * Drag &amp; Drop to reorder.\n"
                      "  * Type column is editable."));
  gtk_container_add (GTK_CONTAINER (buttonbox), button);
  gtk_button_box_set_child_secondary (GTK_BUTTON_BOX (buttonbox), button, TRUE);

  if (glade_app_get_accel_group ())
    {
      gtk_window_add_accel_group (GTK_WINDOW (window),
                                  glade_app_get_accel_group ());
      g_signal_connect (G_OBJECT (window), "key-press-event",
                        G_CALLBACK (glade_utils_hijack_key_press), NULL);
    }

  gtk_widget_show_all (GTK_WIDGET (editor));

  gtk_container_set_border_width (GTK_CONTAINER (editor), 6);
  gtk_container_add (GTK_CONTAINER (window), GTK_WIDGET (editor));

  gtk_window_set_default_size (GTK_WINDOW (window), 640, 480);

  return window;
}
