/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2001 Ximian, Inc.
 * Copyright (C) 2008 Tristan Van Berkom
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
 *   Chema Celorio <chema@celorio.com>
 *   Tristan Van Berkom <tvb@gnome.org>
 */

#include <config.h>

/**
 * SECTION:glade-project
 * @Short_Description: The Glade document hub and Load/Save interface.
 *
 * This object owns all project objects and is responsable for loading and
 * saving the glade document, you can monitor the project state via this
 * object and its signals.
 */

#include <string.h>
#include <stdlib.h>
#include <glib.h>
#include <glib/gi18n-lib.h>
#include <glib/gstdio.h>

#include "glade.h"
#include "glade-widget.h"
#include "glade-id-allocator.h"
#include "glade-app.h"
#include "glade-marshallers.h"
#include "glade-catalog.h"

#include "glade-project.h"
#include "glade-command.h"
#include "glade-name-context.h"
#include "glade-object-stub.h"

#include "glade-widget-private.h"
#include "glade-tsort.h"

#define VALID_ITER(project, iter) ((iter)!= NULL && G_IS_OBJECT ((iter)->user_data) && ((GladeProject*)(project))->priv->stamp == (iter)->stamp)

enum
{
	ADD_WIDGET,
	REMOVE_WIDGET,
	WIDGET_NAME_CHANGED,
	SELECTION_CHANGED,
	CLOSE,
	CHANGED,
 	PARSE_BEGAN,
 	PARSE_FINISHED,
	CONVERT_FINISHED,
	TARGETS_CHANGED,
	LOAD_PROGRESS,
	LAST_SIGNAL
};

enum
{
	PROP_0,
	PROP_MODIFIED,
	PROP_HAS_SELECTION,
	PROP_PATH,
	PROP_READ_ONLY,
	PROP_FORMAT
};

struct _GladeProjectPrivate
{
	gchar *path;            /* The full canonical path of the glade file for this project */

	gint   unsaved_number;  /* A unique number for this project if it is untitled */

	gboolean readonly;      /* A flag that is set if the project is readonly */

	gboolean loading;       /* A flags that is set when the project is loading */
	
	gboolean modified;    /* A flag that is set when a project has unsaved modifications
			       * if this flag is not set we don't have to query
			       * for confirmation after a close or exit is
			       * requested
			       */

	gint   stamp;     /* A a random int per instance of project used to stamp/check the
			   * GtkTreeIter->stamps */
	GList *tree;      /* List of toplevel Objects in this projects */
	GList *objects;   /* List of all objects in this project */

	GList *selection; /* We need to keep the selection in the project
			   * because we have multiple projects and when the
			   * user switchs between them, he will probably
			   * not want to loose the selection. This is a list
			   * of #GtkWidget items.
			   */

	GladeNameContext *toplevel_names; /* Context for uniqueness of names at the toplevel */
	GList            *toplevels;      /* List of toplevels with thier own naming contexts */


	gboolean     has_selection;           /* Whether the project has a selection */

	GList       *undo_stack;              /* A stack with the last executed commands */
	GList       *prev_redo_item;          /* Points to the item previous to the redo items */
	
	gboolean first_modification_is_na; /* the flag indicates that  the first_modification item has been lost */
	
	GList *first_modification; /* we record the first modification, so that we
	                                   * can set "modification" to FALSE when we
	                                   * undo this modification
	                                   */
	
	GtkAccelGroup *accel_group;
	
	gchar *comment;        /* XML comment, Glade will preserve whatever comment was
			        * in file, so users can delete or change it.
			        */
			 
	time_t  mtime;         /* last UTC modification time of file, or 0 if it could not be read */

	GladeProjectFormat format; /* file format */

	GHashTable *target_versions_major; /* target versions by catalog */
	GHashTable *target_versions_minor; /* target versions by catalog */

	GladeNamingPolicy naming_policy;	/* What rules apply to widget names */

	gchar *resource_path; /* Indicates where to load resources from for this project 
			       * (full or relative path, null means project directory).
			       */
	
	GList *unknown_catalogs; /* List of CatalogInfo catalogs */

	/* Control on the preferences dialog to update buttons etc when properties change */
	GtkWidget *prefs_dialog;
	GtkWidget *glade_radio;
	GtkWidget *builder_radio;
	GtkWidget *project_wide_radio;
	GtkWidget *toplevel_contextual_radio;
	GHashTable *target_radios;

	GtkWidget *resource_default_radio;
	GtkWidget *resource_relative_radio;
	GtkWidget *resource_fullpath_radio;
	GtkWidget *relative_path_entry;
	GtkWidget *full_path_button;

	/* For the loading progress bars ("load-progress" signal) */
	gint       progress_step;
	gint       progress_full;
	gboolean   load_cancel;
};

typedef struct 
{
  gchar *catalog;
  gint position;
} CatalogInfo;

typedef struct {
	GladeWidget      *toplevel;
	GladeNameContext *names;
} TopLevelInfo;

typedef struct {
	gchar *stock;
	gchar *filename;
} StockFilePair;

typedef struct {
	GIOChannel *channel;
	guint watch;
} ChannelWatchPair;


static void         glade_project_set_target_version       (GladeProject *project,
							    const gchar  *catalog,
							    guint16       major,
							    guint16       minor);

static void         glade_project_target_version_for_adaptor (GladeProject        *project, 
							      GladeWidgetAdaptor  *adaptor,
							      gint                *major,
							      gint                *minor);

static void         glade_project_set_readonly             (GladeProject       *project, 
							    gboolean            readonly);


static gboolean     glade_project_verify                   (GladeProject       *project, 
							    gboolean            saving);

static void         glade_project_verify_adaptor           (GladeProject       *project,
							    GladeWidgetAdaptor *adaptor,
							    const gchar        *path_name,
							    GString            *string,
							    gboolean            saving,
							    gboolean            forwidget,
							    GladeSupportMask   *mask);

static GladeXmlContext *glade_project_write                (GladeProject       *project);

static GladeWidget *search_ancestry_by_name                 (GladeWidget       *toplevel, 
							     const gchar       *name);

static GtkWidget   *glade_project_build_prefs_dialog        (GladeProject      *project);

static void         format_libglade_button_toggled          (GtkWidget         *widget,
							     GladeProject      *project);
static void         format_builder_button_toggled           (GtkWidget         *widget,
							     GladeProject      *project);
static void         policy_project_wide_button_clicked      (GtkWidget         *widget,
							     GladeProject      *project);
static void         policy_toplevel_contextual_button_clicked (GtkWidget       *widget,
							       GladeProject    *project);
static void         target_button_clicked                   (GtkWidget         *widget,
							     GladeProject      *project);
static void         update_prefs_for_resource_path          (GladeProject      *project);

static void	    gtk_tree_model_iface_init (GtkTreeModelIface* iface);

static void         glade_project_model_get_iter_for_object (GladeProject* project,
                                                             GObject* object,
                                                             GtkTreeIter* iter);

static gint         glade_project_count_children (GladeProject *project, 
						  GladeWidget  *parent);


static guint              glade_project_signals[LAST_SIGNAL] = {0};

static GladeIDAllocator  *unsaved_number_allocator = NULL;



#define GLADE_PROJECT_LARGE_PROJECT 40

G_DEFINE_TYPE_WITH_CODE (GladeProject, glade_project, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (GTK_TYPE_TREE_MODEL,
                                      gtk_tree_model_iface_init))

/*******************************************************************
                            GObjectClass
 *******************************************************************/

static GladeIDAllocator *
get_unsaved_number_allocator (void)
{
	if (unsaved_number_allocator == NULL)
		unsaved_number_allocator = glade_id_allocator_new ();
		
	return unsaved_number_allocator;
}

static void
glade_project_list_unref (GList *original_list)
{
	GList *l;
	for (l = original_list; l; l = l->next)
		g_object_unref (G_OBJECT (l->data));

	if (original_list != NULL)
		g_list_free (original_list);
}

static void
unparent_objects_recurse (GladeWidget *widget)
{
	GladeWidget *child;
	GList       *children, *list;

	/* Unparent all children */
	if ((children = 
	     glade_widget_get_children (widget)) != NULL)
	{
		for (list = children; list; list = list->next)
		{
			child = glade_widget_get_from_gobject (list->data);

			unparent_objects_recurse (child);

			if (child->internal == NULL)
				glade_widget_remove_child (widget, child);
		}
		g_list_free (children);
	}
}

static void
glade_project_dispose (GObject *object)
{
	GladeProject  *project = GLADE_PROJECT (object);
	GList         *list, *tree;
	
	/* Emit close signal */
	g_signal_emit (object, glade_project_signals [CLOSE], 0);
	
	glade_project_selection_clear (project, TRUE);

	glade_project_list_unref (project->priv->undo_stack);
	project->priv->undo_stack = NULL;

	/* Remove objects from the project */
	tree = g_list_copy (project->priv->tree);
	for (list = tree; list; list = list->next)
	{
		GladeWidget *gwidget = glade_widget_get_from_gobject (list->data);

		unparent_objects_recurse (gwidget);
	}
	g_list_free (tree);

	while (project->priv->tree)
		glade_project_remove_object (project, project->priv->tree->data);

	while (project->priv->objects)
		glade_project_remove_object (project, project->priv->objects->data);

	g_assert (project->priv->tree == NULL);
	g_assert (project->priv->objects == NULL);

	G_OBJECT_CLASS (glade_project_parent_class)->dispose (object);
}

static void
glade_project_finalize (GObject *object)
{
	GladeProject *project = GLADE_PROJECT (object);
	GList        *list;
	TopLevelInfo *tinfo;

	/* XXX FIXME: Destroy dialog related sizegroups here... */
	gtk_widget_destroy (project->priv->prefs_dialog);

	g_free (project->priv->path);
	g_free (project->priv->comment);

	if (project->priv->unsaved_number > 0)
		glade_id_allocator_release (get_unsaved_number_allocator (), project->priv->unsaved_number);

	g_hash_table_destroy (project->priv->target_versions_major);
	g_hash_table_destroy (project->priv->target_versions_minor);
	g_hash_table_destroy (project->priv->target_radios);

	glade_name_context_destroy (project->priv->toplevel_names);

	for (list = project->priv->toplevels; list; list = list->next)
	{
		tinfo = list->data;
		glade_name_context_destroy (tinfo->names);
		g_free (tinfo);
	}
	g_list_free (project->priv->toplevels);

	if (project->priv->unknown_catalogs)
	{
		GList *l;

		for (l = project->priv->unknown_catalogs; l; l = g_list_next (l))
		{
			CatalogInfo *data = l->data;
			g_free (data->catalog);
			g_free (data);
		}

		g_list_free (project->priv->unknown_catalogs);
		project->priv->unknown_catalogs = NULL;
	}

	G_OBJECT_CLASS (glade_project_parent_class)->finalize (object);
}

static void
glade_project_get_property (GObject    *object,
			    guint       prop_id,
			    GValue     *value,
			    GParamSpec *pspec)
{
	GladeProject *project = GLADE_PROJECT (object);

	switch (prop_id)
	{
		case PROP_MODIFIED:
			g_value_set_boolean (value, project->priv->modified);
			break;
		case PROP_HAS_SELECTION:
			g_value_set_boolean (value, project->priv->has_selection);
			break;
		case PROP_PATH:
			g_value_set_string (value, project->priv->path);
			break;				
		case PROP_READ_ONLY:
			g_value_set_boolean (value, project->priv->readonly);
			break;
		case PROP_FORMAT:
			g_value_set_int (value, project->priv->format);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;			
	}
}

/**
 * glade_project_set_modified:
 * @project: a #GladeProject
 * @modified: Whether the project should be set as modified or not
 * @modification: The first #GladeCommand which caused the project to have unsaved changes
 *
 * Set's whether a #GladeProject should be flagged as modified or not. This is useful
 * for indicating that a project has unsaved changes. If @modified is #TRUE, then
 * @modification will be recorded as the first change which caused the project to 
 * have unsaved changes. @modified is #FALSE then @modification will be ignored.
 *
 * If @project is already flagged as modified, then calling this method with
 * @modified as #TRUE, will have no effect. Likewise, if @project is unmodified
 * then calling this method with @modified as #FALSE, will have no effect.
 *
 */
static void
glade_project_set_modified (GladeProject *project,
			    gboolean      modified)
{
	GladeProjectPrivate *priv = project->priv;

	if (priv->modified != modified)
	{
		priv->modified = !priv->modified;
		
		if (!priv->modified)
		{
			priv->first_modification = project->priv->prev_redo_item;
			priv->first_modification_is_na = FALSE;
		}
		
		g_object_notify (G_OBJECT (project), "modified");
	}
}

/*******************************************************************
                          GladeProjectClass
 *******************************************************************/
static void
glade_project_walk_back (GladeProject *project)
{
	if (project->priv->prev_redo_item)
		project->priv->prev_redo_item = project->priv->prev_redo_item->prev;
}

static void
glade_project_walk_forward (GladeProject *project)
{
	if (project->priv->prev_redo_item)
		project->priv->prev_redo_item = project->priv->prev_redo_item->next;
	else
		project->priv->prev_redo_item = project->priv->undo_stack;
}

static void
glade_project_undo_impl (GladeProject *project)
{
	GladeCommand *cmd, *next_cmd;

	while ((cmd = glade_project_next_undo_item (project)) != NULL)
	{	
		glade_command_undo (cmd);

		glade_project_walk_back (project);

		g_signal_emit (G_OBJECT (project),
			       glade_project_signals [CHANGED], 
			       0, cmd, FALSE);

		if ((next_cmd = glade_project_next_undo_item (project)) != NULL &&
		    (next_cmd->group_id == 0 || next_cmd->group_id != cmd->group_id))
			break;
	}

	glade_editor_refresh (glade_app_get_editor ());
}

static void
glade_project_redo_impl (GladeProject *project)
{
	GladeCommand *cmd, *next_cmd;
	
	while ((cmd = glade_project_next_redo_item (project)) != NULL)
	{
		glade_command_execute (cmd);

		glade_project_walk_forward (project);

		g_signal_emit (G_OBJECT (project),
			       glade_project_signals [CHANGED],
			       0, cmd, TRUE);

		if ((next_cmd = glade_project_next_redo_item (project)) != NULL &&
		    (next_cmd->group_id == 0 || next_cmd->group_id != cmd->group_id))
			break;
	}

	glade_editor_refresh (glade_app_get_editor ());
}

static GladeCommand *
glade_project_next_undo_item_impl (GladeProject *project)
{
	GList *l;

	if ((l = project->priv->prev_redo_item) == NULL)
		return NULL;

	return GLADE_COMMAND (l->data);
}

static GladeCommand *
glade_project_next_redo_item_impl (GladeProject *project)
{
	GList *l;

	if ((l = project->priv->prev_redo_item) == NULL)
		return project->priv->undo_stack ? 
			GLADE_COMMAND (project->priv->undo_stack->data) : NULL;
	else
		return l->next ? GLADE_COMMAND (l->next->data) : NULL;
}

static GList *
glade_project_free_undo_item (GladeProject *project, GList *item)
{
	g_assert (item->data);

	if (item == project->priv->first_modification)
		project->priv->first_modification_is_na = TRUE;

	g_object_unref (G_OBJECT (item->data));

	return g_list_next (item);
}

static void
glade_project_push_undo_impl (GladeProject *project, GladeCommand *cmd)
{
	GladeProjectPrivate *priv = project->priv;
	GList *tmp_redo_item;

	/* We should now free all the "redo" items */
	tmp_redo_item = g_list_next (priv->prev_redo_item);
	while (tmp_redo_item)
		tmp_redo_item = glade_project_free_undo_item (project, tmp_redo_item);

	if (priv->prev_redo_item)
	{
		g_list_free (g_list_next (priv->prev_redo_item));
		priv->prev_redo_item->next = NULL;
	}
	else
	{
		g_list_free (priv->undo_stack);
		priv->undo_stack = NULL;
	}

	/* Try to unify only if group depth is 0 and the project has not been recently saved*/
	if (glade_command_get_group_depth() == 0 &&
	    priv->prev_redo_item != NULL &&
	    project->priv->prev_redo_item != project->priv->first_modification)
	{
		GladeCommand *cmd1 = priv->prev_redo_item->data;

		
		if (glade_command_unifies (cmd1, cmd))
		{
			glade_command_collapse (cmd1, cmd);
			g_object_unref (cmd);

			if (glade_command_unifies (cmd1, NULL))
			{
				tmp_redo_item = priv->prev_redo_item;
				glade_project_walk_back (project);
				glade_project_free_undo_item (project, tmp_redo_item);
				priv->undo_stack = g_list_delete_link (priv->undo_stack, tmp_redo_item);
			}

			g_signal_emit (G_OBJECT (project),
				       glade_project_signals [CHANGED],
				       0, NULL, TRUE);
			return;
		}
	}

	/* and then push the new undo item */
	priv->undo_stack = g_list_append (priv->undo_stack, cmd);

	if (project->priv->prev_redo_item == NULL)
		priv->prev_redo_item = priv->undo_stack;
	else
		priv->prev_redo_item = g_list_next (priv->prev_redo_item);


	g_signal_emit (G_OBJECT (project),
		       glade_project_signals [CHANGED],
		       0, cmd, TRUE);
}

static void
glade_project_changed_impl (GladeProject *project, 
			    GladeCommand *command,
			    gboolean      forward)
{
	if (!project->priv->loading)
	{
		/* if this command is the first modification to cause the project
		 * to have unsaved changes, then we can now flag the project as unmodified
		 */
		if (!project->priv->first_modification_is_na && project->priv->prev_redo_item == project->priv->first_modification)
			glade_project_set_modified (project, FALSE);
		else
			glade_project_set_modified (project, TRUE);
	}
	glade_app_update_ui ();
}


/*******************************************************************
                          Class Initializers
 *******************************************************************/
static void
glade_project_init (GladeProject *project)
{
	GladeProjectPrivate *priv;
	GList *list;

	project->priv = priv = 
		G_TYPE_INSTANCE_GET_PRIVATE ((project), GLADE_TYPE_PROJECT, GladeProjectPrivate);

	priv->path = NULL;
	priv->readonly = FALSE;
	priv->tree = NULL;
	priv->toplevels = NULL;
	priv->selection = NULL;
	priv->has_selection = FALSE;
	priv->undo_stack = NULL;
	priv->prev_redo_item = NULL;
	priv->first_modification = NULL;
	priv->first_modification_is_na = FALSE;
	priv->unknown_catalogs = NULL;

	priv->toplevel_names = glade_name_context_new ();
	priv->naming_policy = GLADE_POLICY_PROJECT_WIDE;

	priv->accel_group = NULL;

	priv->unsaved_number = glade_id_allocator_allocate (get_unsaved_number_allocator ());

	do { /* Get a random non-zero TreeIter stamper */
		priv->stamp = g_random_int ();
	} while (priv->stamp == 0);

	priv->format = GLADE_PROJECT_FORMAT_GTKBUILDER;

	priv->target_versions_major = g_hash_table_new_full (g_str_hash,
							     g_str_equal,
							     g_free,
							     NULL);
	priv->target_versions_minor = g_hash_table_new_full (g_str_hash,
							     g_str_equal,
							     g_free,
							     NULL);

	for (list = glade_app_get_catalogs(); list; list = list->next)
	{
		GladeCatalog *catalog = list->data;

		/* Set default target to catalog version */
		glade_project_set_target_version (project,
						  glade_catalog_get_name (catalog),
						  glade_catalog_get_major_version (catalog),
						  glade_catalog_get_minor_version (catalog));
	}

	priv->target_radios = g_hash_table_new_full (g_str_hash, g_str_equal,
						     g_free, NULL);
	priv->prefs_dialog = glade_project_build_prefs_dialog (project);

}

static void
glade_project_class_init (GladeProjectClass *klass)
{
	GObjectClass *object_class;

	object_class = G_OBJECT_CLASS (klass);

	object_class->get_property = glade_project_get_property;
	object_class->finalize     = glade_project_finalize;
	object_class->dispose      = glade_project_dispose;
	
	klass->add_object          = NULL;
	klass->remove_object       = NULL;
	klass->undo                = glade_project_undo_impl;
	klass->redo                = glade_project_redo_impl;
	klass->next_undo_item      = glade_project_next_undo_item_impl;
	klass->next_redo_item      = glade_project_next_redo_item_impl;
	klass->push_undo           = glade_project_push_undo_impl;

	klass->widget_name_changed = NULL;
	klass->selection_changed   = NULL;
	klass->close               = NULL;
	klass->changed             = glade_project_changed_impl;

	/**
	 * GladeProject::add-widget:
	 * @gladeproject: the #GladeProject which received the signal.
	 * @arg1: the #GladeWidget that was added to @gladeproject.
	 *
	 * Emitted when a widget is added to a project.
	 */
	glade_project_signals[ADD_WIDGET] =
		g_signal_new ("add_widget",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GladeProjectClass, add_object),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__OBJECT,
			      G_TYPE_NONE,
			      1,
			      GLADE_TYPE_WIDGET);

	/**
	 * GladeProject::remove-widget:
	 * @gladeproject: the #GladeProject which received the signal.
	 * @arg1: the #GladeWidget that was removed from @gladeproject.
	 * 
	 * Emitted when a widget is removed from a project.
	 */
	glade_project_signals[REMOVE_WIDGET] =
		g_signal_new ("remove_widget",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GladeProjectClass, remove_object),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__OBJECT,
			      G_TYPE_NONE,
			      1,
			      GLADE_TYPE_WIDGET);


	/**
	 * GladeProject::widget-name-changed:
	 * @gladeproject: the #GladeProject which received the signal.
	 * @arg1: the #GladeWidget who's name changed.
	 *
	 * Emitted when @gwidget's name changes.
	 */
	glade_project_signals[WIDGET_NAME_CHANGED] =
		g_signal_new ("widget_name_changed",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GladeProjectClass, widget_name_changed),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__OBJECT,
			      G_TYPE_NONE,
			      1,
			      GLADE_TYPE_WIDGET);


	/** 
	 * GladeProject::selection-changed:
	 * @gladeproject: the #GladeProject which received the signal.
	 *
	 * Emitted when @gladeproject selection list changes.
	 */
	glade_project_signals[SELECTION_CHANGED] =
		g_signal_new ("selection_changed",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GladeProjectClass, selection_changed),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);


	/**
	 * GladeProject::close:
	 * @gladeproject: the #GladeProject which received the signal.
	 *
	 * Emitted when a project is closing (a good time to clean up
	 * any associated resources).
	 */
	glade_project_signals[CLOSE] =
		g_signal_new ("close",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GladeProjectClass, close),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	/**
	 * GladeProject::changed:
	 * @gladeproject: the #GladeProject which received the signal.
	 * @arg1: the #GladeCommand that was executed
	 * @arg2: whether the command was executed or undone.
	 *
	 * Emitted when a @gladeproject's state changes via a #GladeCommand.
	 */
	glade_project_signals[CHANGED] =
		g_signal_new ("changed",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GladeProjectClass, changed),
			      NULL, NULL,
			      glade_marshal_VOID__OBJECT_BOOLEAN,
			      G_TYPE_NONE,
			      2,
			      GLADE_TYPE_COMMAND, G_TYPE_BOOLEAN);

	/**
	 * GladeProject::parse-began:
	 * @gladeproject: the #GladeProject which received the signal.
	 *
	 * Emitted when @gladeproject parsing starts.
	 */
	glade_project_signals[PARSE_BEGAN] =
		g_signal_new ("parse-began",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_FIRST,
			      0, NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	/**
	 * GladeProject::parse-finished:
	 * @gladeproject: the #GladeProject which received the signal.
	 *
	 * Emitted when @gladeproject parsing has finished.
	 */
	glade_project_signals[PARSE_FINISHED] =
		g_signal_new ("parse-finished",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GladeProjectClass, parse_finished),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	/**
	 * GladeProject::convert-finished:
	 * @gladeproject: the #GladeProject which received the signal.
	 *
	 * Emitted when @gladeproject format conversion has finished.
	 *
	 * NOTE: Some properties are internally handled differently
	 * when the project is in a said format, this signal is fired after
	 * the new format is in effect to allow the backend access to both
	 * before and after.
	 */
	glade_project_signals[CONVERT_FINISHED] =
		g_signal_new ("convert-finished",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GladeProjectClass, parse_finished),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

        /**
	 * GladeProject::targets-changed:
         * @gladeproject: the #GladeProject which received the signal.
         *
         * Emitted when @gladeproject target versions change.
         */
	glade_project_signals[TARGETS_CHANGED] =
		g_signal_new ("targets-changed",
                              G_TYPE_FROM_CLASS (object_class),
                              G_SIGNAL_RUN_FIRST,
                              0, NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);
 
	/**
	 * GladeProject::load-progress:
	 * @gladeproject: the #GladeProject which received the signal.
	 * @objects_total: the total amount of objects to load
	 * @objects_loaded: the current amount of loaded objects
	 *
	 * Emitted while @project is loading.
	 */
	glade_project_signals[LOAD_PROGRESS] =
		g_signal_new ("load-progress",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_FIRST,
			      0, NULL, NULL,
			      glade_marshal_VOID__INT_INT,
			      G_TYPE_NONE, 2, G_TYPE_INT, G_TYPE_INT);

	g_object_class_install_property (object_class,
					 PROP_MODIFIED,
					 g_param_spec_boolean ("modified",
							       "Modified",
							       _("Whether project has been modified since it was last saved"),
							       FALSE,
							       G_PARAM_READABLE));

	g_object_class_install_property (object_class,
					 PROP_HAS_SELECTION,
					 g_param_spec_boolean ("has-selection",
							       _("Has Selection"),
							       _("Whether project has a selection"),
							       FALSE,
							       G_PARAM_READABLE));

	g_object_class_install_property (object_class,
					 PROP_PATH,
					 g_param_spec_string ("path",
							      _("Path"),
							      _("The filesystem path of the project"),
							      NULL,
							      G_PARAM_READABLE));

	g_object_class_install_property (object_class,
					 PROP_READ_ONLY,
					 g_param_spec_boolean ("read-only",
							       _("Read Only"),
							       _("Whether project is read-only"),
							       FALSE,
							       G_PARAM_READABLE));

	g_object_class_install_property (object_class,
					 PROP_FORMAT,
					 g_param_spec_int ("format",
							   _("Format"),
							   _("The project file format"),
							   GLADE_PROJECT_FORMAT_LIBGLADE,
							   GLADE_PROJECT_FORMAT_GTKBUILDER,
							   GLADE_PROJECT_FORMAT_GTKBUILDER,
							   G_PARAM_READABLE));


	g_type_class_add_private (klass, sizeof (GladeProjectPrivate));
}

/*******************************************************************
                    Loading project code here
 *******************************************************************/

/**
 * glade_project_new:
 *
 * Creates a new #GladeProject.
 *
 * Returns: a new #GladeProject
 */
GladeProject *
glade_project_new (void)
{
	GladeProject *project = g_object_new (GLADE_TYPE_PROJECT, NULL);
	return project;
}

/* Called when finishing loading a glade file to resolve object type properties
 */
static void 
glade_project_fix_object_props (GladeProject *project)
{
	GList         *l, *ll, *objects;
	GValue        *value;
	GladeWidget   *gwidget;
	GladeProperty *property;
	gchar         *txt;

	objects = g_list_copy (project->priv->objects);
	for (l = objects; l; l = l->next)
	{
		gwidget = glade_widget_get_from_gobject (l->data);

		for (ll = gwidget->properties; ll; ll = ll->next)
		{
			property = GLADE_PROPERTY (ll->data);

			if (glade_property_class_is_object (property->klass, 
							    project->priv->format) &&
			    (txt = g_object_get_data (G_OBJECT (property), 
						      "glade-loaded-object")) != NULL)
			{
				/* Parse the object list and set the property to it
				 * (this magicly works for both objects & object lists)
				 */
				value = glade_property_class_make_gvalue_from_string
					(property->klass, txt, gwidget->project, gwidget);
				
				glade_property_set_value (property, value);
				
				g_value_unset (value);
				g_free (value);
				
				g_object_set_data (G_OBJECT (property), 
						   "glade-loaded-object", NULL);
			}
		}
	}
	g_list_free (objects);
}

static gchar *
glade_project_read_requires_from_comment (GladeXmlNode  *comment,
					  guint16       *major,
					  guint16       *minor)
{
	gint   maj, min;
	gchar *value, buffer[256];
	gchar *required_lib = NULL;

	if (!glade_xml_node_is_comment (comment)) 
		return NULL;

	value = glade_xml_get_content (comment);

	if (value && !strncmp (" interface-requires", value, strlen (" interface-requires")))
	{
		if (sscanf (value, " interface-requires %s %d.%d", buffer, &maj, &min) == 3)
		{
			if (major) *major = maj;
			if (minor) *minor = min;
			required_lib = g_strdup (buffer);
		}
	}
	g_free (value);

	return required_lib;
}
					  

static gboolean
glade_project_read_requires (GladeProject *project,
			     GladeXmlNode *root_node, 
			     const gchar  *path,
			     gboolean     *has_gtk_dep)
{

	GString      *string = g_string_new (NULL);
	GladeXmlNode *node;
	gchar        *required_lib = NULL;
	gboolean      loadable = TRUE;
	guint16       major, minor;
	gint          position = 0;

	for (node = glade_xml_node_get_children_with_comments (root_node); 
	     node; node = glade_xml_node_next_with_comments (node))
	{
		/* Skip non "requires" tags */
		if (!(glade_xml_node_verify_silent (node, GLADE_XML_TAG_REQUIRES) ||	
		      (required_lib = 
		       glade_project_read_requires_from_comment (node, &major, &minor))))
			continue;

		if (!required_lib)
		{
			required_lib =  
				glade_xml_get_property_string_required (node, GLADE_XML_TAG_LIB, 
									NULL);
			glade_xml_get_property_version (node, GLADE_XML_TAG_VERSION, 
							&major, &minor);
		}

		if (!required_lib) continue;

		/* Dont mention gtk+ as a required lib in 
		 * the generated glade file
		 */
		if (!glade_catalog_is_loaded (required_lib))
		{
			CatalogInfo *data = g_new0(CatalogInfo, 1);

			data->catalog = required_lib;
			data->position = position;

			/* Keep a list of unknown catalogs to avoid loosing the requirement */
			project->priv->unknown_catalogs = g_list_append (project->priv->unknown_catalogs,
                                                           data);
			/* Also keep the version */
			glade_project_set_target_version (project, required_lib, major, minor);

			if (!loadable)
				g_string_append (string, ", ");

			g_string_append (string, required_lib);
			loadable = FALSE;
		}
		else
	       	{
			if (has_gtk_dep && strcmp (required_lib, "gtk+") == 0)
				*has_gtk_dep = TRUE;

			glade_project_set_target_version
				(project, required_lib, major, minor);
			g_free (required_lib);
		}

		position++;
	}

	if (!loadable)
		glade_util_ui_message (glade_app_get_window(),
				       GLADE_UI_ERROR, NULL,
				       _("Failed to load %s.\n"
					 "The following required catalogs are unavailable: %s"),
				       path, string->str);
	g_string_free (string, TRUE);
	return loadable;
}


static gboolean
glade_project_read_policy_from_comment (GladeXmlNode      *comment,
					GladeNamingPolicy *policy)
{
	gchar *value, buffer[256];
	gboolean loaded = FALSE;

	if (!glade_xml_node_is_comment (comment)) 
		return FALSE;

	value = glade_xml_get_content (comment);
	if (value && !strncmp (" interface-naming-policy", value, strlen (" interface-naming-policy")))
	{
		if (sscanf (value, " interface-naming-policy %s", buffer) == 1)
		{
			if (strcmp (buffer, "project-wide") == 0)
				*policy = GLADE_POLICY_PROJECT_WIDE;
			else
				*policy = GLADE_POLICY_TOPLEVEL_CONTEXTUAL;
			
			loaded = TRUE;
		}
	}
	g_free (value);

	return loaded;
}

static void
glade_project_read_naming_policy (GladeProject *project,
				  GladeXmlNode *root_node)
{
	/* A project file with no mention of a policy needs more lax rules 
	 * (a new project has a project wide policy by default)
	 */
	GladeNamingPolicy policy = GLADE_POLICY_TOPLEVEL_CONTEXTUAL;
	GladeXmlNode *node;
	
	for (node = glade_xml_node_get_children_with_comments (root_node); 
	     node; node = glade_xml_node_next_with_comments (node))
	{
		if (glade_project_read_policy_from_comment (node, &policy))
			break;
	}

	glade_project_set_naming_policy (project, policy);
}


static gchar *
glade_project_read_resource_path_from_comment (GladeXmlNode  *comment)
{
	gchar *value, buffer[FILENAME_MAX], *path = NULL;

	if (!glade_xml_node_is_comment (comment)) 
		return FALSE;

	value = glade_xml_get_content (comment);
	if (value && !strncmp (" interface-local-resource-path", value, strlen (" interface-local-resource-path")))
	{
		if (sscanf (value, " interface-local-resource-path %s", buffer) == 1)
			path = g_strdup (buffer);
	}
	g_free (value);

	return path;
}

static void
update_project_for_resource_path (GladeProject *project)
{
	GladeWidget   *widget;
	GladeProperty *property;
	GList         *l, *list;

	for (l = project->priv->objects; l; l = l->next)
	{
		
		widget = glade_widget_get_from_gobject (l->data);

		for (list = widget->properties; list; list = list->next)
		{
			property = list->data;

			/* XXX We should have a "resource" flag on properties that need
			 *   to be loaded from the resource path, but that would require
			 * that they can serialize both ways (custom properties are only
			 * required to generate unique strings for value comparisons).
			 */
			if (property->klass->pspec->value_type == GDK_TYPE_PIXBUF)
			{
				GValue *value;
				gchar *string;

				string = glade_property_class_make_string_from_gvalue
					(property->klass, property->value, project->priv->format);

				value = glade_property_class_make_gvalue_from_string 
					(property->klass, string, project, widget);

				glade_property_set_value (property, value);

				g_value_unset (value);
				g_free (value);
				g_free (string);
			}
		}
	}
}


/* This function assumes ownership of 'path'. */
static void
glade_project_set_resource_path (GladeProject *project, 
				 gchar        *path)
{
	g_free (project->priv->resource_path);
	project->priv->resource_path = path;

	update_project_for_resource_path (project);
	update_prefs_for_resource_path (project);
}

static void
glade_project_read_resource_path (GladeProject *project,
				  GladeXmlNode *root_node)
{
	GladeXmlNode *node;
	gchar *path = NULL;
	
	for (node = glade_xml_node_get_children_with_comments (root_node); 
	     node; node = glade_xml_node_next_with_comments (node))
	{
		/* Skip non "requires" tags */
		if ((path = glade_project_read_resource_path_from_comment (node)) != NULL)
			break;
	}

	glade_project_set_resource_path (project, path);
}


static void
glade_project_read_comment (GladeProject *project, GladeXmlDoc *doc)
{
	/* TODO Write me !! Find out how to extract root level comments 
	 * with libxml2 !!! 
	 */
}


typedef struct {
	GladeWidget *widget;
	gint         major;
	gint         minor;
} VersionData;

static void
glade_project_introspect_signal_versions (const gchar *signal_name,
					  GPtrArray   *signals,
					  VersionData *data)
{
	gint i;

	for (i = 0; i < signals->len; i++)
	{
		GladeSignalClass *signal_class;
		GladeSignal      *signal = g_ptr_array_index (signals, i);
		gchar            *catalog = NULL;
		gboolean          is_gtk_adaptor = FALSE;

		signal_class = glade_widget_adaptor_get_signal_class (data->widget->adaptor, signal->name);
		/*  unknown signal... can it happen ? */
		if (!signal_class)
			continue;
		g_assert (signal_class->adaptor);

		/* Check if the signal comes from a GTK+ widget class */
		g_object_get (signal_class->adaptor, "catalog", &catalog, NULL);
		if (strcmp (catalog, "gtk+") == 0)
			is_gtk_adaptor = TRUE;
		g_free (catalog);

		/* Check GTK+ version that signal was introduced */
		if (is_gtk_adaptor &&
		    (data->major < signal_class->version_since_major ||
		     (data->major == signal_class->version_since_major &&
		      data->minor < signal_class->version_since_minor)))
		{
			data->major = signal_class->version_since_major;
			data->minor = signal_class->version_since_minor;
		}
	}
}

static void
glade_project_introspect_gtk_version (GladeProject *project)
{
	GladeProjectFormat fmt;
	GladeWidget *widget;
	GList *list, *l;
	gint  target_major = 2, target_minor = 0;

	fmt = glade_project_get_format (project);

	if (fmt == GLADE_PROJECT_FORMAT_GTKBUILDER)
		target_minor = 12;
	else
		target_minor = 6;

	for (list = project->priv->objects; list; list = list->next)
       	{
		gboolean  is_gtk_adaptor = FALSE;
		gchar    *catalog = NULL;
		VersionData data = { 0, };

		widget = glade_widget_get_from_gobject (list->data);

		/* Check if its a GTK+ widget class */
		g_object_get (widget->adaptor, "catalog", &catalog, NULL);
		if (strcmp (catalog, "gtk+") == 0)
			is_gtk_adaptor = TRUE;
		g_free (catalog);

		/* Check widget class version */
		if (is_gtk_adaptor && !GWA_VERSION_CHECK (widget->adaptor, target_major, target_minor))
       		{
			target_major = GWA_VERSION_SINCE_MAJOR (widget->adaptor);
			target_minor = GWA_VERSION_SINCE_MINOR (widget->adaptor);
       		}

		/* Check all properties */
		for (l = widget->properties; l; l = l->next)
		{
			GladeProperty *property = l->data;
			GladeWidgetAdaptor *prop_adaptor, *adaptor;

			/* Unset properties ofcourse dont count... */
			if (glade_property_original_default (property))
				continue;

			/* Check if this property originates from a GTK+ widget class */
			prop_adaptor = glade_widget_adaptor_from_pclass (property->klass);
			adaptor = glade_widget_adaptor_from_pspec (prop_adaptor, property->klass->pspec);
			
			catalog = NULL;
			is_gtk_adaptor = FALSE;
			g_object_get (adaptor, "catalog", &catalog, NULL);
			if (strcmp (catalog, "gtk+") == 0)
				is_gtk_adaptor = TRUE;
			g_free (catalog);

			/* Check GTK+ property class versions */
			if (is_gtk_adaptor &&
			    !GPC_VERSION_CHECK (property->klass, target_major, target_minor))
			{
				target_major = property->klass->version_since_major;
				target_minor = property->klass->version_since_minor;
			}
		}

		/* Check all signal versions here */
		data.widget = widget;
		data.major  = target_major;
		data.minor  = target_minor;

		g_hash_table_foreach (widget->signals,
				      (GHFunc)glade_project_introspect_signal_versions,
				      &data);

		if (target_major < data.major)
			target_major = data.major;

		if (target_minor < data.minor)
			target_minor = data.minor;
       	}

	glade_project_set_target_version (project, "gtk+", target_major, target_minor);
}


static gint 
glade_project_count_xml_objects (GladeProject *project, GladeXmlNode *root, gint count)
{
	GladeXmlNode    *node;

	for (node = glade_xml_node_get_children (root); 
	     node; node = glade_xml_node_next (node))
	{
		if (glade_xml_node_verify_silent
		    (node, GLADE_XML_TAG_WIDGET (project->priv->format)))
			count = glade_project_count_xml_objects (project, node, ++count);
		else if (glade_xml_node_verify_silent (node, GLADE_XML_TAG_CHILD))
			count = glade_project_count_xml_objects (project, node, count);			
	}
	return count;
}

void
glade_project_cancel_load (GladeProject *project)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));

	project->priv->load_cancel = TRUE;
}

gboolean
glade_project_load_cancelled (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), FALSE);

	return project->priv->load_cancel;
}

void
glade_project_push_progress (GladeProject *project)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));

	project->priv->progress_step++;

	g_signal_emit (project, glade_project_signals [LOAD_PROGRESS], 0,
		       project->priv->progress_full,
		       project->priv->progress_step);
}

static gboolean
glade_project_load_internal (GladeProject *project)
{
	GladeXmlContext *context;
	GladeXmlDoc     *doc;
	GladeXmlNode    *root;
	GladeXmlNode    *node;
	GladeWidget     *widget;
	gboolean         has_gtk_dep = FALSE;
	gint             count;

	project->priv->selection = NULL;
	project->priv->toplevels = NULL;
	project->priv->objects = NULL;
	project->priv->loading = TRUE;

	/* get the context & root node of the catalog file */
	if (!(context = 
	      glade_xml_context_new_from_path (project->priv->path,
					       NULL, 
					       NULL)))
	{
		g_warning ("Couldn't open glade file [%s].", project->priv->path);
		project->priv->loading = FALSE;
		return FALSE;
	}

	project->priv->mtime = glade_util_get_file_mtime (project->priv->path, NULL);

	doc  = glade_xml_context_get_doc (context);
	root = glade_xml_doc_get_root (doc);

	if (glade_xml_node_verify_silent (root, GLADE_XML_TAG_LIBGLADE_PROJECT))
		glade_project_set_format (project, GLADE_PROJECT_FORMAT_LIBGLADE);
	else if (glade_xml_node_verify_silent (root, GLADE_XML_TAG_BUILDER_PROJECT))
		glade_project_set_format (project, GLADE_PROJECT_FORMAT_GTKBUILDER);
	else
	{
		g_warning ("Couldnt determine project format, skipping %s", 
			   project->priv->path);
		glade_xml_context_free (context);
		project->priv->loading = FALSE;
		return FALSE;
	}

	/* Emit "parse-began" signal */
	g_signal_emit (project, glade_project_signals [PARSE_BEGAN], 0);

	/* XXX Need to load project->priv->comment ! */
	glade_project_read_comment (project, doc);

	glade_project_read_requires (project, root, project->priv->path, &has_gtk_dep);

	glade_project_read_naming_policy (project, root);

	glade_project_read_resource_path (project, root);

	/* Launch a dialog if it's going to take enough time to be
	 * worth showing at all */
	count = glade_project_count_xml_objects (project, root, 0);
	project->priv->progress_full = count;
	project->priv->progress_step = 0;

	for (node = glade_xml_node_get_children (root); 
	     node; node = glade_xml_node_next (node))
	{
		/* Skip "requires" tags */
		if (!glade_xml_node_verify_silent
		    (node, GLADE_XML_TAG_WIDGET (project->priv->format)))
			continue;

		if ((widget = glade_widget_read (project, NULL, node, NULL)) != NULL)
			glade_project_add_object (project, NULL, widget->object);

		if (project->priv->load_cancel)
			break;
	}

	/* Finished with the xml context */
	glade_xml_context_free (context);

	if (project->priv->load_cancel)
	{
		project->priv->loading = FALSE;
		return FALSE;
	}

	if (!has_gtk_dep)
		glade_project_introspect_gtk_version (project);

	if (glade_util_file_is_writeable (project->priv->path) == FALSE)
		glade_project_set_readonly (project, TRUE);

	/* Reset project status here too so that you get a clean
	 * slate after calling glade_project_open().
	 */
	project->priv->modified = FALSE;
	project->priv->loading = FALSE;

	/* Now we have to loop over all the object properties
	 * and fix'em all ('cause they probably weren't found)
	 */
	glade_project_fix_object_props (project);

	/* Emit "parse-finished" signal */
	g_signal_emit (project, glade_project_signals [PARSE_FINISHED], 0);

	/* Update ui with versioning info
	 */
	glade_project_verify_project_for_ui (project);

	/* Update various things in the UI */
	glade_app_update_ui ();

	return TRUE;

}

gboolean
glade_project_load_from_file (GladeProject *project, 
			      const gchar  *path)
{
	gboolean retval;
	
	g_return_val_if_fail (path != NULL, FALSE);
	g_return_val_if_fail (GLADE_IS_PROJECT (project), FALSE);
	
	project->priv->path = glade_util_canonical_path (path);	
	
	retval = glade_project_load_internal (project);
	
	if (retval)
	{
		gchar *name, *title;

		/* Update prefs dialogs here... */
		name = glade_project_get_name (project);
		title = g_strdup_printf (_("%s preferences"), name);
		gtk_window_set_title (GTK_WINDOW (project->priv->prefs_dialog), title);
		g_free (title);
		g_free (name);
	}
	return retval;
}

/**
 * glade_project_load:
 * @path:
 * 
 * Opens a project at the given path.
 *
 * Returns: a new #GladeProject for the opened project on success, %NULL on 
 *          failure
 */
GladeProject *
glade_project_load (const gchar *path)
{
	GladeProject *project;
	gboolean      retval;
	
	g_return_val_if_fail (path != NULL, NULL);
	
	project = g_object_new (GLADE_TYPE_PROJECT, NULL);

	project->priv->path = glade_util_canonical_path (path);	
	
	retval = glade_project_load_internal (project);
	
	if (retval)
	{
		gchar *name, *title;

		/* Update prefs dialogs here... */
		name = glade_project_get_name (project);
		title = g_strdup_printf (_("%s preferences"), name);
		gtk_window_set_title (GTK_WINDOW (project->priv->prefs_dialog), title);
		g_free (title);
		g_free (name);

		return project;
	}
	else
	{
		g_object_unref (project);
		return NULL;
	}
}

/*******************************************************************
                    Writing project code here
 *******************************************************************/

#define GLADE_XML_COMMENT "Generated with "PACKAGE_NAME

static gchar *
glade_project_make_comment ()
{
	time_t now = time (NULL);
	gchar *comment;
	comment = g_strdup_printf (GLADE_XML_COMMENT" "PACKAGE_VERSION" on %s",
				   ctime (&now));
	glade_util_replace (comment, '\n', ' ');
	
	return comment;
}

static void
glade_project_update_comment (GladeProject *project)
{
	gchar **lines, **l, *comment = NULL;
	
	/* If project has no comment -> add the new one */
	if (project->priv->comment == NULL)
	{
		project->priv->comment = glade_project_make_comment ();
		return;
	}
	
	for (lines = l = g_strsplit (project->priv->comment, "\n", 0); *l; l++)
	{
		gchar *start;
		
		/* Ignore leading spaces */
		for (start = *l; *start && g_ascii_isspace (*start); start++);
		
		if (g_str_has_prefix (start, GLADE_XML_COMMENT))
		{
			/* This line was generated by glade -> updating... */
			g_free (*l);
			*l = comment = glade_project_make_comment ();
		}
	}
	
	if (comment)
	{
		g_free (project->priv->comment);
		project->priv->comment = g_strjoinv ("\n", lines);
	}
	
	g_strfreev (lines);
}

static void
glade_project_write_required_libs (GladeProject    *project,
				   GladeXmlContext *context,
				   GladeXmlNode    *root)
{
	GladeProjectFormat fmt;
	GladeXmlNode    *req_node;
	GList           *required, *list;
	gint             major, minor;
	gchar           *version;

	fmt = glade_project_get_format (project);

	if ((required = glade_project_required_libs (project)) != NULL)
	{
		for (list = required; list; list = list->next)
		{
			glade_project_get_target_version (project, (gchar *)list->data, 
							  &major, &minor);
			
			version = g_strdup_printf ("%d.%d", major, minor);

			/* Write the standard requires tag */
			if (fmt == GLADE_PROJECT_FORMAT_GTKBUILDER ||
			    (fmt == GLADE_PROJECT_FORMAT_LIBGLADE &&
			     strcmp ("gtk+", (gchar *)list->data)))
			{
				if (GLADE_GTKBUILDER_HAS_VERSIONING (major, minor))
				{
					req_node = glade_xml_node_new (context, GLADE_XML_TAG_REQUIRES);
					glade_xml_node_append_child (root, req_node);
					glade_xml_node_set_property_string (req_node, 
									    GLADE_XML_TAG_LIB, 
									    (gchar *)list->data);
				}
				else
				{
					gchar *comment = 
						g_strdup_printf (" interface-requires %s %s ", 
								 (gchar *)list->data, version);
					req_node = glade_xml_node_new_comment (context, comment);
					glade_xml_node_append_child (root, req_node);
					g_free (comment);
				}

				if (fmt != GLADE_PROJECT_FORMAT_LIBGLADE)
					glade_xml_node_set_property_string 
						(req_node, GLADE_XML_TAG_VERSION, version);
			}

			/* Add extra metadata for libglade */
			if (fmt == GLADE_PROJECT_FORMAT_LIBGLADE)
			{
				gchar *comment = g_strdup_printf (" interface-requires %s %s ", 
								  (gchar *)list->data, version);
				req_node = glade_xml_node_new_comment (context, comment);
				glade_xml_node_append_child (root, req_node);
				g_free (comment);
			}
			g_free (version);

		}
		g_list_foreach (required, (GFunc)g_free, NULL);
		g_list_free (required);
	}

}

static void
glade_project_write_naming_policy (GladeProject    *project,
				   GladeXmlContext *context,
				   GladeXmlNode    *root)
{
	GladeXmlNode *policy_node;
	gchar *comment = g_strdup_printf (" interface-naming-policy %s ", 
					  project->priv->naming_policy == GLADE_POLICY_PROJECT_WIDE ? 
					  "project-wide" : "toplevel-contextual");

	policy_node = glade_xml_node_new_comment (context, comment);
	glade_xml_node_append_child (root, policy_node);
	g_free (comment);
}

static void
glade_project_write_resource_path (GladeProject    *project,
				   GladeXmlContext *context,
				   GladeXmlNode    *root)
{
	GladeXmlNode *path_node;
	if (project->priv->resource_path)
	{
		gchar *comment = g_strdup_printf (" interface-local-resource-path %s ", 
						  project->priv->resource_path);
		path_node = glade_xml_node_new_comment (context, comment);
		glade_xml_node_append_child (root, path_node);
		g_free (comment);
	}
}

/**
 * glade_project_save:
 * @project: a #GladeProject
 * @path: location to save glade file
 * @error: an error from the G_FILE_ERROR domain.
 * 
 * Saves @project to the given path. 
 *
 * Returns: %TRUE on success, %FALSE on failure
 */
gboolean
glade_project_save (GladeProject *project, const gchar *path, GError **error)
{
	GladeXmlContext *context;
	GladeXmlDoc     *doc;
	gchar           *canonical_path;
	gint             ret;

	g_return_val_if_fail (GLADE_IS_PROJECT (project), FALSE);

	if (glade_project_is_loading (project))
		return FALSE;

	if (!glade_project_verify (project, TRUE))
		return FALSE;

	context = glade_project_write (project);
	doc     = glade_xml_context_get_doc (context);
	ret     = glade_xml_doc_save (doc, path);
	glade_xml_context_destroy (context);

	canonical_path = glade_util_canonical_path (path);
	g_assert (canonical_path);

	if (project->priv->path == NULL ||
	    strcmp (canonical_path, project->priv->path))
        {
		gchar *name, *title;

		project->priv->path = (g_free (project->priv->path),
				 g_strdup (canonical_path));

		/* Update prefs dialogs here... */
		name = glade_project_get_name (project);
		title = g_strdup_printf (_("%s preferences"), name);
		gtk_window_set_title (GTK_WINDOW (project->priv->prefs_dialog), title);
		g_free (title);
		g_free (name);
	}

	glade_project_set_readonly (project, 
				    !glade_util_file_is_writeable (project->priv->path));

	project->priv->mtime = glade_util_get_file_mtime (project->priv->path, NULL);
	
	glade_project_set_modified (project, FALSE);

	if (project->priv->unsaved_number > 0)
	{
		glade_id_allocator_release (get_unsaved_number_allocator (), project->priv->unsaved_number);
		project->priv->unsaved_number = 0;
        }

	g_free (canonical_path);

	return ret > 0;
}

/*******************************************************************
     Verify code here (versioning, incompatability checks)
 *******************************************************************/

/* translators: reffers to a widget in toolkit version '%s %d.%d' and a project targeting toolkit version '%s %d.%d' */
#define WIDGET_VERSION_CONFLICT_MSGFMT         _("This widget was introduced in %s %d.%d while project targets %s %d.%d")

/* translators: reffers to a widget '[%s]' introduced in toolkit version '%s %d.%d' */
#define WIDGET_VERSION_CONFLICT_FMT            _("[%s] Object class '%s' was introduced in %s %d.%d\n")

/* translators: reffers to a widget in toolkit version '%s %d.%d' and a project targeting toolkit version '%s %d.%d' */
#define WIDGET_BUILDER_VERSION_CONFLICT_MSGFMT _("This widget was made available in GtkBuilder format in %s %d.%d " \
						 "while project targets %s %d.%d")

/* translators: reffers to a widget '[%s]' introduced in toolkit version '%s %d.%d' */
#define WIDGET_BUILDER_VERSION_CONFLICT_FMT    _("[%s] Object class '%s' was made available in GtkBuilder format " \
						 "in %s %d.%d\n")

#define WIDGET_LIBGLADE_ONLY_MSG               _("This widget is only supported in libglade format")

/* translators: reffers to a widget '[%s]' loaded from toolkit version '%s %d.%d' */
#define WIDGET_LIBGLADE_ONLY_FMT               _("[%s] Object class '%s' from %s %d.%d " \
						 "is only supported in libglade format\n")

#define WIDGET_LIBGLADE_UNSUPPORTED_MSG        _("This widget is not supported in libglade format")

/* translators: reffers to a widget '[%s]' loaded from toolkit version '%s %d.%d' */
#define WIDGET_LIBGLADE_UNSUPPORTED_FMT        _("[%s] Object class '%s' from %s %d.%d " \
						 "is not supported in libglade format\n")

#define WIDGET_DEPRECATED_MSG                  _("This widget is deprecated")

/* translators: reffers to a widget '[%s]' loaded from toolkit version '%s %d.%d' */
#define WIDGET_DEPRECATED_FMT                  _("[%s] Object class '%s' from %s %d.%d is deprecated\n")


/* Defined here for pretty translator comments (bug in intl tools, for some reason
 * you can only comment about the line directly following, forcing you to write
 * ugly messy code with comments in line breaks inside function calls).
 */
#define PROP_LIBGLADE_UNSUPPORTED_MSG          _("This property is not supported in libglade format")

/* translators: reffers to a property '%s' of widget '[%s]' */
#define PROP_LIBGLADE_UNSUPPORTED_FMT          _("[%s] Property '%s' of object class '%s' is not " \
						 "supported in libglade format\n")
/* translators: reffers to a property '%s' of widget '[%s]' */
#define PACK_PROP_LIBGLADE_UNSUPPORTED_FMT     _("[%s] Packing property '%s' of object class '%s' is not " \
						 "supported in libglade format\n")

#define PROP_LIBGLADE_ONLY_MSG                 _("This property is only supported in libglade format")

/* translators: reffers to a property '%s' of widget '[%s]' */
#define PROP_LIBGLADE_ONLY_FMT                 _("[%s] Property '%s' of object class '%s' is only " \
						 "supported in libglade format\n")

/* translators: reffers to a property '%s' of widget '[%s]' */
#define PACK_PROP_LIBGLADE_ONLY_FMT            _("[%s] Packing property '%s' of object class '%s' is only " \
						 "supported in libglade format\n")

/* translators: reffers to a property in toolkit version '%s %d.%d' 
 * and a project targeting toolkit version '%s %d.%d' */
#define PROP_VERSION_CONFLICT_MSGFMT           _("This property was introduced in %s %d.%d while project targets %s %d.%d")

/* translators: reffers to a property '%s' of widget '[%s]' in toolkit version '%s %d.%d' */
#define PROP_VERSION_CONFLICT_FMT              _("[%s] Property '%s' of object class '%s' was introduced in %s %d.%d\n")

/* translators: reffers to a property '%s' of widget '[%s]' in toolkit version '%s %d.%d' */
#define PACK_PROP_VERSION_CONFLICT_FMT         _("[%s] Packing property '%s' of object class '%s' " \
						 "was introduced in %s %d.%d\n")

/* translators: reffers to a property in toolkit version '%s %d.%d' and a project targeting toolkit version '%s %d.%d' */
#define PROP_BUILDER_VERSION_CONFLICT_MSGFMT   _("This property was made available in GtkBuilder format in %s %d.%d " \
						 "while project targets %s %d.%d")

/* translators: reffers to a property '%s' of widget '[%s]' in toolkit version '%s %d.%d' */
#define PROP_BUILDER_VERSION_CONFLICT_FMT      _("[%s] Property '%s' of object class '%s' was " \
						 "made available in GtkBuilder format in %s %d.%d\n")

/* translators: reffers to a property '%s' of widget '[%s]' in toolkit version '%s %d.%d' */
#define PACK_PROP_BUILDER_VERSION_CONFLICT_FMT _("[%s] Packing property '%s' of object class '%s' " \
						 "was made available in GtkBuilder format in %s %d.%d\n")

/* translators: reffers to a signal '%s' of widget '[%s]' in toolkit version '%s %d.%d' */
#define SIGNAL_VERSION_CONFLICT_FMT            _("[%s] Signal '%s' of object class '%s' was introduced in %s %d.%d\n")

/* translators: reffers to a signal in toolkit version '%s %d.%d' 
 * and a project targeting toolkit version '%s %d.%d' */
#define SIGNAL_VERSION_CONFLICT_MSGFMT         _("This signal was introduced in %s %d.%d while project targets %s %d.%d")

static void
glade_project_verify_property (GladeProject   *project,
			       GladeProperty  *property, 
			       const gchar    *path_name,
			       GString        *string,
			       gboolean        forwidget)
{
	GladeWidgetAdaptor *adaptor, *prop_adaptor;
	gint target_major, target_minor;
	gchar *catalog, *tooltip;

	if (glade_property_original_default (property) && !forwidget)
		return;

	prop_adaptor = glade_widget_adaptor_from_pclass (property->klass);
	adaptor = glade_widget_adaptor_from_pspec (prop_adaptor, property->klass->pspec);
	
	g_object_get (adaptor, "catalog", &catalog, NULL);
	glade_project_target_version_for_adaptor (property->widget->project, adaptor, 
						  &target_major,
						  &target_minor);
	
	if (project->priv->format == GLADE_PROJECT_FORMAT_LIBGLADE &&
	    property->klass->libglade_unsupported)
	{
		if (forwidget)
			glade_property_set_support_warning
				(property, TRUE, PROP_LIBGLADE_UNSUPPORTED_MSG);
		else
			g_string_append_printf (string,
						property->klass->packing ?
						PACK_PROP_LIBGLADE_UNSUPPORTED_FMT :
						PROP_LIBGLADE_UNSUPPORTED_FMT,
						path_name,
						property->klass->name, 
						adaptor->title);
	}
	else if (project->priv->format == GLADE_PROJECT_FORMAT_GTKBUILDER &&
		 property->klass->libglade_only)
	{
		if (forwidget)
			glade_property_set_support_warning
				(property, TRUE, PROP_LIBGLADE_ONLY_MSG);
		else
			g_string_append_printf (string,
						property->klass->packing ?
						PACK_PROP_LIBGLADE_ONLY_FMT :
						PROP_LIBGLADE_ONLY_FMT,
						path_name,
						property->klass->name, 
						adaptor->title);
	} 
	else if (!GPC_VERSION_CHECK (property->klass, target_major, target_minor))
	{
		if (forwidget)
		{
			tooltip = g_strdup_printf (PROP_VERSION_CONFLICT_MSGFMT,
						   catalog,
						   property->klass->version_since_major,
						   property->klass->version_since_minor,
						   catalog,
						   target_major, target_minor);
			
			glade_property_set_support_warning (property, FALSE, tooltip);
			g_free (tooltip);
		}
		else
			g_string_append_printf (string,
						property->klass->packing ?
						PACK_PROP_VERSION_CONFLICT_FMT :
						PROP_VERSION_CONFLICT_FMT,
						path_name,
						property->klass->name, 
						adaptor->title, catalog,
						property->klass->version_since_major,
						property->klass->version_since_minor);
	} 
	else if (project->priv->format == GLADE_PROJECT_FORMAT_GTKBUILDER &&
		 !GPC_BUILDER_VERSION_CHECK (property->klass, target_major, target_minor))
	{
		if (forwidget)
		{
			tooltip = g_strdup_printf (PROP_BUILDER_VERSION_CONFLICT_MSGFMT,
						   catalog,
						   property->klass->builder_since_major,
						   property->klass->builder_since_minor,
						   catalog,
						   target_major, target_minor);
			
			glade_property_set_support_warning (property, FALSE, tooltip);
			g_free (tooltip);
		}
		else
			g_string_append_printf (string,
						property->klass->packing ?
						PACK_PROP_BUILDER_VERSION_CONFLICT_FMT :
						PROP_BUILDER_VERSION_CONFLICT_FMT,
						path_name,
						property->klass->name, 
						adaptor->title, catalog,
						property->klass->builder_since_major,
						property->klass->builder_since_minor);
	} 
	else if (forwidget)
		glade_property_set_support_warning (property, FALSE, NULL);

	g_free (catalog);
}

static void
glade_project_verify_properties_internal (GladeWidget  *widget, 
					  const gchar  *path_name,
					  GString      *string,
					  gboolean      forwidget)
{
	GList *list;
	GladeProperty *property;

	for (list = widget->properties; list; list = list->next)
	{
		property = list->data;
		glade_project_verify_property (widget->project, property, path_name, string, forwidget);
	}

	/* Sometimes widgets on the clipboard have packing props with no parent */
	if (widget->parent)
	{
		for (list = widget->packing_properties; list; list = list->next)
		{
			property = list->data;
			glade_project_verify_property (widget->project, property, path_name, string, forwidget);
		}
	}
}

static void
glade_project_verify_signal (GladeWidget  *widget,
			     GladeSignal  *signal,
			     const gchar  *path_name,
			     GString      *string,
			     gboolean      forwidget)
{
	GladeSignalClass *signal_class;
	gint target_major, target_minor;
	gchar *catalog;

	signal_class = 
		glade_widget_adaptor_get_signal_class (widget->adaptor,
						       signal->name);
	//* Cannot verify unknown signal */
	if (!signal_class)
		return;
	g_assert (signal_class->adaptor);
	
	g_object_get (signal_class->adaptor, "catalog", &catalog, NULL);
	glade_project_target_version_for_adaptor (widget->project, 
						  signal_class->adaptor, 
						  &target_major,
						  &target_minor);

	if (!GSC_VERSION_CHECK (signal_class, target_major, target_minor))
	{
		if (forwidget)
		{
			gchar *warning;

			warning = g_strdup_printf (SIGNAL_VERSION_CONFLICT_MSGFMT,
						   catalog,
						   signal_class->version_since_major,
						   signal_class->version_since_minor,
						   catalog, target_major, target_minor);
			glade_signal_set_support_warning (signal, warning);
			g_free (warning);
		}
		else
			g_string_append_printf (string, 
						SIGNAL_VERSION_CONFLICT_FMT,
						path_name,
						signal->name,
						signal_class->adaptor->title, 
						catalog,
						signal_class->version_since_major,
						signal_class->version_since_minor);
	}
	else if (forwidget)
		glade_signal_set_support_warning (signal, NULL);

	g_free (catalog);
}

void
glade_project_update_signal_support_warning (GladeWidget  *widget,
					     GladeSignal  *signal)
{
	glade_project_verify_signal (widget, signal, NULL, NULL, TRUE);
}

static void
glade_project_verify_signals (GladeWidget  *widget, 
			      const gchar  *path_name,
			      GString      *string,
			      gboolean      forwidget)
{
	GladeSignal      *signal;
	GList *signals, *list;
	
	if ((signals = glade_widget_get_signal_list (widget)) != NULL)
	{
		for (list = signals; list; list = list->next)
		{
			signal = list->data;
			glade_project_verify_signal (widget, signal, path_name, string, forwidget);
		}
		g_list_free (signals);
	}	
}


/**
 * glade_project_verify_properties:
 * @widget: A #GladeWidget
 *
 * Synchonizes @widget with user visible information
 * about version compatability and notifies the UI
 * it should update.
 */
void
glade_project_verify_properties (GladeWidget *widget)
{
	GladeProject *project;

	g_return_if_fail (GLADE_IS_WIDGET (widget));

	project = glade_widget_get_project (widget);
	if (!project || project->priv->loading)
		return;

	glade_project_verify_properties_internal (widget, NULL, NULL, TRUE);
	glade_project_verify_signals (widget, NULL, NULL, TRUE);

	glade_widget_support_changed (widget);
}

static gboolean
glade_project_verify_dialog (GladeProject *project,
			     GString      *string,
			     gboolean      saving)
{
	GtkWidget     *swindow;
	GtkWidget     *textview;
	GtkWidget     *expander;
	GtkTextBuffer *buffer;
	gchar         *name;
	gboolean ret;

	swindow   = gtk_scrolled_window_new (NULL, NULL);
	textview  = gtk_text_view_new ();
	buffer    = gtk_text_view_get_buffer (GTK_TEXT_VIEW (textview));
	expander  = gtk_expander_new (_("Details"));

	gtk_text_buffer_set_text (buffer, string->str, -1);

	gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (swindow),
					       textview);
	gtk_container_add (GTK_CONTAINER (expander), swindow);
	gtk_widget_show_all (expander);

	gtk_widget_set_size_request (swindow, 800, -1);
	
	name = glade_project_get_name (project);
	ret = glade_util_ui_message (glade_app_get_window (),
				     saving ? GLADE_UI_YES_OR_NO : GLADE_UI_INFO,
				     expander,
				     saving ? 
				     _("Project \"%s\" has errors. Save anyway?") :
				     _("Project \"%s\" has deprecated widgets "
				       "and/or version mismatches."), name);
	g_free (name);

	return ret;
}


static gboolean
glade_project_verify (GladeProject *project, 
		      gboolean      saving)
{
  GString *string = g_string_new (NULL);
  GList *list;
  gboolean ret = TRUE;
  
  for (list = project->priv->objects; list; list = list->next)
    {
      GladeWidget *widget = glade_widget_get_from_gobject (list->data);
      
      if (GLADE_IS_OBJECT_STUB (list->data))
        {
          gchar *type;
          g_object_get (list->data, "object-type", &type, NULL);
          
          /* translators: reffers to an unknow object named '%s' of type '%s' */
          g_string_append_printf (string, _("Unknow object %s with type %s\n"), 
                                  glade_widget_get_name (widget), type);
          g_free (type);
        }
      else
        {
          gchar *path_name = glade_widget_generate_path_name (widget);

          glade_project_verify_adaptor (project, glade_widget_get_adaptor (widget),
                                        path_name, string, saving, FALSE, NULL);
          glade_project_verify_properties_internal (widget, path_name, string,
                                                    FALSE);
          glade_project_verify_signals (widget, path_name, string, FALSE);

          g_free (path_name);
        }
    }

	if (string->len > 0)
	{
		ret = glade_project_verify_dialog (project, string, saving);

		if (!saving)
			ret = FALSE;
	}

	g_string_free (string, TRUE);

	return ret;
}

static void
glade_project_target_version_for_adaptor (GladeProject        *project, 
					  GladeWidgetAdaptor  *adaptor,
					  gint                *major,
					  gint                *minor)
{
	gchar   *catalog = NULL;

	g_object_get (adaptor, "catalog", &catalog, NULL);
	glade_project_get_target_version (project, catalog, major, minor);
	g_free (catalog);
}

static void
glade_project_verify_adaptor (GladeProject       *project,
			      GladeWidgetAdaptor *adaptor,
			      const gchar        *path_name,
			      GString            *string,
			      gboolean            saving,
			      gboolean            forwidget,
			      GladeSupportMask   *mask)
{
	GladeSupportMask    support_mask = GLADE_SUPPORT_OK;
	GladeWidgetAdaptor *adaptor_iter;
	gint                target_major, target_minor;
	gchar              *catalog = NULL;

	for (adaptor_iter = adaptor; adaptor_iter && support_mask == GLADE_SUPPORT_OK;
	     adaptor_iter = glade_widget_adaptor_get_parent_adaptor (adaptor_iter))
	{

		g_object_get (adaptor_iter, "catalog", &catalog, NULL);
		glade_project_target_version_for_adaptor (project, adaptor_iter, 
							  &target_major,
							  &target_minor);

		/* Only one versioning message (builder or otherwise)...
		 */
		if (!GWA_VERSION_CHECK (adaptor_iter, target_major, target_minor))
		{
			if (forwidget)
				g_string_append_printf (string, 
							WIDGET_VERSION_CONFLICT_MSGFMT,
							catalog,
							GWA_VERSION_SINCE_MAJOR (adaptor_iter),
							GWA_VERSION_SINCE_MINOR (adaptor_iter),
							catalog, target_major, target_minor);
			else
				g_string_append_printf (string, 
							WIDGET_VERSION_CONFLICT_FMT,
							path_name, adaptor_iter->title, catalog,
							GWA_VERSION_SINCE_MAJOR (adaptor_iter),
							GWA_VERSION_SINCE_MINOR (adaptor_iter));
			
			support_mask |= GLADE_SUPPORT_MISMATCH;
		}
		else if (project->priv->format == GLADE_PROJECT_FORMAT_GTKBUILDER &&
			 (target_major < GWA_BUILDER_SINCE_MAJOR (adaptor_iter) ||
			  (target_major == GWA_BUILDER_SINCE_MAJOR (adaptor_iter) &&
			   target_minor < GWA_BUILDER_SINCE_MINOR (adaptor_iter))))
		{
			if (forwidget)
				g_string_append_printf (string, 
							WIDGET_BUILDER_VERSION_CONFLICT_MSGFMT,
							catalog,
							GWA_BUILDER_SINCE_MAJOR (adaptor_iter),
							GWA_BUILDER_SINCE_MINOR (adaptor_iter),
							catalog, target_major, target_minor);
			else
				g_string_append_printf (string, 
							WIDGET_BUILDER_VERSION_CONFLICT_FMT,
							path_name, adaptor_iter->title, catalog,
							GWA_BUILDER_SINCE_MAJOR (adaptor_iter),
							GWA_BUILDER_SINCE_MINOR (adaptor_iter));

			support_mask |= GLADE_SUPPORT_MISMATCH;
		}

		/* Now accumulate some more messages...
		 */
		if (project->priv->format == GLADE_PROJECT_FORMAT_GTKBUILDER &&
		    GWA_LIBGLADE_ONLY (adaptor_iter))
		{
			if (forwidget)
			{
				if (string->len)
					g_string_append (string, "\n");
				g_string_append_printf (string, WIDGET_LIBGLADE_ONLY_MSG);
			}
			else
				g_string_append_printf (string,
							WIDGET_LIBGLADE_ONLY_FMT,
							path_name, adaptor_iter->title, catalog,
							target_major, target_minor);

			support_mask |= GLADE_SUPPORT_LIBGLADE_ONLY;
		}
		else if (project->priv->format == GLADE_PROJECT_FORMAT_LIBGLADE &&
			 GWA_LIBGLADE_UNSUPPORTED (adaptor_iter))
		{
			if (forwidget)
			{
				if (string->len)
					g_string_append (string, "\n");

				g_string_append_printf (string, WIDGET_LIBGLADE_UNSUPPORTED_MSG);
			}
			else
				g_string_append_printf (string, WIDGET_LIBGLADE_UNSUPPORTED_FMT,
							path_name, adaptor_iter->title, catalog,
							target_major, target_minor);

			support_mask |= GLADE_SUPPORT_LIBGLADE_UNSUPPORTED;
		}

		if (!saving && GWA_DEPRECATED (adaptor_iter))
		{
			if (forwidget)
			{
				if (string->len)
					g_string_append (string, "\n");

				g_string_append_printf (string, WIDGET_DEPRECATED_MSG);
			}
			else
				g_string_append_printf (string, WIDGET_DEPRECATED_FMT,
							path_name, adaptor_iter->title, catalog,
							target_major, target_minor);

			support_mask |= GLADE_SUPPORT_DEPRECATED;
		}
		g_free (catalog);
	}
	if (mask)
		*mask = support_mask;

}


static gint
glade_widgets_name_cmp (gconstpointer ga, gconstpointer gb)
{
  return g_strcmp0 (glade_widget_get_name ((gpointer)ga),
                    glade_widget_get_name ((gpointer)gb));
}

static gint
glade_node_edge_name_cmp (gconstpointer ga, gconstpointer gb)
{
  _NodeEdge *na = (gpointer)ga, *nb = (gpointer)gb;
  return g_strcmp0 (glade_widget_get_name (nb->successor),
                    glade_widget_get_name (na->successor));
}

static inline gboolean
glade_project_widget_hard_depends (GladeWidget *widget, GladeWidget *another)
{
  GList *l;

  for (l = _glade_widget_peek_prop_refs (another); l; l = g_list_next (l))
    {
      GladePropertyClass *klass;
      
      /* If one of the properties that reference @another is
       * owned by @widget then @widget depends on @another
       */
      if (glade_property_get_widget (l->data) == widget &&
          (klass = glade_property_get_class (l->data)) &&
          glade_property_class_get_construct_only (klass))
        return TRUE;
    }

  return FALSE;
}

static GList *
glade_project_get_graph_deps (GladeProject *project)
{
  GladeProjectPrivate *priv = project->priv;
  GList *l, *edges = NULL;
  
  /* Create edges of the directed graph */
  for (l = priv->objects; l; l = g_list_next (l))
    {
      GladeWidget *predecessor = glade_widget_get_from_gobject (l->data);
      GladeWidget *predecessor_top;
      GList *ll;

      predecessor_top = glade_widget_get_toplevel (predecessor);

      /* Adds dependencies based on properties refs */
      for (ll = _glade_widget_peek_prop_refs (predecessor); ll; ll = g_list_next (ll))
        {
          GladeWidget *successor = glade_property_get_widget (ll->data);
          GladeWidget *successor_top;

          /* Ignore widgets that are not part of this project. (ie removed ones) */
          if (glade_widget_get_project (successor) != project)
            continue;

          successor_top = glade_widget_get_toplevel (successor);

          /* Ignore objects within the same toplevel */
          if (predecessor_top != successor_top)
            edges = _node_edge_prepend (edges, predecessor_top, successor_top);
        }
    }

  return edges;
}

static GList *
glade_project_get_nodes_from_edges (GladeProject *project, GList *edges)
{
  GList *l, *hard_edges = NULL;
  GList *cycles = NULL;

  /* Collect widgets with circular dependencies */
  for (l = edges; l; l = g_list_next (l))
    {
      _NodeEdge *edge = l->data;

      if (glade_widget_get_parent (edge->successor) == edge->predecessor ||
          glade_project_widget_hard_depends (edge->predecessor, edge->successor))
        hard_edges = _node_edge_prepend (hard_edges, edge->predecessor, edge->successor);

      /* Just toplevels */
      if (glade_widget_get_parent (edge->successor))
        continue;

      if (!g_list_find (cycles, edge->successor))
        cycles = g_list_prepend (cycles, edge->successor);
    }

  /* Sort them alphabetically */
  cycles = g_list_sort (cycles, glade_widgets_name_cmp);

  if (!hard_edges)
    return cycles;

  /* Sort them by hard deps */
  cycles = _glade_tsort (&cycles, &hard_edges);
  
  if (hard_edges)
    {
      GList *l, *hard_cycles = NULL;

      /* Collect widgets with hard circular dependencies */
      for (l = hard_edges; l; l = g_list_next (l))
        {
          _NodeEdge *edge = l->data;

          /* Just toplevels */
          if (glade_widget_get_parent (edge->successor))
            continue;

          if (!g_list_find (hard_cycles, edge->successor))
            hard_cycles = g_list_prepend (hard_cycles, edge->successor);
        }

      /* And append to the end of the cycles list */
      cycles = g_list_concat (cycles, g_list_sort (hard_cycles, glade_widgets_name_cmp));

      /* Opps! there is at least one hard circular dependency,
       * GtkBuilder will fail to set one of the properties that create the cycle
       */

      _node_edge_list_free (hard_edges);
    }

  return cycles;
}

static GList *
glade_project_add_hardcoded_dependencies (GList *edges, GladeProject *project)
{
  GList *l, *toplevels = project->priv->tree;

  /* Iterate over every toplevel */
  for (l = toplevels; l; l = g_list_next (l))
    {
      GObject *predecessor = l->data;

      /* Looking for a GtkIconFactory */
      if (GTK_IS_ICON_FACTORY (predecessor))
        {
          GladeWidget *predecessor_top = glade_widget_get_from_gobject (predecessor);
          GList *ll;

          /* add dependency on the GtkIconFactory on every toplevel */
          for (ll = toplevels; ll; ll = g_list_next (ll))
            {          
              GObject *successor = ll->data;

              /* except for GtkIconFactory */
              if (!GTK_IS_ICON_FACTORY (successor))
                edges = _node_edge_prepend (edges, predecessor_top,
                                            glade_widget_get_from_gobject (successor));
            }
        }
    }

  return edges;
}

static GList *
glade_project_get_ordered_toplevels (GladeProject *project)
{
  GladeProjectPrivate *priv = project->priv;
  GList *l, *sorted_tree, *tree = NULL;
  GList *edges;

  /* Create list of toplevels GladeWidgets */
  for (l = priv->tree; l; l = g_list_next (l))
    tree = g_list_prepend (tree, glade_widget_get_from_gobject (l->data));

  /* Get dependency graph */
  edges = glade_project_get_graph_deps (project);

  /* Added hardcoded dependencies */
  edges = glade_project_add_hardcoded_dependencies (edges, project);
    
  /* Sort toplevels alphabetically */
  tree = g_list_sort (tree, glade_widgets_name_cmp);

  /* Sort dep graph alphabetically using successor name.
   * _glade_tsort() is a stable sort algorithm so, output of nodes without
   * dependency depends on the input order
   */
  edges = g_list_sort (edges, glade_node_edge_name_cmp);
  
  /* Sort tree */
  sorted_tree = _glade_tsort (&tree, &edges);

  if (edges)
    {
      GList *cycles = glade_project_get_nodes_from_edges (project, edges);
      
      /* And append to the end of the sorted list */
      sorted_tree = g_list_concat (sorted_tree, cycles);

      _node_edge_list_free (edges);
    }

  /* No need to free tree as tsort will consume the list */
  return sorted_tree;
}

static GladeXmlContext *
glade_project_write (GladeProject *project)
{
	GladeXmlContext *context;
	GladeXmlDoc     *doc;
	GladeXmlNode    *root; /* *comment_node; */
	GList           *list;
	GList           *toplevels;

	doc     = glade_xml_doc_new ();
	context = glade_xml_context_new (doc, NULL);
	root    = glade_xml_node_new (context, GLADE_XML_TAG_PROJECT (project->priv->format));
	glade_xml_doc_set_root (doc, root);

	glade_project_update_comment (project);
	/* comment_node = glade_xml_node_new_comment (context, project->priv->comment); */

	/* XXX Need to append this to the doc ! not the ROOT !
	   glade_xml_node_append_child (root, comment_node); */

	glade_project_write_required_libs (project, context, root);

	glade_project_write_naming_policy (project, context, root);

	glade_project_write_resource_path (project, context, root);

  /* Get sorted toplevels */
  toplevels = glade_project_get_ordered_toplevels (project);

  for (list = toplevels; list; list = g_list_next (list))
    {
      GladeWidget *widget = list->data;

      /* 
       * Append toplevel widgets. Each widget then takes
       * care of appending its children.
       */
      if (!glade_widget_get_parent (widget))
        glade_widget_write (widget, context, root);
      else
        g_warning ("Tried to save a non toplevel object '%s' at xml root",
                   glade_widget_get_name (widget));
    }

  g_list_free (toplevels);
	
	return context;
}

/**
 * glade_project_verify_widget_adaptor:
 * @project: A #GladeProject
 * @adaptor: the #GladeWidgetAdaptor to verify
 * @mask: a return location for a #GladeSupportMask
 * 
 * Checks the supported state of this widget adaptor
 * and generates a string to show in the UI describing why.
 *
 * Returns: A newly allocated string 
 */
gchar *
glade_project_verify_widget_adaptor (GladeProject       *project,
				     GladeWidgetAdaptor *adaptor,
				     GladeSupportMask   *mask)
{
	GString *string = g_string_new (NULL);
	gchar   *ret = NULL;

	glade_project_verify_adaptor (project, adaptor, NULL,
				      string, FALSE, TRUE, mask);

	/* there was a '\0' byte... */
	if (string->len > 0)
	{
		ret = string->str;
		g_string_free (string, FALSE);
	}
	else
		g_string_free (string, TRUE);


	return ret;
}


/**
 * glade_project_verify_project_for_ui:
 * @project: A #GladeProject
 * 
 * Checks the project and updates warning strings in the UI
 */
void
glade_project_verify_project_for_ui (GladeProject *project)
{
	GList *list;
	GladeWidget *widget;
	gchar *warning;

	/* Sync displayable info here */
	for (list = project->priv->objects; list; list = list->next)
	{
		widget = glade_widget_get_from_gobject (list->data);

		warning = glade_project_verify_widget_adaptor (project, widget->adaptor, NULL);
		glade_widget_set_support_warning (widget, warning);

		if (warning)
			g_free (warning);

		glade_project_verify_properties (widget);
	}

	/* refresh palette if this is the active project */
	if (project == glade_app_get_project ())
		glade_palette_refresh (glade_app_get_palette ());
}

/*******************************************************************
   Project object tracking code, name exclusivity etc...
 *******************************************************************/
static GladeNameContext *
name_context_by_widget (GladeProject *project, 
			GladeWidget  *gwidget)
{
	TopLevelInfo  *tinfo;
	GladeWidget   *iter;
	GList         *list;

	if (!gwidget->parent)
		return NULL;

	iter = gwidget;
	while (iter->parent) iter = iter->parent;

	for (list = project->priv->toplevels; list; list = list->next)
	{
		tinfo = list->data;
		if (tinfo->toplevel == iter)
			return tinfo->names;
	}
	return NULL;
}

static GladeWidget *
search_ancestry_by_name (GladeWidget *toplevel, const gchar *name)
{
	GladeWidget *widget = NULL, *iter;
	GList *list, *children;

	if ((children = glade_widget_get_children (toplevel)) != NULL)
	{
		for (list = children; list; list = list->next) 
		{
			iter = glade_widget_get_from_gobject (list->data);

			if (iter->name && strcmp (iter->name, name) == 0)
			{
				widget = iter;
				break;
			}
			else if ((widget = search_ancestry_by_name (iter, name)) != NULL)
				break;
		}
		
		g_list_free (children);
	}
	return widget;
}

/**
 * glade_project_get_widget_by_name:
 * @project: a #GladeProject
 * @ancestor: The toplevel project object to search under
 * @name: The user visible name of the widget we are looking for
 * 
 * Searches under @ancestor in @project looking for a #GladeWidget named @name.
 * 
 * Returns: a pointer to the widget, %NULL if the widget does not exist
 */
GladeWidget *
glade_project_get_widget_by_name (GladeProject *project, GladeWidget *ancestor, const gchar *name)
{
	GladeWidget *toplevel, *widget = NULL;
	GList *list;

	g_return_val_if_fail (GLADE_IS_PROJECT (project), NULL);
	g_return_val_if_fail (name != NULL, NULL);
	
	if (ancestor)
	{
		toplevel = glade_widget_get_toplevel (ancestor);
		if ((widget = search_ancestry_by_name (toplevel, name)) != NULL)
			return widget;
	}

	/* Now try searching in only toplevel objects... */
	for (list = project->priv->tree; list; list = list->next) {
		GladeWidget *widget;

		widget = glade_widget_get_from_gobject (list->data);
		g_assert (widget->name);
		if (strcmp (widget->name, name) == 0)
			return widget;
	}

	/* Finally resort to a project wide search. */
	for (list = project->priv->objects; list; list = list->next) {
		GladeWidget *widget;

		widget = glade_widget_get_from_gobject (list->data);
		g_return_val_if_fail (widget->name != NULL, NULL);
		if (strcmp (widget->name, name) == 0)
			return widget;
	}

	return NULL;
}

static void
glade_project_release_widget_name (GladeProject *project, GladeWidget *gwidget, const char *widget_name)
{
	GladeNameContext *context = NULL;
	TopLevelInfo     *tinfo = NULL;
	GladeWidget      *iter;
	GList            *list;

	/* Search by hand here since we need the tinfo to free... */
	iter = gwidget;
	while (iter->parent) iter = iter->parent;

	for (list = project->priv->toplevels; list; list = list->next)
	{
		tinfo = list->data;
		if (tinfo->toplevel == iter)
		{
			context = tinfo->names;
			break;
		}
	}

	if (context)
	{
		glade_name_context_release_name (context, widget_name);
		//g_print ("Released widget name %s from sub context\n", widget_name);
	}

	if (project->priv->naming_policy == GLADE_POLICY_PROJECT_WIDE || !gwidget->parent)
	{
		glade_name_context_release_name (project->priv->toplevel_names, widget_name);
		//g_print ("Released widget name %s from toplevel context\n", widget_name);
	}

	if (context && glade_name_context_n_names (context) == 0)
       	{
		glade_name_context_destroy (context);
		g_free (tinfo);
		project->priv->toplevels = g_list_remove (project->priv->toplevels, tinfo);
	}
}

/**
 * glade_project_available_widget_name:
 * @project: a #GladeProject
 * @widget: the #GladeWidget intended to recieve a new name
 * @name: base name of the widget to create
 *
 * Checks whether @name is an appropriate name for @widget.
 *
 * Returns: whether the name is available or not.
 */
gboolean
glade_project_available_widget_name (GladeProject *project, 
				     GladeWidget  *widget,
				     const gchar  *name)
{
	GladeNameContext *context;
	gboolean          sub_has_name = FALSE;
	gboolean          available = FALSE;
	
	g_return_val_if_fail (GLADE_IS_PROJECT (project), FALSE);
	g_return_val_if_fail (GLADE_IS_WIDGET (widget), FALSE);

	if (!name || !name[0])
		return FALSE;

	if ((context = name_context_by_widget (project, widget)) != NULL)
		sub_has_name = glade_name_context_has_name (context, name);

	if (project->priv->naming_policy == GLADE_POLICY_PROJECT_WIDE)
       	{
		available = (!sub_has_name &&
			     !glade_name_context_has_name (project->priv->toplevel_names, name));

	} 
	else if (context)
		available = !sub_has_name;
	else
		available = !glade_name_context_has_name (project->priv->toplevel_names, name);

	//g_print ("widget name %s is available: %d (policy %d)\n", name, available, project->priv->naming_policy);

	return available;
}

static void
glade_project_reserve_widget_name (GladeProject *project, GladeWidget *gwidget, const char *widget_name)
{
	GladeNameContext *context;

	if (!glade_project_available_widget_name (project, gwidget, widget_name))
	{
		g_warning ("BUG: widget '%s' attempting to reserve an unavailable widget name '%s' !",
			   gwidget->name, widget_name);
		return;
	}

	/* Add to name context(s) */
	if ((context = name_context_by_widget (project, gwidget)) != NULL)
	{
		//g_print ("Reserving widget name %s from sub context\n", widget_name);
		glade_name_context_add_name (context, widget_name);
	}

	if (project->priv->naming_policy == GLADE_POLICY_PROJECT_WIDE || !gwidget->parent)
       	{
		glade_name_context_add_name (project->priv->toplevel_names, widget_name);
		//g_print ("Reserving widget name %s from main context\n", widget_name);
	}

}

/**
 * glade_project_new_widget_name:
 * @project: a #GladeProject
 * @widget: the #GladeWidget intended to recieve a new name
 * @base_name: base name of the widget to create
 *
 * Creates a new name for a widget that doesn't collide with any of the names 
 * already in @project. This name will start with @base_name.
 *
 * Returns: a string containing the new name, %NULL if there is not enough 
 *          memory for this string
 */
gchar *
glade_project_new_widget_name (GladeProject *project, 
			       GladeWidget  *widget,
			       const gchar  *base_name)
{
	GladeNameContext *context;
	gchar            *name;

	g_return_val_if_fail (GLADE_IS_PROJECT (project), NULL);
	g_return_val_if_fail (GLADE_IS_WIDGET (widget), NULL);
	g_return_val_if_fail (base_name && base_name[0], NULL);

	context = name_context_by_widget (project, widget);

	if (project->priv->naming_policy == GLADE_POLICY_PROJECT_WIDE)
       	{
		if (context)
			name = glade_name_context_dual_new_name (context, project->priv->toplevel_names, base_name);
		else
			name = glade_name_context_new_name (project->priv->toplevel_names, base_name);
	} 
	else if (context)
		name = glade_name_context_new_name (context, base_name);
	else
		name = glade_name_context_new_name (project->priv->toplevel_names, base_name);

	//g_print ("Allocating widget name %s, widget parent %p\n", name, widget->parent);

	return name;
}

/**
 * glade_project_set_widget_name:
 * @project: a #GladeProject
 * @widget: the #GladeWidget to set a name on
 * @name: the name to set.
 *
 * Sets @name on @widget in @project, if @name is not
 * available then a new name will be used.
 */
void
glade_project_set_widget_name (GladeProject *project, 
			       GladeWidget  *widget, 
			       const gchar  *name)
{
	gchar       *new_name;
	GtkTreeIter  iter;
	GtkTreePath *path;

	g_return_if_fail (GLADE_IS_PROJECT (project));
	g_return_if_fail (GLADE_IS_WIDGET (widget));
	g_return_if_fail (name && name[0]);

	if (strcmp (name, widget->name) == 0)
		return;

	/* Police the widget name */
	if (!glade_project_available_widget_name (project, widget, name))
		new_name = glade_project_new_widget_name (project, widget, name);
	else
		new_name = g_strdup (name);

	glade_project_reserve_widget_name (project, widget, new_name);

	/* Release old name and set new widget name */
	glade_project_release_widget_name (project, widget, widget->name);
	glade_widget_set_name (widget, new_name);
	
	g_signal_emit (G_OBJECT (project),
		       glade_project_signals [WIDGET_NAME_CHANGED],
		       0, widget);

	g_free (new_name);

	/* Notify views about the iter change */
	glade_project_model_get_iter_for_object (project, widget->object, &iter);
	path = gtk_tree_model_get_path (GTK_TREE_MODEL (project), &iter);
	gtk_tree_model_row_changed (GTK_TREE_MODEL (project), path, &iter);
	gtk_tree_path_free (path);
}

static void
glade_project_notify_row_has_child (GladeProject *project,
				    GladeWidget  *gwidget,
				    gboolean      adding)
{
	GladeWidget *parent;
	gint         siblings;
	
	parent = glade_widget_get_parent (gwidget);

	if (parent)
	{
		siblings = glade_project_count_children (project, parent);

		if (siblings == (adding ? 1 : 0))
		{
			GtkTreePath *path;
			GtkTreeIter  iter;
 
			glade_project_model_get_iter_for_object (project, 
								 glade_widget_get_object (parent), &iter);
			path = gtk_tree_model_get_path (GTK_TREE_MODEL (project), &iter);
			gtk_tree_model_row_has_child_toggled (GTK_TREE_MODEL (project), path, &iter);
			gtk_tree_path_free (path);
		}
	}
}
 

static void 
glade_project_notify_row_inserted (GladeProject *project,
				   GladeWidget  *gwidget)
{
	GtkTreeIter       iter;
	GtkTreePath	 *path;

	/* The state of old iters go invalid and then the new iter is valid
	 * until the next change */
	project->priv->stamp++;

	glade_project_model_get_iter_for_object (project, gwidget->object, &iter);
	path = gtk_tree_model_get_path (GTK_TREE_MODEL (project), &iter);
	gtk_tree_model_row_inserted (GTK_TREE_MODEL (project), path, &iter);
	gtk_tree_path_free (path);

	glade_project_notify_row_has_child (project, gwidget, TRUE);
}

static void 
glade_project_notify_row_deleted (GladeProject *project,
				   GladeWidget  *gwidget)
{
	GtkTreeIter    iter;
	GtkTreePath   *path;

	glade_project_model_get_iter_for_object (project, gwidget->object, &iter);
	path = gtk_tree_model_get_path (GTK_TREE_MODEL (project), &iter);
	gtk_tree_model_row_deleted (GTK_TREE_MODEL (project), path);
	gtk_tree_path_free (path);

	project->priv->stamp++;
}

void
glade_project_check_reordered (GladeProject       *project,
			       GladeWidget        *parent,
			       GList              *old_order)
{
	GList       *new_order, *l, *ll;
	gint        *order, n_children, i;
	GtkTreeIter  iter;
	GtkTreePath *path;

	g_return_if_fail (GLADE_IS_PROJECT (project));
	g_return_if_fail (GLADE_IS_WIDGET (parent));
	g_return_if_fail (glade_project_has_object (project,
											 glade_widget_get_object (parent)));

	new_order = glade_widget_get_children (parent);

	/* Check if the list changed */
	for (l = old_order, ll = new_order;  l && ll; l = l->next, ll = ll->next)
	{
		if (l->data != ll->data)
			break;
	}

	if (l || ll)
	{
		n_children = glade_project_count_children (project, parent);
		order = g_new (gint, n_children);

		for (i = 0, l = new_order; l; l = l->next)
		{
			GObject *obj = l->data;

			if (glade_project_has_object (project, obj))
			{
				GList *node = g_list_find (old_order, obj);

				g_assert (node);

				order[i] = g_list_position (old_order, node);

				i++;
			}
		}

		/* Signal that the rows were reordered */
		glade_project_model_get_iter_for_object (project, glade_widget_get_object (parent), &iter);
		path = gtk_tree_model_get_path (GTK_TREE_MODEL (project), &iter);
		gtk_tree_model_rows_reordered (GTK_TREE_MODEL (project), path, &iter, order);
		gtk_tree_path_free (path);

		g_free (order);

	}

	g_list_free (new_order);
}

/**
 * glade_project_add_object:
 * @project: the #GladeProject the widget is added to
 * @old_project: the #GladeProject the widget was previously in
 *               (or %NULL for the clipboard)
 * @object: the #GObject to add
 *
 * Adds an object to the project.
 */
void
glade_project_add_object (GladeProject *project, 
			  GladeProject *old_project, 
			  GObject      *object)
{
	GladeWidget      *gwidget;
	GList            *list, *children;
	gchar            *name;

	g_return_if_fail (GLADE_IS_PROJECT (project));
	g_return_if_fail (G_IS_OBJECT      (object));

	/* We don't list placeholders */
	if (GLADE_IS_PLACEHOLDER (object)) 
		return;

	/* Only widgets accounted for in the catalog or widgets declared
	 * in the plugin with glade_widget_new_for_internal_child () are
	 * usefull in the project.
	 */
	if ((gwidget = glade_widget_get_from_gobject (object)) == NULL)
		return;

	if (glade_project_has_object (project, object))
	{
		/* FIXME: It's possible we need to notify the model iface if this 
		 * happens to make sure the hierarchy is the same, I dont know, this 
		 * happens when message dialogs with children are rebuilt but the 
		 * hierarchy still looks good afterwards. */
		return;
	}

	/* Create a name context for newly added toplevels... */
	if (!gwidget->parent)
	{
		TopLevelInfo *tinfo = g_new0 (TopLevelInfo, 1);
		tinfo->toplevel     = gwidget;
		tinfo->names        = glade_name_context_new ();
		project->priv->toplevels = g_list_prepend (project->priv->toplevels, tinfo);
	}

	/* Make sure we have an exclusive name first... */
	if (!glade_project_available_widget_name (project, gwidget, gwidget->name))
	{
		name = glade_project_new_widget_name (project, gwidget, gwidget->name);

		glade_widget_set_name (gwidget, name);

		g_free (name);
	}

	glade_project_reserve_widget_name (project, gwidget, gwidget->name);

	glade_widget_set_project (gwidget, (gpointer)project);
	gwidget->in_project = TRUE;
	g_object_ref_sink (gwidget);

	/* Be sure to update the lists before emitting signals */
	if (glade_widget_get_parent (gwidget) == NULL)
		project->priv->tree = g_list_append (project->priv->tree, object);

	project->priv->objects = g_list_prepend (project->priv->objects, object);

	glade_project_notify_row_inserted (project, gwidget);

	/* NOTE: Sensitive ordering here, we need to recurse after updating
	 * the tree model listeners (and update those listeners after our
	 * internal lists have been resolved), otherwise children are added
	 * before the parents (and the views dont like that).
	 */
	if ((children = glade_widget_get_children (gwidget)) != NULL)
	{
		for (list = children; list && list->data; list = list->next)
			glade_project_add_object
				(project, old_project, G_OBJECT (list->data));
		g_list_free (children);
	}

	/* Update user visible compatibility info */
	glade_project_verify_properties (gwidget);
	
	g_signal_emit (G_OBJECT (project),
		       glade_project_signals [ADD_WIDGET],
		       0, gwidget);
}

/**
 * glade_project_has_object:
 * @project: the #GladeProject the widget is added to
 * @object: the #GObject to search
 *
 * Returns: whether this object is in this project.
 */
gboolean
glade_project_has_object (GladeProject *project, GObject *object)
{
	GladeWidget *gwidget;

	g_return_val_if_fail (GLADE_IS_PROJECT (project), FALSE);
	g_return_val_if_fail (G_IS_OBJECT (object), FALSE);

	gwidget = glade_widget_get_from_gobject (object);
	g_return_val_if_fail (GLADE_IS_WIDGET (gwidget), FALSE);

	return (glade_widget_get_project (gwidget) == project && gwidget->in_project);
}

/**
 * glade_project_remove_object:
 * @project: a #GladeProject
 * @object: the #GObject to remove
 *
 * Removes @object from @project.
 *
 * Note that when removing the #GObject from the project we
 * don't change ->project in the associated #GladeWidget; this
 * way UNDO can work.
 */
void
glade_project_remove_object (GladeProject *project, GObject *object)
{
	GladeWidget   *gwidget;
	GList         *list, *children;
	
	g_return_if_fail (GLADE_IS_PROJECT (project));
	g_return_if_fail (G_IS_OBJECT      (object));

	if (!glade_project_has_object (project, object))
		return;

	if (GLADE_IS_PLACEHOLDER (object))
		return;

	if ((gwidget = glade_widget_get_from_gobject (object)) == NULL)
		return;

	/* Recurse and remove deepest children first */
	if ((children = 
	     glade_widget_get_children (gwidget)) != NULL)
	{
		for (list = children; list && list->data; list = list->next)
			glade_project_remove_object (project, G_OBJECT (list->data));
		g_list_free (children);
	}

	/* Notify views that the row is being deleted *before* deleting it */
	glade_project_notify_row_deleted (project, gwidget);

	/* Remove selection and release name from the name context */
	glade_project_selection_remove (project, object, TRUE);
	glade_project_release_widget_name (project, gwidget,
					   glade_widget_get_name (gwidget));

	g_signal_emit (G_OBJECT (project),
		       glade_project_signals [REMOVE_WIDGET],
		       0,
		       gwidget);

	/* Update internal data structure (remove from lists) */
	project->priv->tree    = g_list_remove (project->priv->tree, object);
	project->priv->objects = g_list_remove (project->priv->objects, object);

	/* Unset the project pointer on the GladeWidget */
	glade_widget_set_project (gwidget, NULL);
	gwidget->in_project = FALSE;


	glade_project_notify_row_has_child (project, gwidget, FALSE);
	g_object_unref (gwidget);

}

static void
adjust_naming_policy (GladeProject       *project, 
		      GladeNamingPolicy   policy)
{
	GList *list, *objects;
	GladeWidget *widget;

	/* Ref *all* objects */
	for (list = project->priv->objects; list; list = list->next)
	{
		widget = glade_widget_get_from_gobject (list->data);
		g_object_ref (widget);
	}

	/* Remove all toplevels (recursive operation) */
	objects = g_list_copy (project->priv->tree);
	for (list = objects; list; list = list->next)
		glade_project_remove_object (project, G_OBJECT (list->data));

	project->priv->naming_policy = policy;

	/* Put the toplevels back with the new policy (recursive operation) */
	for (list = objects; list; list = list->next)
		glade_project_add_object (project, project, G_OBJECT (list->data));
	g_list_free (objects);

	/* Unref them now */
	for (list = project->priv->objects; list; list = list->next)
	{
		widget = glade_widget_get_from_gobject (list->data);
		g_object_ref (widget);
	}
}

/*******************************************************************
                        Remaining stubs and api
 *******************************************************************/
static void
glade_project_set_target_version (GladeProject *project,
				  const gchar  *catalog,
				  guint16       major,
				  guint16       minor)
{
	GladeTargetableVersion *version;
	GSList *radios, *list;
	GtkWidget *radio;

	g_hash_table_insert (project->priv->target_versions_major,
			     g_strdup (catalog),
			     GINT_TO_POINTER ( ( int ) major));
	g_hash_table_insert (project->priv->target_versions_minor,
			     g_strdup (catalog),
			     GINT_TO_POINTER ( ( int ) minor));


	/* Update prefs dialog from here... */
	if (project->priv->target_radios && 
	    (radios = g_hash_table_lookup (project->priv->target_radios, catalog)) != NULL)
	{
		for (list = radios; list; list = list->next)
			g_signal_handlers_block_by_func (G_OBJECT (list->data), 
							 G_CALLBACK (target_button_clicked),
							 project);

		for (list = radios; list; list = list->next)
		{
			radio = list->data;

			version = g_object_get_data (G_OBJECT (radio), "version");
			if (version->major == major &&
			    version->minor == minor)
			{
				gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radio), TRUE);
				break;
			}
		}

		for (list = radios; list; list = list->next)
			g_signal_handlers_unblock_by_func (G_OBJECT (list->data), 
							   G_CALLBACK (target_button_clicked),
							   project);
	}

	glade_project_verify_project_for_ui (project);

	g_signal_emit (project, glade_project_signals [TARGETS_CHANGED], 0);
}

static void
glade_project_set_readonly (GladeProject *project, gboolean readonly)
{
	g_assert (GLADE_IS_PROJECT (project));
	
	if (project->priv->readonly != readonly)
	{
		project->priv->readonly = readonly;
		g_object_notify (G_OBJECT (project), "read-only");
	}
}                                                                                               


/**
 * glade_project_get_target_version:
 * @project: a #GladeProject
 * @catalog: the name of the catalog @project includes
 * @major: the return location for the target major version
 * @minor: the return location for the target minor version
 *
 * Fetches the target version of the @project for @catalog.
 *
 */
void
glade_project_get_target_version (GladeProject *project,
				  const gchar  *catalog,
				  gint         *major,
				  gint         *minor)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));
	g_return_if_fail (catalog && catalog[0]);
	g_return_if_fail (major && minor);

	*major = GPOINTER_TO_INT 
		(g_hash_table_lookup (project->priv->target_versions_major,
				      catalog));
	*minor = GPOINTER_TO_INT 
		(g_hash_table_lookup (project->priv->target_versions_minor,
				      catalog));
}

/**
 * glade_project_get_readonly:
 * @project: a #GladeProject
 *
 * Gets whether the project is read only or not
 *
 * Returns: TRUE if project is read only
 */
gboolean
glade_project_get_readonly (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), FALSE);

	return project->priv->readonly;
}


/**
 * glade_project_selection_changed:
 * @project: a #GladeProject
 *
 * Causes @project to emit a "selection_changed" signal.
 */
void
glade_project_selection_changed (GladeProject *project)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));
	g_signal_emit (G_OBJECT (project),
		       glade_project_signals [SELECTION_CHANGED],
		       0);
}

static void
glade_project_set_has_selection (GladeProject *project, gboolean has_selection)
{
	g_assert (GLADE_IS_PROJECT (project));

	if (project->priv->has_selection != has_selection)
	{
		project->priv->has_selection = has_selection;
		g_object_notify (G_OBJECT (project), "has-selection");
	}
}

/**
 * glade_project_get_has_selection:
 * @project: a #GladeProject
 *
 * Returns: whether @project currently has a selection
 */
gboolean
glade_project_get_has_selection (GladeProject *project)
{
	g_assert (GLADE_IS_PROJECT (project));

	return project->priv->has_selection;
}

/**
 * glade_project_is_selected:
 * @project: a #GladeProject
 * @object: a #GObject
 *
 * Returns: whether @object is in @project selection
 */
gboolean
glade_project_is_selected (GladeProject *project,
			   GObject      *object)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), FALSE);
	return (g_list_find (project->priv->selection, object)) != NULL;
}

/**
 * glade_project_selection_clear:
 * @project: a #GladeProject
 * @emit_signal: whether or not to emit a signal indication a selection change
 *
 * Clears @project's selection chain
 *
 * If @emit_signal is %TRUE, calls glade_project_selection_changed().
 */
void
glade_project_selection_clear (GladeProject *project, gboolean emit_signal)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));
	if (project->priv->selection == NULL)
		return;

	glade_util_clear_selection ();

	g_list_free (project->priv->selection);
	project->priv->selection = NULL;
	glade_project_set_has_selection (project, FALSE);

	if (emit_signal)
		glade_project_selection_changed (project);
}

/**
 * glade_project_selection_remove:
 * @project: a #GladeProject
 * @object:  a #GObject in @project
 * @emit_signal: whether or not to emit a signal 
 *               indicating a selection change
 *
 * Removes @object from the selection chain of @project
 *
 * If @emit_signal is %TRUE, calls glade_project_selection_changed().
 */
void
glade_project_selection_remove (GladeProject *project,
				GObject      *object,
				gboolean      emit_signal)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));
	g_return_if_fail (G_IS_OBJECT      (object));

	if (glade_project_is_selected (project, object))
	{
		if (GTK_IS_WIDGET (object))
			glade_util_remove_selection (GTK_WIDGET (object));
		project->priv->selection = g_list_remove (project->priv->selection, object);
		if (project->priv->selection == NULL)
			glade_project_set_has_selection (project, FALSE);
		if (emit_signal)
			glade_project_selection_changed (project);
	}
}

/**
 * glade_project_selection_add:
 * @project: a #GladeProject
 * @object:  a #GObject in @project
 * @emit_signal: whether or not to emit a signal indicating 
 *               a selection change
 *
 * Adds @object to the selection chain of @project
 *
 * If @emit_signal is %TRUE, calls glade_project_selection_changed().
 */
void
glade_project_selection_add (GladeProject *project,
			     GObject      *object,
			     gboolean      emit_signal)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));
	g_return_if_fail (G_IS_OBJECT      (object));
	g_return_if_fail (glade_project_has_object (project, object));

	if (glade_project_is_selected (project, object) == FALSE)
	{
		if (GTK_IS_WIDGET (object))
			glade_util_add_selection (GTK_WIDGET (object));
		if (project->priv->selection == NULL)
			glade_project_set_has_selection (project, TRUE);
		project->priv->selection = g_list_prepend (project->priv->selection, object);
		if (emit_signal)
			glade_project_selection_changed (project);
	}
}

/**
 * glade_project_selection_set:
 * @project: a #GladeProject
 * @object:  a #GObject in @project
 * @emit_signal: whether or not to emit a signal 
 *               indicating a selection change
 *
 * Set the selection in @project to @object
 *
 * If @emit_signal is %TRUE, calls glade_project_selection_changed().
 */
void
glade_project_selection_set (GladeProject *project,
			     GObject      *object,
			     gboolean      emit_signal)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));
	g_return_if_fail (G_IS_OBJECT      (object));
	g_return_if_fail (glade_project_has_object (project, object));

	if (project->priv->selection == NULL)
		glade_project_set_has_selection (project, TRUE);

	if (glade_project_is_selected (project, object) == FALSE ||
	    g_list_length (project->priv->selection) != 1)
	{
		glade_project_selection_clear (project, FALSE);
		glade_project_selection_add (project, object, emit_signal);
	}
}	

/**
 * glade_project_selection_get:
 * @project: a #GladeProject
 *
 * Returns: a #GList containing the #GtkWidget items currently selected in 
 *          @project
 */
GList *
glade_project_selection_get (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), NULL);

	return project->priv->selection;
}

/**
 * glade_project_required_libs:
 * @project: a #GladeProject
 *
 * Returns: a #GList of allocated strings which are the names
 * of the required catalogs for this project
 */
GList *
glade_project_required_libs (GladeProject *project)
{
	GList       *required = NULL, *l, *ll;
	GladeWidget *gwidget;
	gboolean     listed;

	for (l = project->priv->objects; l; l = l->next)
	{
		gchar *catalog = NULL;

		gwidget = glade_widget_get_from_gobject (l->data);
		g_assert (gwidget);

		g_object_get (gwidget->adaptor, "catalog", &catalog, NULL);

		if (catalog)
		{
			listed = FALSE;
			for (ll = required; ll; ll = ll->next)
				if (!strcmp ((gchar *)ll->data, catalog))
				{
					listed = TRUE;
					break;
				}

			if (!listed)
				required = g_list_prepend (required, catalog);
		}
	}

	/* Assume GTK+ here */
	if (!required)
		required = g_list_prepend (required, g_strdup ("gtk+"));

	for (l = project->priv->unknown_catalogs; l; l = g_list_next (l))
	{
		CatalogInfo *data = l->data;
		/* Keep position to make sure we do not create a diff when saving */
		required = g_list_insert (required, g_strdup (data->catalog), data->position);
	}

	return required;
}

/**
 * glade_project_undo:
 * @project: a #GladeProject
 * 
 * Undoes a #GladeCommand in this project.
 */
void
glade_project_undo (GladeProject *project)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));
	GLADE_PROJECT_GET_CLASS (project)->undo (project);
}

/**
 * glade_project_undo:
 * @project: a #GladeProject
 * 
 * Redoes a #GladeCommand in this project.
 */
void
glade_project_redo (GladeProject *project)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));
	GLADE_PROJECT_GET_CLASS (project)->redo (project);
}

/**
 * glade_project_next_undo_item:
 * @project: a #GladeProject
 * 
 * Gets the next undo item on @project's command stack.
 *
 * Returns: the #GladeCommand
 */
GladeCommand *
glade_project_next_undo_item (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), NULL);
	return GLADE_PROJECT_GET_CLASS (project)->next_undo_item (project);
}


/**
 * glade_project_next_redo_item:
 * @project: a #GladeProject
 * 
 * Gets the next redo item on @project's command stack.
 *
 * Returns: the #GladeCommand
 */
GladeCommand *
glade_project_next_redo_item (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), NULL);
	return GLADE_PROJECT_GET_CLASS (project)->next_redo_item (project);
}



/**
 * glade_project_push_undo:
 * @project: a #GladeProject
 * @cmd: the #GladeCommand
 * 
 * Pushes a newly created #GladeCommand onto @projects stack.
 */
void
glade_project_push_undo (GladeProject *project, GladeCommand *cmd)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));
	g_return_if_fail (GLADE_IS_COMMAND (cmd));

	GLADE_PROJECT_GET_CLASS (project)->push_undo (project, cmd);
}

static GList *
walk_command (GList *list, gboolean forward)
{
	GladeCommand *cmd = list->data;
	GladeCommand *next_cmd;

	if (forward)
		list = list->next;
	else
		list = list->prev;
	
	next_cmd = list ? list->data : NULL;
	
	while (list && next_cmd->group_id != 0 &&
	       next_cmd->group_id == cmd->group_id)
	{
		if (forward)
			list = list->next;
		else
			list = list->prev;

		if (list)
			next_cmd = list->data;
	}

	return list;
}

static void
undo_item_activated (GtkMenuItem  *item,
		     GladeProject *project)
{
	gint index, next_index;
	
	GladeCommand *cmd = g_object_get_data (G_OBJECT (item), "command-data");
	GladeCommand *next_cmd;

	index = g_list_index (project->priv->undo_stack, cmd);

	do
	{
		next_cmd = glade_project_next_undo_item (project);
		next_index = g_list_index (project->priv->undo_stack, next_cmd);

		glade_project_undo (project);
		
	} while (next_index > index);
}

static void
redo_item_activated (GtkMenuItem  *item,
		     GladeProject *project)
{
	gint index, next_index;
	
	GladeCommand *cmd = g_object_get_data (G_OBJECT (item), "command-data");
	GladeCommand *next_cmd;

	index = g_list_index (project->priv->undo_stack, cmd);

	do
	{
		next_cmd = glade_project_next_redo_item (project);
		next_index = g_list_index (project->priv->undo_stack, next_cmd);

		glade_project_redo (project);
		
	} while (next_index < index);
}


/**
 * glade_project_undo_items:
 * @project: A #GladeProject
 *
 * Creates a menu of the undo items in the project stack
 *
 * Returns: A newly created menu
 */
GtkWidget *
glade_project_undo_items (GladeProject *project)
{
	GtkWidget *menu = NULL;
	GtkWidget *item;
	GladeCommand *cmd;
	GList   *l;

	g_return_val_if_fail (project != NULL, NULL);

	for (l = project->priv->prev_redo_item; l; l = walk_command (l, FALSE))
	{
		cmd = l->data;

		if (!menu) menu = gtk_menu_new ();
		
		item = gtk_menu_item_new_with_label (cmd->description);
		gtk_widget_show (item);
		gtk_menu_shell_append (GTK_MENU_SHELL (menu), GTK_WIDGET (item));
		g_object_set_data (G_OBJECT (item), "command-data", cmd);

		g_signal_connect (G_OBJECT (item), "activate",
				  G_CALLBACK (undo_item_activated), project);

	}
	
	return menu;
}

/**
 * glade_project_redo_items:
 * @project: A #GladeProject
 *
 * Creates a menu of the undo items in the project stack
 *
 * Returns: A newly created menu
 */
GtkWidget *
glade_project_redo_items (GladeProject *project)
{
	GtkWidget *menu = NULL;
	GtkWidget *item;
	GladeCommand *cmd;
	GList   *l;

	g_return_val_if_fail (project != NULL, NULL);

	for (l = project->priv->prev_redo_item ?
		     project->priv->prev_redo_item->next :
		     project->priv->undo_stack;
	     l; l = walk_command (l, TRUE))
	{
		cmd = l->data;

		if (!menu) menu = gtk_menu_new ();
		
		item = gtk_menu_item_new_with_label (cmd->description);
		gtk_widget_show (item);
		gtk_menu_shell_append (GTK_MENU_SHELL (menu), GTK_WIDGET (item));
		g_object_set_data (G_OBJECT (item), "command-data", cmd);

		g_signal_connect (G_OBJECT (item), "activate",
				  G_CALLBACK (redo_item_activated), project);

	}
	
	return menu;
}


void
glade_project_reset_path (GladeProject *project)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));
	project->priv->path = (g_free (project->priv->path), NULL);
}

/**
 * glade_project_resource_fullpath:
 * @project: The #GladeProject.
 * @resource: The resource basename
 *
 * Project resource strings may be relative or fullpaths, but glade
 * always expects a copy in the glade file directory, this function
 * is used to make a local path to the file.
 *
 * Returns: A newly allocated string holding the 
 *          local path the the project resource.
 */
gchar *
glade_project_resource_fullpath (GladeProject *project,
				 const gchar  *resource)
{
	gchar *fullpath, *project_dir = NULL, *basename;

	g_return_val_if_fail (GLADE_IS_PROJECT (project), NULL);

	basename = g_path_get_basename (resource);

	if (project->priv->path == NULL)
		project_dir = g_get_current_dir ();
	else
		project_dir = g_path_get_dirname (project->priv->path);

	if (project->priv->resource_path)
	{
		if (g_path_is_absolute (project->priv->resource_path))
			fullpath = g_build_filename (project->priv->resource_path, basename, NULL);
		else
			fullpath = g_build_filename (project_dir, project->priv->resource_path, basename, NULL);
	}
	else
		fullpath = g_build_filename (project_dir, basename, NULL);
	
	g_free (project_dir);
	g_free (basename);
	
	return fullpath;
}

const gchar *
glade_project_get_path (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), NULL);

	return project->priv->path;
}

gchar *
glade_project_get_name (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), NULL);

	if (project->priv->path)
		return g_filename_display_basename (project->priv->path);
	else
		return g_strdup_printf (_("Unsaved %i"), project->priv->unsaved_number);
}

/**
 * glade_project_is_loading:
 * @project: A #GladeProject
 *
 * Returns: Whether the project is being loaded or not
 *       
 */
gboolean
glade_project_is_loading (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), FALSE);
	
	return project->priv->loading;
}

time_t
glade_project_get_file_mtime (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), 0);
	
	return project->priv->mtime;
}

/**
 * glade_projects_get_objects:
 * @project: a GladeProject
 *
 * Returns: List of all objects in this project
 */
const GList *
glade_project_get_objects (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), NULL);

	return project->priv->objects;
}

/** 
 * glade_project_get_modified:
 * @project: a #GladeProject
 *
 * Get's whether the project has been modified since it was last saved.
 *
 * Returns: #TRUE if the project has been modified since it was last saved
 */ 
gboolean
glade_project_get_modified (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), FALSE);

	return project->priv->modified;
}

void
glade_project_set_naming_policy (GladeProject       *project,
				 GladeNamingPolicy   policy)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));

	if (project->priv->naming_policy != policy)
	{
		adjust_naming_policy (project, policy);

		/* Update the toggle button in the prefs dialog here: */
		g_signal_handlers_block_by_func (project->priv->project_wide_radio,
						 G_CALLBACK (policy_project_wide_button_clicked), project);
		g_signal_handlers_block_by_func (project->priv->toplevel_contextual_radio,
						 G_CALLBACK (policy_toplevel_contextual_button_clicked), project);

		if (policy == GLADE_POLICY_PROJECT_WIDE)
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project->priv->project_wide_radio), TRUE);
		else
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project->priv->toplevel_contextual_radio), TRUE);

		g_signal_handlers_unblock_by_func (project->priv->project_wide_radio,
						   G_CALLBACK (policy_project_wide_button_clicked), project);
		g_signal_handlers_unblock_by_func (project->priv->toplevel_contextual_radio,
						   G_CALLBACK (policy_toplevel_contextual_button_clicked), project);

	}


}

GladeNamingPolicy
glade_project_get_naming_policy (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), -1);

	return project->priv->naming_policy;
}


/** 
 * glade_project_set_format:
 * @project: a #GladeProject
 * @format: the #GladeProjectFormat
 *
 * Sets @project format to @format, used internally to set the actual format
 * state; note that conversions should be done through the glade-command api.
 */ 
void
glade_project_set_format (GladeProject *project, GladeProjectFormat format)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));

	if (project->priv->format != format)
	{
		project->priv->format = format; 
		g_object_notify (G_OBJECT (project), "format");
		glade_project_verify_project_for_ui (project);

		/* Update the toggle button in the prefs dialog here: */
		g_signal_handlers_block_by_func (project->priv->glade_radio,
						 G_CALLBACK (format_libglade_button_toggled), project);
		g_signal_handlers_block_by_func (project->priv->builder_radio,
						 G_CALLBACK (format_builder_button_toggled), project);

		if (format == GLADE_PROJECT_FORMAT_GTKBUILDER)
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project->priv->builder_radio), TRUE);
		else
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project->priv->glade_radio), TRUE);

		g_signal_handlers_unblock_by_func (project->priv->glade_radio,
						   G_CALLBACK (format_libglade_button_toggled), project);
		g_signal_handlers_unblock_by_func (project->priv->builder_radio,
						   G_CALLBACK (format_builder_button_toggled), project);

	}
}

GladeProjectFormat
glade_project_get_format (GladeProject *project)
{
	g_return_val_if_fail (GLADE_IS_PROJECT (project), -1);

	return project->priv->format;
}


static void
format_libglade_button_toggled (GtkWidget *widget,
				GladeProject *project)
{
	glade_command_set_project_format (project, GLADE_PROJECT_FORMAT_LIBGLADE);
}

static void
format_builder_button_toggled (GtkWidget *widget,
			       GladeProject *project)
{
	glade_command_set_project_format (project, GLADE_PROJECT_FORMAT_GTKBUILDER);
}

static gint
count_objects_with_name (GladeProject *project,
			 const gchar  *name)
{
	GList *l;
	GladeWidget *widget;
	gint count = 0;

	for (l = project->priv->objects; l; l = l->next)
       	{
		widget = glade_widget_get_from_gobject (l->data);
		if (!strcmp (widget->name, name))
			count++;
       	}
	return count;
}

static void
policy_project_wide_button_clicked (GtkWidget *widget,
				    GladeProject *project)
{
	GList *l, *objects;
	GladeWidget *gwidget;

	/* The formatting here is only to avoid a string change in a stable series... */
	gchar *prj_name = glade_project_get_name (project);
	glade_command_push_group (_("Setting %s to use a %s naming policy"),
				  prj_name, "project wide");
	g_free (prj_name);

	/* Uniquify names here before switching policy (so names undo) */
	objects = g_list_copy (project->priv->objects);
	for (l = g_list_last (objects); l; l = l->prev)
       	{
		gwidget = glade_widget_get_from_gobject (l->data);

		if (count_objects_with_name (project, gwidget->name) > 1)
       		{
			GladeNameContext *context = 
				name_context_by_widget (project, gwidget);
			gchar *new_name;

			if (context)
				new_name = glade_name_context_dual_new_name 
					(context, project->priv->toplevel_names, gwidget->name);
			else
				new_name = glade_name_context_new_name (project->priv->toplevel_names, gwidget->name);

			glade_command_set_name (gwidget, new_name);	
		}
       	}
	g_list_free (objects);

	glade_command_set_project_naming_policy (project, GLADE_POLICY_PROJECT_WIDE);

	glade_command_pop_group ();
}

static void
policy_toplevel_contextual_button_clicked (GtkWidget *widget,
					   GladeProject *project)
{
	glade_command_set_project_naming_policy (project, GLADE_POLICY_TOPLEVEL_CONTEXTUAL);
}

static void
target_button_clicked (GtkWidget *widget,
		       GladeProject *project)
{
	GladeTargetableVersion *version = 
		g_object_get_data (G_OBJECT (widget), "version");
	gchar *catalog = 
		g_object_get_data (G_OBJECT (widget), "catalog");

	glade_project_set_target_version (project,
					  catalog,
					  version->major,
					  version->minor);
}

static void
verify_clicked (GtkWidget    *button,
		GladeProject *project)
{
	if (glade_project_verify (project, FALSE))
	{
		gchar *name = glade_project_get_name (project);
		glade_util_ui_message (glade_app_get_window(),
				       GLADE_UI_INFO, NULL,
				       _("Project %s has no deprecated widgets "
					 "or version mismatches."),
				       name);
		g_free (name);
	}
}

static void
resource_default_toggled (GtkWidget *widget,
			  GladeProject *project)
{
	if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget)))
		return;

	glade_project_set_resource_path (project, NULL);
	gtk_widget_set_sensitive (project->priv->relative_path_entry, FALSE);
	gtk_widget_set_sensitive (project->priv->full_path_button, FALSE);
}


static void
resource_relative_toggled (GtkWidget *widget,
			   GladeProject *project)
{
	if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget)))
		return;

	gtk_widget_set_sensitive (project->priv->relative_path_entry, TRUE);
	gtk_widget_set_sensitive (project->priv->full_path_button, FALSE);
}

static void
resource_fullpath_toggled (GtkWidget *widget,
			   GladeProject *project)
{
	if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget)))
		return;

	gtk_widget_set_sensitive (project->priv->relative_path_entry, FALSE);
	gtk_widget_set_sensitive (project->priv->full_path_button, TRUE);
}

static void
resource_path_activated (GtkEntry *entry,
			 GladeProject *project)
{
	const gchar *text = gtk_entry_get_text (entry);

	glade_project_set_resource_path (project, text ? g_strdup (text) : NULL);
}


static void
resource_full_path_set (GtkFileChooserButton *button,
			GladeProject *project)
{
	gchar *text = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (button));

	glade_project_set_resource_path (project, text);
}


static void
update_prefs_for_resource_path (GladeProject *project)
{
	gtk_widget_set_sensitive (project->priv->full_path_button, FALSE);
	gtk_widget_set_sensitive (project->priv->relative_path_entry, FALSE);


	g_signal_handlers_block_by_func (project->priv->resource_default_radio,
					 G_CALLBACK (resource_default_toggled), project);
	g_signal_handlers_block_by_func (project->priv->resource_relative_radio,
					 G_CALLBACK (resource_relative_toggled), project);
	g_signal_handlers_block_by_func (project->priv->resource_fullpath_radio,
					 G_CALLBACK (resource_fullpath_toggled), project);
	g_signal_handlers_block_by_func (project->priv->relative_path_entry,
					 G_CALLBACK (resource_path_activated), project);

	if (project->priv->resource_path == NULL)
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project->priv->resource_default_radio), TRUE);
	else if (g_path_is_absolute (project->priv->resource_path))
	{
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project->priv->resource_fullpath_radio), TRUE);
		gtk_widget_set_sensitive (project->priv->full_path_button, TRUE);
	}
	else
	{
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project->priv->resource_relative_radio), TRUE);
		gtk_widget_set_sensitive (project->priv->relative_path_entry, TRUE);
	}

	gtk_entry_set_text (GTK_ENTRY (project->priv->relative_path_entry),
			    project->priv->resource_path ? project->priv->resource_path : "");

	g_signal_handlers_unblock_by_func (project->priv->resource_default_radio,
					   G_CALLBACK (resource_default_toggled), project);
	g_signal_handlers_unblock_by_func (project->priv->resource_relative_radio,
					   G_CALLBACK (resource_relative_toggled), project);
	g_signal_handlers_unblock_by_func (project->priv->resource_fullpath_radio,
					   G_CALLBACK (resource_fullpath_toggled), project);
	g_signal_handlers_unblock_by_func (project->priv->relative_path_entry,
					   G_CALLBACK (resource_path_activated), project);
}


static GtkWidget *
glade_project_build_prefs_box (GladeProject *project)
{
	GtkWidget *main_box, *button;
	GtkWidget *vbox, *hbox, *frame;
	GtkWidget *target_radio, *active_radio;
	GtkWidget *label, *alignment;
	GList     *list, *targets;
	gchar     *string;
	GtkWidget *main_frame, *main_alignment;
	GtkSizeGroup *sizegroup1 = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL),
		*sizegroup2 = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL),
		*sizegroup3 = gtk_size_group_new (GTK_SIZE_GROUP_VERTICAL);

	main_frame = gtk_frame_new (NULL);
	gtk_frame_set_shadow_type (GTK_FRAME (main_frame), GTK_SHADOW_NONE);
	main_alignment = gtk_alignment_new (0.5F, 0.5F, 0.8F, 0.8F);
	main_box = gtk_vbox_new (FALSE, 0);

	gtk_alignment_set_padding (GTK_ALIGNMENT (main_alignment), 0, 0, 4, 0);

	gtk_container_add (GTK_CONTAINER (main_alignment), main_box);
	gtk_container_add (GTK_CONTAINER (main_frame), main_alignment);


	/* Project format */
	string = g_strdup_printf ("<b>%s</b>", _("Project file format:"));
	frame = gtk_frame_new (NULL);
	hbox = gtk_hbox_new (FALSE, 0);
	alignment = gtk_alignment_new (0.5F, 0.5F, 0.8F, 0.8F);

	gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 8, 0, 12, 0);

	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_NONE);

	label = gtk_label_new (string);
	g_free (string);
	gtk_label_set_use_markup (GTK_LABEL (label), TRUE);

	project->priv->glade_radio = gtk_radio_button_new_with_label (NULL, "Libglade");
	project->priv->builder_radio = gtk_radio_button_new_with_label_from_widget
		(GTK_RADIO_BUTTON (project->priv->glade_radio), "GtkBuilder");

	gtk_size_group_add_widget (sizegroup1, project->priv->builder_radio);
	gtk_size_group_add_widget (sizegroup2, project->priv->glade_radio);

	gtk_frame_set_label_widget (GTK_FRAME (frame), label);
	gtk_container_add (GTK_CONTAINER (alignment), hbox);
	gtk_container_add (GTK_CONTAINER (frame), alignment);

	gtk_box_pack_start (GTK_BOX (hbox), project->priv->builder_radio, TRUE, TRUE, 2);
	gtk_box_pack_start (GTK_BOX (hbox), project->priv->glade_radio, TRUE, TRUE, 2);

	gtk_box_pack_start (GTK_BOX (main_box), frame, TRUE, TRUE, 2);


	if (glade_project_get_format (project) == GLADE_PROJECT_FORMAT_GTKBUILDER)
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project->priv->builder_radio), TRUE);
	else
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project->priv->glade_radio), TRUE);

	g_signal_connect (G_OBJECT (project->priv->glade_radio), "toggled",
			  G_CALLBACK (format_libglade_button_toggled), project);

	g_signal_connect (G_OBJECT (project->priv->builder_radio), "toggled",
			  G_CALLBACK (format_builder_button_toggled), project);


	/* Naming policy format */
	string = g_strdup_printf ("<b>%s</b>", _("Object names are unique:"));
	frame = gtk_frame_new (NULL);
	hbox = gtk_hbox_new (FALSE, 0);
	alignment = gtk_alignment_new (0.5F, 0.5F, 0.8F, 0.8F);

	gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 8, 0, 12, 0);

	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_NONE);

	label = gtk_label_new (string);
	g_free (string);
	gtk_label_set_use_markup (GTK_LABEL (label), TRUE);

	project->priv->project_wide_radio = gtk_radio_button_new_with_label (NULL, _("within the project"));
	project->priv->toplevel_contextual_radio = gtk_radio_button_new_with_label_from_widget
		(GTK_RADIO_BUTTON (project->priv->project_wide_radio), _("inside toplevels"));

	gtk_size_group_add_widget (sizegroup1, project->priv->project_wide_radio);
	gtk_size_group_add_widget (sizegroup2, project->priv->toplevel_contextual_radio);

	gtk_box_pack_start (GTK_BOX (hbox), project->priv->project_wide_radio, TRUE, TRUE, 2);
	gtk_box_pack_start (GTK_BOX (hbox), project->priv->toplevel_contextual_radio, TRUE, TRUE, 2);

	gtk_frame_set_label_widget (GTK_FRAME (frame), label);
	gtk_container_add (GTK_CONTAINER (alignment), hbox);
	gtk_container_add (GTK_CONTAINER (frame), alignment);

	gtk_box_pack_start (GTK_BOX (main_box), frame, TRUE, TRUE, 6);

	if (project->priv->naming_policy == GLADE_POLICY_PROJECT_WIDE)
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project->priv->project_wide_radio), TRUE);
	else
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project->priv->toplevel_contextual_radio), TRUE);

	g_signal_connect (G_OBJECT (project->priv->project_wide_radio), "clicked",
			  G_CALLBACK (policy_project_wide_button_clicked), project);

	g_signal_connect (G_OBJECT (project->priv->toplevel_contextual_radio), "clicked",
			  G_CALLBACK (policy_toplevel_contextual_button_clicked), project);


	/* Resource path */
	string    = g_strdup_printf ("<b>%s</b>", _("Image resources are loaded locally:"));
	frame     = gtk_frame_new (NULL);
	vbox      = gtk_vbox_new (FALSE, 0);
	alignment = gtk_alignment_new (0.5F, 0.5F, 0.8F, 0.8F);
	gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 8, 0, 12, 0);
	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_NONE);
	label = gtk_label_new (string);
	g_free (string);
	gtk_label_set_use_markup (GTK_LABEL (label), TRUE);

	gtk_box_pack_start (GTK_BOX (main_box), frame, TRUE, TRUE, 2);
	gtk_frame_set_label_widget (GTK_FRAME (frame), label);
	gtk_container_add (GTK_CONTAINER (frame), alignment);
	gtk_container_add (GTK_CONTAINER (alignment), vbox);
	
	/* Project directory... */
	project->priv->resource_default_radio = gtk_radio_button_new_with_label (NULL, _("From the project directory"));
	gtk_box_pack_start (GTK_BOX (vbox), project->priv->resource_default_radio, FALSE, FALSE, 0);
	gtk_size_group_add_widget (sizegroup3, project->priv->resource_default_radio);

	/* Project relative directory... */
	hbox = gtk_hbox_new (FALSE, 0);
	project->priv->resource_relative_radio = gtk_radio_button_new_with_label_from_widget
		(GTK_RADIO_BUTTON (project->priv->resource_default_radio), _("From a project relative directory"));

	gtk_box_pack_start (GTK_BOX (hbox), project->priv->resource_relative_radio, TRUE, TRUE, 0);
	project->priv->relative_path_entry = gtk_entry_new ();
	gtk_box_pack_start (GTK_BOX (hbox), project->priv->relative_path_entry, FALSE, TRUE, 2);
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
	gtk_size_group_add_widget (sizegroup3, hbox);


	/* fullpath directory... */
	hbox = gtk_hbox_new (FALSE, 0);
	project->priv->resource_fullpath_radio = gtk_radio_button_new_with_label_from_widget
		(GTK_RADIO_BUTTON (project->priv->resource_default_radio), _("From this directory"));
	gtk_box_pack_start (GTK_BOX (hbox), project->priv->resource_fullpath_radio, TRUE, TRUE, 0);
	
	project->priv->full_path_button = gtk_file_chooser_button_new (_("Choose a path to load image resources"),
								       GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER);
	gtk_box_pack_start (GTK_BOX (hbox), project->priv->full_path_button, FALSE, TRUE, 2);
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
	gtk_size_group_add_widget (sizegroup3, hbox);

	update_prefs_for_resource_path (project);

	g_signal_connect (G_OBJECT (project->priv->resource_default_radio), "toggled",
			  G_CALLBACK (resource_default_toggled), project);
	g_signal_connect (G_OBJECT (project->priv->resource_relative_radio), "toggled",
			  G_CALLBACK (resource_relative_toggled), project);
	g_signal_connect (G_OBJECT (project->priv->resource_fullpath_radio), "toggled",
			  G_CALLBACK (resource_fullpath_toggled), project);

	g_signal_connect (G_OBJECT (project->priv->relative_path_entry), "activate",
			  G_CALLBACK (resource_path_activated), project);
	g_signal_connect (G_OBJECT (project->priv->full_path_button), "file-set",
			  G_CALLBACK (resource_full_path_set), project);

	/* Target versions */
	string = g_strdup_printf ("<b>%s</b>", _("Toolkit versions required:"));
	frame = gtk_frame_new (NULL);
	vbox = gtk_vbox_new (FALSE, 0);
	alignment = gtk_alignment_new (0.5F, 0.5F, 1.0F, 1.0F);
	
	gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 8, 0, 12, 0);
	
	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_NONE);

	label = gtk_label_new (string);
	g_free (string);
	gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
	
	gtk_frame_set_label_widget (GTK_FRAME (frame), label);
	gtk_container_add (GTK_CONTAINER (alignment), vbox);
	gtk_container_add (GTK_CONTAINER (frame), alignment);
	
	gtk_box_pack_start (GTK_BOX (main_box), frame, TRUE, TRUE, 6);

	/* Add stuff to vbox */
	for (list = glade_app_get_catalogs (); list; list = list->next)
	{
		GladeCatalog *catalog = list->data;
		gint          minor, major;

		/* Skip if theres only one option */
		if (g_list_length (glade_catalog_get_targets (catalog)) <= 1)
			continue;

		glade_project_get_target_version (project,
						  glade_catalog_get_name (catalog),
						  &major,
						  &minor);

		/* Special case to mark GTK+ in upper case */
		if (strcmp (glade_catalog_get_name (catalog), "gtk+") == 0)
			label = gtk_label_new ("GTK+");
		else
			label = gtk_label_new (glade_catalog_get_name (catalog));
		gtk_misc_set_alignment (GTK_MISC (label), 0.0F, 0.5F);
		
		gtk_box_pack_start (GTK_BOX (vbox), label, TRUE, TRUE, 2);
		hbox = gtk_hbox_new (FALSE, 0);

		active_radio = NULL;
		target_radio = NULL;
		
		for (targets = glade_catalog_get_targets (catalog); 
		     targets; targets = targets->next)
		{
			GladeTargetableVersion *version = targets->data;
			gchar     *name = g_strdup_printf ("%d.%d", 
							   version->major,
							   version->minor);

			if (!target_radio)
				target_radio = gtk_radio_button_new_with_label (NULL, name);
			else
				target_radio = 
					gtk_radio_button_new_with_label_from_widget 
					(GTK_RADIO_BUTTON (target_radio), name);
			g_free (name);

			g_signal_connect (G_OBJECT (target_radio), "clicked",
					  G_CALLBACK (target_button_clicked), project);

			g_object_set_data (G_OBJECT (target_radio), "version", version);
			g_object_set_data (G_OBJECT (target_radio), "catalog", 
					   (gchar *)glade_catalog_get_name (catalog));

			gtk_box_pack_end (GTK_BOX (hbox), target_radio, TRUE, TRUE, 2);

			if (major == version->major &&
			    minor == version->minor)
				active_radio = target_radio;
			
		}

		if (active_radio)
		{
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (active_radio), TRUE);
			g_hash_table_insert (project->priv->target_radios, 
					     g_strdup (glade_catalog_get_name (catalog)),
					     gtk_radio_button_get_group (GTK_RADIO_BUTTON (active_radio)));
		}
		else 
			g_warning ("Corrupt catalog versions");

		gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 2);
	}

	/* Run verify */
	hbox = gtk_hbox_new (FALSE, 2);
	button = gtk_button_new_from_stock (GTK_STOCK_EXECUTE);
	g_signal_connect (G_OBJECT (button), "clicked", 
			  G_CALLBACK (verify_clicked), project);
	
	label = gtk_label_new (_("Verify versions and deprecations:"));

	gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 4);
	gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 4);
	
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 4);

	gtk_widget_show_all (main_frame);

	return main_frame;
}

static GtkWidget *
glade_project_build_prefs_dialog (GladeProject *project)
{
	GtkWidget *widget, *dialog;
	gchar     *title, *name;

	name = glade_project_get_name (project);
	title = g_strdup_printf (_("%s preferences"), name);

	dialog = gtk_dialog_new_with_buttons (title,
					      GTK_WINDOW (glade_app_get_window ()),
					      GTK_DIALOG_DESTROY_WITH_PARENT,
					      GTK_STOCK_CLOSE,
					      GTK_RESPONSE_ACCEPT,
					      NULL);
	g_free (title);
	g_free (name);

	widget = glade_project_build_prefs_box (project);
	gtk_box_pack_end (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG (dialog))),
			  widget, TRUE, TRUE, 2);

	gtk_container_set_border_width (GTK_CONTAINER (dialog), 5);
	gtk_box_set_spacing (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG (dialog))), 2);

	gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);

	/* HIG spacings */
	gtk_container_set_border_width (GTK_CONTAINER (dialog), 5);
	gtk_box_set_spacing (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG (dialog))), 2); /* 2 * 5 + 2 = 12 */
	gtk_container_set_border_width (GTK_CONTAINER (gtk_dialog_get_action_area (GTK_DIALOG (dialog))), 5);
	gtk_box_set_spacing (GTK_BOX (gtk_dialog_get_action_area (GTK_DIALOG (dialog))), 6);


	/* Were explicitly destroying it anyway */
	g_signal_connect (G_OBJECT (dialog), "delete-event",
			  G_CALLBACK (gtk_widget_hide_on_delete), NULL);

	/* Only one action, used to "close" the dialog */
	g_signal_connect (G_OBJECT (dialog), "response",
			  G_CALLBACK (gtk_widget_hide), NULL);

	return dialog;
}

/**
 * glade_project_preferences:
 * @project: A #GladeProject
 *
 * Runs a preferences dialog for @project.
 */
void
glade_project_preferences (GladeProject *project)
{
	g_return_if_fail (GLADE_IS_PROJECT (project));

	gtk_window_present (GTK_WINDOW (project->priv->prefs_dialog));
}


gchar *
glade_project_display_dependencies (GladeProject *project)
{
	GList *catalogs, *l;
	GString *string;

	g_return_val_if_fail (GLADE_IS_PROJECT (project), NULL);

	string = g_string_new ("");

	catalogs = glade_project_required_libs (project);
	for (l = catalogs; l; l = l->next)
	{
		gchar *catalog = l->data;
		gint   major = 0, minor = 0;

		glade_project_get_target_version (project, catalog, &major, &minor);

		if (l != catalogs)
			g_string_append (string, ", ");

		/* Capitalize GTK+ */
		if (strcmp (catalog, "gtk+") == 0)
			g_string_append_printf (string, "GTK+ >= %d.%d", major, minor);
		else if (major && minor)
			g_string_append_printf (string, "%s >= %d.%d", catalog, major, minor);
		else
			g_string_append_printf (string, "%s", catalog);

		g_free (catalog);
	}
	g_list_free (catalogs);

	return g_string_free (string, FALSE);
}

/* GtkTreeModel implementation */
static GObject *
glade_project_nth_child (GladeProject *project, 
			 GladeWidget  *parent, 
			 gint          nth)
{
  GList   *children, *list;
  GObject *child = NULL;
  gint     i;

  children = glade_widget_get_children (parent);

  for (list = children, i = 0; list; list = list->next)
    {
      child = list->data;

      if (!glade_project_has_object (project, child))
	continue;

      if (i == nth)
	break;

      child = NULL;
      i++;
    }

  g_list_free (children);

  return child;
}

static gint
glade_project_child_position (GladeProject *project, 
			      GladeWidget  *parent, 
			      GObject      *child)
{
  GList   *children, *list;
  GObject *iter;
  gint     i, position = -1;

  children = glade_widget_get_children (parent);

  for (list = children, i = 0; list; list = list->next)
    {
      iter = list->data;

      if (!glade_project_has_object (project, iter))
	continue;

      if (iter == child)
	{
	  position = i;
	  break;
	}
      i++;
    }

  g_list_free (children);

  return position;
}

static gint
glade_project_count_children (GladeProject *project, 
			      GladeWidget  *parent)
{
  GList   *children, *list;
  GObject *iter;
  gint     i;

  children = glade_widget_get_children (parent);

  for (list = children, i = 0; list; list = list->next)
    {
      iter = list->data;

      if (!glade_project_has_object (project, iter))
	continue;
      i++;
    }

  g_list_free (children);

  return i;
}

static void
glade_project_model_get_iter_for_object (GladeProject* project,
                                         GObject* object,
                                         GtkTreeIter* iter)
{
	g_assert (object);

	iter->stamp     = project->priv->stamp;
	iter->user_data = object;
}

static GtkTreeModelFlags
glade_project_model_get_flags (GtkTreeModel* model)
{
	return 0;
}

static gint
glade_project_model_get_n_columns (GtkTreeModel* model)
{
	return GLADE_PROJECT_MODEL_N_COLUMNS;
}

static GType
glade_project_model_get_column_type (GtkTreeModel* model,
                                     gint column)
{
	switch (column)
	{
	case GLADE_PROJECT_MODEL_COLUMN_ICON_NAME:
		return G_TYPE_STRING;
	case GLADE_PROJECT_MODEL_COLUMN_NAME:
		return G_TYPE_STRING;
	case GLADE_PROJECT_MODEL_COLUMN_TYPE_NAME:
		return G_TYPE_STRING;
	case GLADE_PROJECT_MODEL_COLUMN_OBJECT:
		return G_TYPE_OBJECT;
	case GLADE_PROJECT_MODEL_COLUMN_MISC:
		return G_TYPE_STRING;
	default:
		g_assert_not_reached();
		return G_TYPE_NONE;
	}
}

static gboolean
glade_project_model_get_iter (GtkTreeModel *model,
                              GtkTreeIter  *iter, 
			      GtkTreePath  *path)
{
  GladeProject *project = GLADE_PROJECT (model);
  gint         *indices = gtk_tree_path_get_indices (path);
  gint          depth = gtk_tree_path_get_depth (path);
  GladeWidget  *widget;
  GObject      *object;
  gint          i;
  GList        *parent;

  if ((parent = g_list_nth (project->priv->tree, indices[0])) != NULL)
    {
      object = parent->data;
      widget = glade_widget_get_from_gobject (object);
    }
  else
    {
      iter->stamp = 0;
      iter->user_data = NULL;
      return FALSE;
    }

  for (i = 1; i < depth; i++)
    {
      object = glade_project_nth_child (project, widget, indices[i]);

      if (!object)
        {
          iter->stamp = 0;
          iter->user_data = NULL;
          return FALSE;
        }

      widget = glade_widget_get_from_gobject (object);
    }

  if (object)
    {
      glade_project_model_get_iter_for_object (project, object, iter);
      return TRUE;
    }
  else
    {
      iter->stamp = 0;
      iter->user_data = NULL;
      return FALSE;
    }
}

static GtkTreePath *
glade_project_model_get_path (GtkTreeModel * model, GtkTreeIter * iter)
{
  GladeProject *project = GLADE_PROJECT (model);
  GtkTreePath  *path;
  GObject      *object;
  GladeWidget  *widget;
  GladeWidget  *toplevel;
  GladeWidget  *parent;
  GList        *top;

  g_return_val_if_fail (VALID_ITER (project, iter), NULL);

  object   = iter->user_data;
  widget   = glade_widget_get_from_gobject (object);
  toplevel = glade_widget_get_toplevel (widget);
  parent   = widget;

  path = gtk_tree_path_new ();

  while ((parent = glade_widget_get_parent (widget)) != NULL)
    {
      gint position;

      if ((position = 
	   glade_project_child_position (project, parent, 
					 glade_widget_get_object (widget))) < 0)
        gtk_tree_path_prepend_index (path, 0);
      else
        gtk_tree_path_prepend_index (path, position);

      widget = parent;
    }

  /* Get the index for the top-level list */
  top = g_list_find (project->priv->tree, glade_widget_get_object (toplevel));

  /* While the project is disposing widgets are unparented and sometimes no longer in the tree */
  if (top)
    gtk_tree_path_prepend_index (path, g_list_position (project->priv->tree, top));
  else
    gtk_tree_path_prepend_index (path, 0);
  
  return path;
}

static void
glade_project_model_get_value (GtkTreeModel* model,
                               GtkTreeIter* iter,
                               gint column,
                               GValue* value)
{
	GObject* object;
	GladeWidget* widget;
	gchar *str = NULL, *child_type;

	g_return_if_fail (VALID_ITER (model, iter));

	object = iter->user_data;
	widget = glade_widget_get_from_gobject (object);

	value = g_value_init (value, 
	                      glade_project_model_get_column_type (model, column));
	
	switch (column)
	{
	case GLADE_PROJECT_MODEL_COLUMN_ICON_NAME:
		g_object_get (widget->adaptor, "icon-name", &str, NULL);
		g_value_take_string (value, str);
		break;
	case GLADE_PROJECT_MODEL_COLUMN_NAME:
		g_value_set_string (value,
				    glade_widget_get_name (widget));
		break;
	case GLADE_PROJECT_MODEL_COLUMN_TYPE_NAME:
		g_value_set_static_string (value,
					   G_OBJECT_TYPE_NAME(object));
		break;
	case GLADE_PROJECT_MODEL_COLUMN_OBJECT:
		g_value_set_object (value, object);
		break;
	case GLADE_PROJECT_MODEL_COLUMN_MISC:
		/* special child type / internal child */
		if (glade_widget_get_internal (widget) != NULL)
			str = g_strdup_printf (_("(internal %s)"),  
					       glade_widget_get_internal (widget));
		else if ((child_type = g_object_get_data (glade_widget_get_object (widget),
							  "special-child-type")) != NULL)
			str = g_strdup_printf (_("(%s child)"), child_type);
		
		g_value_take_string (value, str);
		break;
	default:
		g_assert_not_reached();
	}
}

static gboolean
glade_project_model_iter_next (GtkTreeModel * model, GtkTreeIter * iter)
{
	GladeProject *project = GLADE_PROJECT (model);
	GObject *object = iter->user_data;
	GladeWidget *widget;
	GladeWidget *parent;
	GList *children;
	GList *child;
	GList *next;
	gboolean retval = FALSE;

	g_return_val_if_fail (VALID_ITER (project, iter), FALSE);

	widget = glade_widget_get_from_gobject (object);
	parent = glade_widget_get_parent (widget);

	if (parent)
	{
		children = glade_widget_get_children (parent);
	}
	else
	{
		children = project->priv->tree;
	}

	child = g_list_find (children, object);
	if (child)
	{
	    /* Get the next child that is actually in the project */
		for (next = child->next; next; next = next->next)
		{
			GObject *object = next->data;

			if (glade_project_has_object (project, object))
				break;
		}

		if (next)
		{
			glade_project_model_get_iter_for_object (project, next->data, iter);
			retval = TRUE;
		}
	}
	if (children != project->priv->tree)
		g_list_free (children);

	return retval;
}

static gboolean
glade_project_model_iter_has_child (GtkTreeModel* model,
                                    GtkTreeIter* iter)
{
	GladeProject *project = GLADE_PROJECT (model);
	GladeWidget  *widget;

	g_return_val_if_fail (VALID_ITER (model, iter), FALSE);

	widget = glade_widget_get_from_gobject (iter->user_data);

	if (glade_project_count_children (project, widget) > 0)
		return TRUE;

	return FALSE;
}

static gint
glade_project_model_iter_n_children (GtkTreeModel* model,
                                     GtkTreeIter* iter)
{
	GladeProject* project = GLADE_PROJECT (model);

	g_return_val_if_fail (iter == NULL || VALID_ITER (project, iter), 0);

	if (iter)
	{
		GladeWidget *widget = glade_widget_get_from_gobject (iter->user_data);

		return glade_project_count_children (project, widget);
	}

	return g_list_length (project->priv->tree);
}

static gboolean
glade_project_model_iter_nth_child (GtkTreeModel * model,
                                    GtkTreeIter * iter,
                                    GtkTreeIter * parent, gint nth)
{
  GladeProject *project = GLADE_PROJECT (model);
  GObject      *object = NULL;

  g_return_val_if_fail (parent == NULL || VALID_ITER (project, parent), FALSE);

  if (parent != NULL)
    {
      GObject     *obj    = parent->user_data;
      GladeWidget *widget = glade_widget_get_from_gobject (obj);

      object = glade_project_nth_child (project, widget, nth);
    }
  else if (project->priv->tree)
    {
      GList *child = 
	g_list_nth (project->priv->tree, nth);

      if (child)
	object = child->data;
    }

  if (object)
    {
      glade_project_model_get_iter_for_object (project, object, iter);
      return TRUE;
    }

  iter->stamp     = 0;
  iter->user_data = NULL;

  return FALSE;
}

static gboolean
glade_project_model_iter_children (GtkTreeModel * model,
                                   GtkTreeIter * iter, GtkTreeIter * parent)
{
  GladeProject *project = GLADE_PROJECT (model);
  GObject      *object  = NULL;

  g_return_val_if_fail (parent == NULL || VALID_ITER (project, parent), FALSE);

  if (parent)
    {
      GladeWidget *widget = glade_widget_get_from_gobject (parent->user_data);

      object = glade_project_nth_child (project, widget, 0);
    }
  else if (project->priv->tree)
      object = project->priv->tree->data;

  if (object)
    {
      glade_project_model_get_iter_for_object (project, object, iter);
      return TRUE;
    }

  iter->stamp = 0;
  iter->user_data = NULL;
  return FALSE;
}

static gboolean
glade_project_model_iter_parent (GtkTreeModel* model,
                                 GtkTreeIter* iter,
                                 GtkTreeIter* child)
{
	GladeProject* project = GLADE_PROJECT (model);
	GladeWidget* widget;
	GladeWidget* parent;

	g_return_val_if_fail (VALID_ITER (project, child), FALSE);

	widget = glade_widget_get_from_gobject (child->user_data);
	parent = glade_widget_get_parent (widget);

	if (parent && 
	    glade_project_has_object (project, glade_widget_get_object (parent)))
	{
		glade_project_model_get_iter_for_object (project,
							 glade_widget_get_object (parent),
							 iter);
		return TRUE;
	}

	iter->stamp = 0;
	iter->user_data = NULL;

	return FALSE;
}

static void
gtk_tree_model_iface_init (GtkTreeModelIface* iface)
{
	iface->get_flags = glade_project_model_get_flags;
	iface->get_column_type = glade_project_model_get_column_type;
	iface->get_n_columns = glade_project_model_get_n_columns;
	iface->get_iter = glade_project_model_get_iter;
	iface->get_path = glade_project_model_get_path;
	iface->get_value = glade_project_model_get_value;	
	iface->iter_next = glade_project_model_iter_next;
	iface->iter_children = glade_project_model_iter_children;
	iface->iter_has_child = glade_project_model_iter_has_child;
	iface->iter_n_children = glade_project_model_iter_n_children;
	iface->iter_nth_child = glade_project_model_iter_nth_child;
	iface->iter_parent = glade_project_model_iter_parent;
}
