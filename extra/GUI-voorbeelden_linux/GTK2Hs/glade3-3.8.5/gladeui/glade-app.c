/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2001 Ximian, Inc.
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
 *   Naba Kumar <naba@gnome.org>
 */

#include <config.h>

/**
 * SECTION:glade-app
 * @Short_Description: The central control point of the Glade core.
 *
 * This main control object is where we try to draw the line between
 * what is the Glade core and what is the main application. The main
 * application must derive from the GladeApp object and create an instance
 * to initialize the Glade core.
 */

#include "glade.h"
#include "glade-debug.h"
#include "glade-cursor.h"
#include "glade-catalog.h"
#include "glade-fixed.h"
#include "glade-design-view.h"
#include "glade-marshallers.h"
#include "glade-accumulators.h"

#include <string.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <glib/gi18n-lib.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#ifdef MAC_INTEGRATION
#  include <gtkosxapplication.h>
#endif

#define GLADE_CONFIG_FILENAME "glade-3.conf"

#define GLADE_APP_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), GLADE_TYPE_APP, GladeAppPrivate))

enum
{
	UPDATE_UI,
	SIGNAL_EDITOR_CREATED,
	LAST_SIGNAL
};

enum
{
	PROP_0,
	PROP_ACTIVE_PROJECT,
	PROP_POINTER_MODE
};

struct _GladeAppPrivate
{
	GtkWidget *window;
	
	GladePalette *palette;         /* See glade-palette */
	GladeProject *active_project;  /* Currently active project (if there is at least one
				        * project; then this is always valid) */
	GladeEditor *editor;           /* See glade-editor */
	GladeClipboard *clipboard;     /* See glade-clipboard */
	GList *catalogs;               /* See glade-catalog */

	GList *projects; /* The list of Projects */
	
	GKeyFile *config;/* The configuration file */

	GtkWindow *transient_parent; /* If set by glade_app_set_transient_parent(); this
				      * will be used as the transient parent of all toplevel
				      * GladeWidgets.
				      */
	GtkAccelGroup *accel_group;	/* Default acceleration group for this app */
	GList *undo_list, *redo_list;	/* Lists of buttons to refresh in update-ui signal */

	GladePointerMode pointer_mode;  /* Current mode for the pointer in the workspace */

	guint selection_changed_id; /* for queue_selection_changed() */
};

static guint glade_app_signals[LAST_SIGNAL] = { 0 };

/* installation paths */
static gchar *catalogs_dir = NULL;
static gchar *modules_dir  = NULL;
static gchar *plugins_dir  = NULL;
static gchar *pixmaps_dir  = NULL;
static gchar *locale_dir   = NULL;
static gchar *bin_dir      = NULL;

static GladeApp *singleton_app = NULL;
static gboolean check_initialised = FALSE;

static void glade_init_check (void);

G_DEFINE_TYPE (GladeApp, glade_app, G_TYPE_OBJECT);


GType
glade_pointer_mode_get_type (void)
{
	static GType etype = 0;

	if (etype == 0) 
	{
		static const GEnumValue values[] = 
		{
			{ GLADE_POINTER_SELECT,      "select",      "Select widgets" },
			{ GLADE_POINTER_ADD_WIDGET,  "add",         "Add widgets" },
			{ GLADE_POINTER_DRAG_RESIZE, "drag-resize", "Drag and resize widgets" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static ("GladePointerMode", 
						values);
	}
	return etype;
}


/*****************************************************************
 *                    GObjectClass                               *
 *****************************************************************/
static GObject *
glade_app_constructor (GType                   type,
                       guint                   n_construct_properties,
                       GObjectConstructParam  *construct_properties)
{
	GObject *object;
	
	/* singleton */
	if (!singleton_app)
	{
		object = G_OBJECT_CLASS (glade_app_parent_class)->constructor (type,
									       n_construct_properties,
									       construct_properties);
		singleton_app = GLADE_APP (object);
	}
	else
	{
		g_object_ref (singleton_app);
	}
	  
	return G_OBJECT (singleton_app);
}



static void
glade_app_dispose (GObject *app)
{
	GladeAppPrivate *priv = GLADE_APP_GET_PRIVATE (app);
	
	if (priv->editor)
	{
		g_object_unref (priv->editor);
		priv->editor = NULL;
	}
	if (priv->palette)
	{
		g_object_unref (priv->palette);
		priv->palette = NULL;
	}
	if (priv->clipboard)
	{
		gtk_widget_destroy (GTK_WIDGET (priv->clipboard->view));
		priv->clipboard = NULL;
	}
	/* FIXME: Remove projects */
	
	if (priv->config)
	{
		g_key_file_free (priv->config);
		priv->config = NULL;
	}
	
	G_OBJECT_CLASS (glade_app_parent_class)->dispose (app);
}

static void
glade_app_finalize (GObject *app)
{
	g_free (catalogs_dir);
	g_free (modules_dir);
	g_free (pixmaps_dir);	
	g_free (locale_dir);
	g_free (bin_dir);

	singleton_app = NULL;
	check_initialised = FALSE;

	G_OBJECT_CLASS (glade_app_parent_class)->finalize (app);
}

static void
glade_app_set_property (GObject      *object,
                        guint         property_id,
                        const GValue *value,
                        GParamSpec   *pspec)
{
	switch (property_id) 
	{
	case PROP_ACTIVE_PROJECT:
		glade_app_set_project (g_value_get_object (value));
		break;
	case PROP_POINTER_MODE:
		glade_app_set_pointer_mode (g_value_get_enum (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID
			(object, property_id, pspec);
		break;
	}
}

static void
glade_app_get_property (GObject      *object,
                        guint         property_id,
                        GValue       *value,
                        GParamSpec   *pspec)
{
	GladeApp *app = GLADE_APP (object);


	switch (property_id) 
	{
	case PROP_ACTIVE_PROJECT:
		g_value_set_object (value, app->priv->active_project);
		break;
	case PROP_POINTER_MODE:
		g_value_set_enum (value, app->priv->pointer_mode);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
		break;
	}
}

/*****************************************************************
 *                    GladeAppClass                              *
 *****************************************************************/
static void
glade_app_refresh_undo_redo_button (GladeApp *app,
				    GtkWidget *button,
				    gboolean undo)
{
	GladeCommand *command = NULL;
	GladeProject *project;
	gchar *desc;

	if ((project = glade_app_get_project ()) != NULL)
	{
		if (undo)
			command = glade_project_next_undo_item (project);
		else
			command = glade_project_next_redo_item (project);
	}

	/* Change tooltips */
	desc = g_strdup_printf ((undo) ? _("Undo: %s") : _("Redo: %s"),
			command ? command->description : _("the last action"));
        gtk_widget_set_tooltip_text (button, desc);
	g_free (desc);

	/* Set sensitivity on the button */
	gtk_widget_set_sensitive (button, command != NULL);
}

static void
glade_app_update_ui_default (GladeApp *app)
{
	GList *list;
	
	for (list = app->priv->undo_list; list; list = list->next)
		if (list->data)
			glade_app_refresh_undo_redo_button (app, list->data, TRUE);

	for (list = app->priv->redo_list; list; list = list->next)
		if (list->data)
			glade_app_refresh_undo_redo_button (app, list->data, FALSE);
}

static void
glade_app_signal_editor_created_default (GladeApp *app, GladeSignalEditor *signal_editor)
{
	glade_signal_editor_construct_signals_list (signal_editor);
}

static GKeyFile *
glade_app_config_load (GladeApp *app)
{
	GKeyFile *config = g_key_file_new ();
	gchar *filename;

	filename = g_build_filename (g_get_user_config_dir (), GLADE_CONFIG_FILENAME, NULL);

	g_key_file_load_from_file (config, filename, G_KEY_FILE_NONE, NULL);
	
	g_free (filename);
	
	return config;
}

const gchar *
glade_app_get_catalogs_dir (void)
{
	glade_init_check ();
	
	return catalogs_dir;
}

const gchar *
glade_app_get_modules_dir (void)
{
	glade_init_check ();

	return modules_dir;
}

const gchar *
glade_app_get_plugins_dir (void)
{
	glade_init_check ();

	return plugins_dir;
}


const gchar *
glade_app_get_pixmaps_dir (void)
{
	glade_init_check ();

	return pixmaps_dir;
}

const gchar *
glade_app_get_locale_dir (void)
{
	glade_init_check ();
	
	return locale_dir;
}

const gchar *
glade_app_get_bin_dir (void)
{
	glade_init_check ();
	
	return bin_dir;
}


/* build package paths at runtime */
static void
build_package_paths (void)
{
#if defined (G_OS_WIN32) || (defined (MAC_INTEGRATION) && defined (MAC_BUNDLE))
	gchar *prefix;

# ifdef G_OS_WIN32
	prefix = g_win32_get_package_installation_directory_of_module (NULL);

# else // defined (MAC_INTEGRATION) && defined (MAC_BUNDLE)
	prefix = quartz_application_get_bundle_path ();
# endif

	pixmaps_dir  = g_build_filename (prefix, "share", PACKAGE, "pixmaps", NULL);
	catalogs_dir = g_build_filename (prefix, "share", PACKAGE, "catalogs", NULL);
	modules_dir  = g_build_filename (prefix, "lib", PACKAGE, "modules", NULL);
	locale_dir   = g_build_filename (prefix, "share", "locale", NULL);
	bin_dir      = g_build_filename (prefix, "bin", NULL);

	g_free (prefix);
#else
	catalogs_dir = g_strdup (GLADE_CATALOGSDIR);
	modules_dir  = g_strdup (GLADE_MODULESDIR);
	plugins_dir  = g_strdup (GLADE_PLUGINSDIR);
	pixmaps_dir  = g_strdup (GLADE_PIXMAPSDIR);
	locale_dir   = g_strdup (GLADE_LOCALEDIR);
	bin_dir      = g_strdup (GLADE_BINDIR);
#endif
}

/* initialization function for libgladeui (not GladeApp) */
static void
glade_init_check (void)
{	
	if (check_initialised)
		return;

	/* Make sure path accessors work on osx */
	g_type_init ();

	build_package_paths ();

	bindtextdomain (GETTEXT_PACKAGE, locale_dir);
	bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");

	check_initialised = TRUE;
}

static void
glade_app_init (GladeApp *app)
{
	static gboolean initialized = FALSE;
	
	app->priv = GLADE_APP_GET_PRIVATE (app);	

	singleton_app = app;
	
	glade_init_check ();	
	
	if (!initialized)
	{
		gtk_icon_theme_append_search_path (gtk_icon_theme_get_default (),
						   pixmaps_dir);	
	
		glade_cursor_init ();
		
		initialized = TRUE;
	}

	app->priv->accel_group = NULL;
	
	/* Initialize app objects */
	app->priv->catalogs = (GList *) glade_catalog_load_all ();
	
	/* Create palette */
	app->priv->palette = (GladePalette *) glade_palette_new (app->priv->catalogs);
	g_object_ref_sink (app->priv->palette);
	
	/* Create Editor */
	app->priv->editor = GLADE_EDITOR (glade_editor_new ());
	g_object_ref_sink (GTK_OBJECT (app->priv->editor));
	
	glade_editor_refresh (app->priv->editor);
	
	/* Create clipboard */
	app->priv->clipboard = glade_clipboard_new ();

	/* Load the configuration file */
	app->priv->config = glade_app_config_load (app);
	
	/* Undo/Redo button list */
	app->priv->undo_list = app->priv->redo_list = NULL;
}

static void
glade_app_class_init (GladeAppClass * klass)
{
	GObjectClass *object_class;
	
	object_class = G_OBJECT_CLASS (klass);
	
	object_class->constructor  = glade_app_constructor;
	object_class->dispose      = glade_app_dispose;
	object_class->finalize     = glade_app_finalize;
	object_class->get_property = glade_app_get_property;
	object_class->set_property = glade_app_set_property;

	klass->update_ui_signal = glade_app_update_ui_default;
	klass->signal_editor_created = glade_app_signal_editor_created_default;
	klass->show_properties  = NULL;
	klass->hide_properties  = NULL;

	/**
	 * GladeApp::update-ui:
	 * @gladeapp: the #GladeApp which received the signal.
	 *
	 * Emitted when a project name changes or a cut/copy/paste/delete occurred.
	 */
	glade_app_signals[UPDATE_UI] =
		g_signal_new ("update-ui",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GladeAppClass,
					       update_ui_signal),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE, 0);

	/**
	 * GladeApp::signal-editor-created:
	 * @gladeapp: the #GladeApp which received the signal.
	 * @signal_editor: the new #GladeSignalEditor.
	 *
	 * Emitted when a new signal editor created.
	 * A tree view is created in the default handler.
	 * Connect your handler before the default handler for setting a custom column or renderer
	 * and after it for connecting to the tree view signals
	 */
	glade_app_signals[SIGNAL_EDITOR_CREATED] =
		g_signal_new ("signal-editor-created",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GladeAppClass,
					       signal_editor_created),
			      NULL, NULL,
			      glade_marshal_VOID__OBJECT,
			      G_TYPE_NONE, 1, G_TYPE_OBJECT);

	g_object_class_install_property 
		(object_class, PROP_ACTIVE_PROJECT,
		 g_param_spec_object 
		 ("active-project", _("Active Project"), 
		  _("The active project"),
		  GLADE_TYPE_PROJECT, G_PARAM_READWRITE));

	g_object_class_install_property 
		(object_class, PROP_POINTER_MODE,
		 g_param_spec_enum 
		 ("pointer-mode", _("Pointer Mode"), 
		  _("Current mode for the pointer in the workspace"),
		  GLADE_TYPE_POINTER_MODE,
		  GLADE_POINTER_SELECT,
		  G_PARAM_READWRITE));
	
	g_type_class_add_private (klass, sizeof (GladeAppPrivate));
}

/*****************************************************************
 *                       Public API                              *
 *****************************************************************/
static void
on_project_selection_changed_cb (GladeProject *project, GladeApp *app)
{
	GList *list;
	gint num;

	g_return_if_fail (GLADE_IS_PROJECT (project));
	g_return_if_fail (GLADE_IS_APP (app));

	/* Only update the editor if the selection has changed on
	 * the currently active project.
	 */
	if (app->priv->editor &&
	    (project == glade_app_get_project ()))
	{
		list = glade_project_selection_get (project);
		num = g_list_length (list);
		if (num == 1 && !GLADE_IS_PLACEHOLDER (list->data))
			glade_editor_load_widget (app->priv->editor,
						  glade_widget_get_from_gobject (list->data));
		else
			glade_editor_load_widget (app->priv->editor, NULL);
	}
}

/**
 * glade_app_config_save
 *
 * Saves the GKeyFile to "g_get_user_config_dir()/GLADE_CONFIG_FILENAME"
 *
 * Return 0 on success.
 */
gint
glade_app_config_save ()
{
	GIOChannel *channel;
	GIOStatus   status;
	gchar *data=NULL, *filename;
	const gchar *config_dir = g_get_user_config_dir ();
	GError *error = NULL;
	gsize size, written, bytes_written = 0;
	static gboolean error_shown = FALSE;
	GladeApp *app;

	/* If we had any errors; wait untill next session to retry.
	 */
	if (error_shown) return -1;

	app = glade_app_get ();
	
	/* Just in case... try to create the config directory */
	if (g_file_test (config_dir, G_FILE_TEST_IS_DIR) == FALSE)
	{
		if (g_file_test (config_dir, G_FILE_TEST_EXISTS))
		{
			/* Config dir exists but is not a directory */
			glade_util_ui_message
				(glade_app_get_window(),
				 GLADE_UI_ERROR, NULL,
				 _("Trying to save private data to %s directory "
				   "but it is a regular file.\n"
				   "No private data will be saved in this session"), 
				 config_dir);
			error_shown = TRUE;
			return -1;
		}
		else if (g_mkdir (config_dir, S_IRWXU) != 0)
		{
			/* Doesnt exist; failed to create */
			glade_util_ui_message
				(glade_app_get_window(),
				 GLADE_UI_ERROR, NULL,
				 _("Failed to create directory %s to save private data.\n"
				   "No private data will be saved in this session"), config_dir);
			error_shown = TRUE;
			return -1;
		}
	} 

	filename = g_build_filename (config_dir, GLADE_CONFIG_FILENAME, NULL);
	
	if ((channel = g_io_channel_new_file (filename, "w", &error)) != NULL)
	{
		if ((data = g_key_file_to_data (app->priv->config, &size, &error)) != NULL)
		{
			
			/* Implement loop here */
			while ((status = g_io_channel_write_chars
				(channel, 
				 data + bytes_written, /* Offset of write */
				 size - bytes_written, /* Size left to write */
				 &written, &error)) != G_IO_STATUS_ERROR &&
			       (bytes_written + written) < size)
				bytes_written += written;

			if (status == G_IO_STATUS_ERROR)
			{
				glade_util_ui_message
					(glade_app_get_window(),
					 GLADE_UI_ERROR, NULL,
					 _("Error writing private data to %s (%s).\n"
					   "No private data will be saved in this session"), 
					 filename, error->message);
				 error_shown = TRUE;
			}
			g_free (data);
		} 
		else
		{
			glade_util_ui_message
				(glade_app_get_window(),
				 GLADE_UI_ERROR, NULL,
				 _("Error serializing configuration data to save (%s).\n"
				   "No private data will be saved in this session"), 
				 error->message);
			error_shown = TRUE;
		}
		g_io_channel_shutdown(channel, TRUE, NULL);
		g_io_channel_unref (channel);
	}
	else
	{
		glade_util_ui_message
			(glade_app_get_window(),
			 GLADE_UI_ERROR, NULL,
			 _("Error opening %s to write private data (%s).\n"
			   "No private data will be saved in this session"), 
			 filename, error->message);
		error_shown = TRUE;
	}
	g_free (filename);
	
	if (error)
	{
		g_error_free (error);
		return -1;
	}
	return 0;
}

void
glade_app_set_transient_parent (GtkWindow *parent)
{
	GList     *projects, *objects;
	GladeApp  *app;
	
	g_return_if_fail (GTK_IS_WINDOW (parent));

	app = glade_app_get ();
	app->priv->transient_parent = parent;

	/* Loop over all projects/widgets and set_transient_for the toplevels.
	 */
	for (projects = glade_app_get_projects (); /* projects */
	     projects; projects = projects->next) 
		for (objects = (GList *) glade_project_get_objects (GLADE_PROJECT (projects->data));  /* widgets */
		     objects; objects = objects->next)
			if (GTK_IS_WINDOW (objects->data))
				gtk_window_set_transient_for
					(GTK_WINDOW (objects->data), parent);
}

GtkWindow *
glade_app_get_transient_parent (void)
{
	GtkWindow *parent;
	GladeApp  *app = glade_app_get ();

	parent = app->priv->transient_parent;
	
	return parent;
}

GladeApp *
glade_app_get (void)
{
	if (!singleton_app)
		g_critical ("No available GladeApp");
	return singleton_app;
}

void
glade_app_update_ui (void)
{
	GladeApp *app = glade_app_get ();

	g_signal_emit (G_OBJECT (app),
		       glade_app_signals[UPDATE_UI], 0);
}

void
glade_app_set_window (GtkWidget *window)
{
	GladeApp *app = glade_app_get ();

	app->priv->window = window;
}

GladeCatalog *
glade_app_get_catalog (const gchar *name)
{
	GladeApp     *app = glade_app_get ();
	GList        *list;
	GladeCatalog *catalog;

	g_return_val_if_fail (name && name[0], NULL);

	for (list = app->priv->catalogs; list; list = list->next)
	{
		catalog = list->data;
		if (!strcmp (glade_catalog_get_name (catalog), name))
			return catalog;
	}
	return NULL;
}

gboolean
glade_app_get_catalog_version (const gchar *name, gint *major, gint *minor)
{
	GladeCatalog *catalog = glade_app_get_catalog (name);

	g_return_val_if_fail (catalog != NULL, FALSE);

	if (major)
		*major = glade_catalog_get_major_version (catalog);
	if (minor)
		*minor = glade_catalog_get_minor_version (catalog);
	
	return TRUE;
}

GList *
glade_app_get_catalogs (void)
{
	GladeApp *app = glade_app_get ();

	return app->priv->catalogs; 
}


GtkWidget *
glade_app_get_window (void)
{
	GladeApp  *app = glade_app_get ();
	return app->priv->window;
}

GladeEditor *
glade_app_get_editor (void)
{
	GladeApp *app    = glade_app_get ();
	return app->priv->editor;
}

GladePalette *
glade_app_get_palette (void)
{
	GladeApp *app = glade_app_get ();
	return app->priv->palette;
}

GladeClipboard *
glade_app_get_clipboard (void)
{
	GladeApp       *app       = glade_app_get ();
	return app->priv->clipboard;
}

GladeProject *
glade_app_get_project (void)
{
	GladeApp *app = glade_app_get ();
	return app->priv->active_project;
}

GladeProject *
glade_app_check_get_project (void)
{
	if (singleton_app)
		return glade_app_get_project ();
	return NULL;
}

GList *
glade_app_get_projects (void)
{
	GladeApp *app = glade_app_get ();
	return app->priv->projects;
}

GKeyFile *
glade_app_get_config (void)
{
	GladeApp *app = glade_app_get ();
	return app->priv->config;
}

gboolean
glade_app_is_project_loaded (const gchar *project_path)
{
	GladeApp *app;
	GList    *list;
	gboolean  loaded = FALSE;

	if (project_path == NULL)
		return FALSE;

	app = glade_app_get ();

	for (list = app->priv->projects; list; list = list->next)
	{
		GladeProject *cur_project = GLADE_PROJECT (list->data);

		if ((loaded = glade_project_get_path (cur_project) && 
		     (strcmp (glade_project_get_path (cur_project), project_path) == 0)))
			break;
	}

	return loaded;
}

/**
 * glade_app_get_project_by_path:
 * @project_path: The path of an open project
 *
 * Finds an open project with @path
 *
 * Returns: A #GladeProject, or NULL if no such open project was found
 */
GladeProject*
glade_app_get_project_by_path (const gchar *project_path)
{
	GladeApp *app;
	GList    *l;
	gchar *canonical_path;

	if (project_path == NULL)
		return NULL;

	app = glade_app_get ();

	canonical_path = glade_util_canonical_path (project_path);

	for (l = app->priv->projects; l; l = l->next)
	{
		GladeProject *project = (GladeProject *) l->data;

		if (glade_project_get_path (project) && strcmp (canonical_path, glade_project_get_path (project)) == 0) {
			g_free (canonical_path);
			return project;
		}
	}

	g_free (canonical_path);

	return NULL;
}

void
glade_app_show_properties (gboolean raise)
{
	GladeApp  *app  = glade_app_get ();

	if (GLADE_APP_GET_CLASS (app)->show_properties)
		GLADE_APP_GET_CLASS (app)->show_properties (app, raise);
	else 
		g_critical ("%s not implemented\n", G_STRFUNC);
}

void
glade_app_hide_properties (void)
{
	GladeApp  *app  = glade_app_get ();

	if (GLADE_APP_GET_CLASS (app)->hide_properties)
		GLADE_APP_GET_CLASS (app)->hide_properties (app);
	else 
		g_critical ("%s not implemented\n", G_STRFUNC);

}

void
glade_app_add_project (GladeProject *project)
{
	GladeApp  *app;
	GladeDesignView *view;
	GladeDesignLayout *layout;

 	g_return_if_fail (GLADE_IS_PROJECT (project));

	app = glade_app_get ();

	/* If the project was previously loaded, don't re-load */
	if (g_list_find (app->priv->projects, project) != NULL)
	{
		glade_app_set_project (project);
		return;
	}
	
	/* Take a reference for GladeApp here... */
	app->priv->projects = g_list_append (app->priv->projects, 
					     g_object_ref (project));
	
	/* connect to the project signals so that the editor can be updated */
	g_signal_connect (G_OBJECT (project), "selection_changed",
			  G_CALLBACK (on_project_selection_changed_cb), app);

	
	glade_app_set_project (project);

	/* Select the first window in the project */
	if (g_list_length (app->priv->projects) == 1 ||
	    !(view = glade_design_view_get_from_project (project)) ||
	    !(layout = glade_design_view_get_layout (view)) ||
	    !gtk_bin_get_child (GTK_BIN (layout)))
	{
		const GList *node;
		for (node = glade_project_get_objects (project);
		     node != NULL;
		     node = g_list_next (node))
		{
			GObject *obj = G_OBJECT (node->data);

			if (GTK_IS_WINDOW (obj))
			{
				glade_project_selection_set (project, obj, TRUE);
				glade_widget_show (glade_widget_get_from_gobject (obj));
				break;
			}
		}
	}

	/* XXX I think the palette & editor should detect this by itself */
	gtk_widget_set_sensitive (GTK_WIDGET (app->priv->palette), TRUE);
	gtk_widget_set_sensitive (GTK_WIDGET (app->priv->editor), TRUE);

}

void
glade_app_remove_project (GladeProject *project)
{
	GladeApp *app;
	g_return_if_fail (GLADE_IS_PROJECT (project));

	app = glade_app_get ();

	app->priv->projects = g_list_remove (app->priv->projects, project);

	/* this is needed to prevent clearing the selection of a closed project 
	 */
	app->priv->active_project = NULL;
	
	/* If no more projects */
	if (app->priv->projects == NULL)
	{
		/* XXX I think the palette & editor should detect this. */
		gtk_widget_set_sensitive (GTK_WIDGET (app->priv->palette), FALSE);

		/* set loaded widget to NULL first so that we dont mess
		 * around with sensitivity of the editor children.
		 */
		glade_editor_load_widget (app->priv->editor, NULL);
		gtk_widget_set_sensitive (GTK_WIDGET (app->priv->editor), FALSE);
	} 
	else
		glade_app_set_project (g_list_last (app->priv->projects)->data);

	/* Its safe to just release the project as the project emits a
	 * "close" signal and everyone is responsable for cleaning up at
	 * that point.
	 */
	g_object_unref (project);
}


/**
 * glade_app_set_project:
 * @project: A #GladeProject
 *
 * Sets the active project in the #GladeApp to @project.
 */
void
glade_app_set_project (GladeProject *project)
{
	GladeApp *app = glade_app_get();

	g_return_if_fail (GLADE_IS_PROJECT (project));

	if (app->priv->active_project == project)
		return;

	if (!g_list_find (app->priv->projects, project))
	{
		g_warning ("Could not set project because it could not "
			   " be found in the app->priv->project list\n");
		return;
	}

	/* clear the selection in the previous project */
	if (app->priv->active_project)
		glade_project_selection_clear (app->priv->active_project, FALSE);

	app->priv->active_project = project;

	/* (XXX really ?) trigger the selection changed signal to update the editor */
	glade_project_selection_changed (project);
	
	/* refresh palette for active project */
	glade_palette_refresh (glade_app_get_palette ());

	/* Update UI */
	glade_app_update_ui ();

	g_object_notify (G_OBJECT (app), "active-project");
}


/**
 * glade_app_set_pointer_mode:
 * @mode: A #GladePointerMode
 *
 * Sets the #GladePointerMode
 */
void
glade_app_set_pointer_mode (GladePointerMode mode)
{
	GladeApp *app = glade_app_get();

	app->priv->pointer_mode = mode;

	g_object_notify (G_OBJECT (app), "pointer-mode");
}


/**
 * glade_app_get_pointer_mode:
 *
 * Gets the current #GladePointerMode
 * 
 * Returns: The #GladePointerMode
 */
GladePointerMode
glade_app_get_pointer_mode (void)
{
	GladeApp *app = glade_app_get();

	return app->priv->pointer_mode;
}

/**
 * glade_app_command_copy:
 * @app: A #GladeApp
 *
 * Copy the active project's selection (the new copies
 * will end up on the clipboard and will be set as
 * the clipboards selection).
 */
void
glade_app_command_copy (void)
{
	GladeApp           *app;
	GList              *widgets = NULL, *list;
	GladeWidget        *widget;
	gboolean            failed = FALSE;

	app = glade_app_get();
	if (app->priv->active_project == NULL ||
	    glade_project_is_loading (app->priv->active_project))
		return;

	for (list = glade_app_get_selection ();
	     list && list->data; list = list->next)
	{
		widget  = glade_widget_get_from_gobject (list->data);
		widgets = g_list_prepend (widgets, widget);
	}

	if (failed == FALSE && widgets != NULL)
	{
		glade_command_copy (widgets);
		glade_app_update_ui ();
	}
	else if (widgets == NULL)
		glade_util_ui_message (glade_app_get_window(),
				       GLADE_UI_INFO, NULL,
				       _("No widget selected."));

	if (widgets) g_list_free (widgets);

}

/**
 * glade_app_command_cut:
 * @app: A #GladeApp
 *
 * Cut the active project's selection (the cut objects
 * will end up on the clipboard and will be set as
 * the clipboards selection).
 */
void
glade_app_command_cut (void)
{
	GladeApp           *app;
	GList              *widgets = NULL, *list;
	GladeWidget        *widget;
	gboolean            failed = FALSE;

	app = glade_app_get();
	if (app->priv->active_project == NULL ||
	    glade_project_is_loading (app->priv->active_project))
		return;
	
	for (list = glade_app_get_selection ();
	     list && list->data; list = list->next)
	{
		widget  = glade_widget_get_from_gobject (list->data);
		widgets = g_list_prepend (widgets, widget);
	}

	if (failed == FALSE && widgets != NULL)
	{
		glade_command_cut (widgets);
		glade_app_update_ui ();
	}
	else if (widgets == NULL)
		glade_util_ui_message (glade_app_get_window(),
				       GLADE_UI_INFO, NULL,
				       _("No widget selected."));

	if (widgets) g_list_free (widgets);

}

/**
 * glade_app_command_paste:
 * @placeholder: A #GladePlaceholder
 *
 * Paste the clipboard selection to the active project's 
 * selection (the project must have only one object selected).
 */
void
glade_app_command_paste (GladePlaceholder *placeholder)
{
	GladeApp           *app;
	GladeClipboard     *clipboard;
	GList              *list;
	GladeWidget        *widget = NULL, *parent;
	gint                placeholder_relations = 0;
	GladeFixed	   *fixed = NULL;

	app = glade_app_get();
	if (app->priv->active_project == NULL ||
	    glade_project_is_loading (app->priv->active_project))
		return;

	if (placeholder)
	{
		if (glade_placeholder_get_project (placeholder) == NULL ||
		    glade_project_is_loading (glade_placeholder_get_project (placeholder)))
			return;
	}

	list      = glade_project_selection_get (app->priv->active_project);
	clipboard = glade_app_get_clipboard ();

	/* If there is a selection, paste in to the selected widget, otherwise
	 * paste into the placeholder's parent, or at the toplevel
	 */
	parent = list ? glade_widget_get_from_gobject (list->data) : 
		 (placeholder) ? glade_placeholder_get_parent (placeholder) : NULL;

	widget = clipboard->selection ? clipboard->selection->data : NULL;

	/* Ignore parent argument if we are pasting a toplevel
	 */
	if (g_list_length (clipboard->selection) == 1 &&
	    widget && GWA_IS_TOPLEVEL (widget->adaptor))
		parent = NULL;

	if (parent && GLADE_IS_FIXED (parent)) fixed = GLADE_FIXED (parent);

	/* Check if parent is actually a container of any sort */
	if (parent && 
	    !glade_widget_adaptor_is_container (parent->adaptor))
	{
		glade_util_ui_message (glade_app_get_window(),
				       GLADE_UI_INFO, NULL,
				       _("Unable to paste to the selected parent"));
		return;
	}
		
	/* Check if selection is good */
	if ((list = glade_app_get_selection ()) != NULL)
	{
		if (g_list_length (list) != 1)
		{
			glade_util_ui_message (glade_app_get_window(),
					       GLADE_UI_INFO, NULL,
					       _("Unable to paste to multiple widgets"));

			return;
		}

	}

	/* Abort operation when adding a non scrollable widget to any kind of GtkScrolledWindow. */
	if (parent && widget &&
	    glade_util_check_and_warn_scrollable (parent, widget->adaptor, glade_app_get_window()))
		return;

	/* Check if we have anything to paste */
	if (g_list_length (clipboard->selection) == 0)
	{
		glade_util_ui_message (glade_app_get_window (), GLADE_UI_INFO, NULL,
				    _("No widget selected on the clipboard"));

		return;
	}
	
	/* Check that we have compatible heirarchies */
	for (list = clipboard->selection; 
	     list && list->data; list = list->next)
	{
		widget = list->data;

		if (!GWA_IS_TOPLEVEL (widget->adaptor) && parent)
		{
			/* Count placeholder relations
			 */
			if (glade_widget_placeholder_relation (parent, widget))
				placeholder_relations++;
		}
	}

	g_assert (widget);

	/* A GladeFixed that doesnt use placeholders can only paste one
	 * at a time
 	 */
	if (GTK_IS_WIDGET (widget->object) && 
	    gtk_widget_is_toplevel (GTK_WIDGET (widget->object)) == FALSE &&
	    parent && fixed && !GWA_USE_PLACEHOLDERS (parent->adaptor) &&
	    g_list_length (clipboard->selection) != 1) 
	{
		glade_util_ui_message (glade_app_get_window (), 
				       GLADE_UI_INFO, NULL,
				       _("Only one widget can be pasted at a "
					 "time to this container"));
		return;
	}

	/* Check that enough placeholders are available */
	if (parent && 
	    GWA_USE_PLACEHOLDERS (parent->adaptor) &&
	    glade_util_count_placeholders (parent) < placeholder_relations)
	{
		glade_util_ui_message (glade_app_get_window (), 
				       GLADE_UI_INFO, NULL,
				       _("Insufficient amount of placeholders in "
					 "target container"));
		return;
	}

	glade_command_paste (clipboard->selection, parent, placeholder);
	glade_app_update_ui ();
}


/**
 * glade_app_command_delete:
 *
 * Delete the active project's selection.
 */
void
glade_app_command_delete (void)
{
	GladeApp           *app;
	GList              *widgets = NULL, *list;
	GladeWidget        *widget;
	gboolean            failed = FALSE;

	app = glade_app_get();
	if (app->priv->active_project == NULL ||
	    glade_project_is_loading (app->priv->active_project))
		return;

	for (list = glade_app_get_selection ();
	     list && list->data; list = list->next)
	{
		widget  = glade_widget_get_from_gobject (list->data);
		widgets = g_list_prepend (widgets, widget);
	}
 
	if (failed == FALSE && widgets != NULL)
	{
		glade_command_delete (widgets);
		glade_app_update_ui ();
	}
	else if (widgets == NULL)
		glade_util_ui_message (glade_app_get_window(),
				       GLADE_UI_INFO, NULL,
				       _("No widget selected."));

	if (widgets) g_list_free (widgets);
}

/**
 * glade_app_command_delete_clipboard:
 *
 * Delete the clipboard's selection.
 */
void
glade_app_command_delete_clipboard (void)
{
	GladeClipboard  *clipboard;

	clipboard = glade_app_get_clipboard ();

	if (clipboard->selection == NULL)
	{
		glade_util_ui_message (glade_app_get_window (), GLADE_UI_INFO, NULL,
				    _("No widget selected on the clipboard"));
		return;
	}

	glade_command_delete (clipboard->selection);
	glade_app_update_ui ();
}


void
glade_app_command_undo (void)
{
	GladeApp *app = glade_app_get();
	if (app->priv->active_project)
	{
		glade_project_undo (app->priv->active_project);
		glade_editor_refresh (app->priv->editor);
		/* Update UI. */
		glade_app_update_ui ();
	}
}

void
glade_app_command_redo (void)
{
	GladeApp *app = glade_app_get();
	if (app->priv->active_project)
	{
		glade_project_redo (app->priv->active_project);
		glade_editor_refresh (app->priv->editor);
		/* Update UI. */
		glade_app_update_ui ();
	}
}

/*
 * glade_app_set_accel_group:
 *
 * Sets @accel_group to app.
 * The acceleration group will made available for editor dialog windows
 * from the plugin backend.
 */
void
glade_app_set_accel_group (GtkAccelGroup *accel_group)
{
	GladeApp *app;
	g_return_if_fail(GTK_IS_ACCEL_GROUP (accel_group));

	app = glade_app_get ();
	
	app->priv->accel_group = accel_group;
}

GtkAccelGroup *
glade_app_get_accel_group (void)
{
	return glade_app_get()->priv->accel_group;
}

static gboolean
glade_app_undo_button_destroyed (GtkWidget *button, GladeApp *app)
{
	app->priv->undo_list = g_list_remove (app->priv->undo_list, button);
	return FALSE;
}

static gboolean
glade_app_redo_button_destroyed (GtkWidget *button, GladeApp *app)
{
	app->priv->redo_list = g_list_remove (app->priv->redo_list, button);
	return FALSE;
}

static GtkWidget *
glade_app_undo_redo_button_new (GladeApp *app, gboolean undo)
{
	GtkWidget *button;
	
	button = gtk_button_new_from_stock ((undo) ? 
					    GTK_STOCK_UNDO : 
					    GTK_STOCK_REDO);
	
	g_signal_connect_swapped (button, "clicked",
				  (undo) ? G_CALLBACK (glade_app_command_undo) : 
					   G_CALLBACK (glade_app_command_redo),
				  app);
	
	if (undo)
	{
		app->priv->undo_list = g_list_prepend (app->priv->undo_list, button);
		g_signal_connect (button, "destroy",
				  G_CALLBACK (glade_app_undo_button_destroyed),
				  app);
	}
	else
	{
		app->priv->redo_list = g_list_prepend (app->priv->redo_list, button);
		g_signal_connect (button, "destroy",
				  G_CALLBACK (glade_app_redo_button_destroyed),
				  app);
	}
	
	glade_app_refresh_undo_redo_button (app, button, undo);
	
	return button;
}

/*
 * glade_app_undo_button_new:
 *
 * Creates a new GtkButton undo widget.
 * The button will be automatically updated with @app's undo stack.
 */
GtkWidget *
glade_app_undo_button_new (void)
{
	GladeApp  *app    = glade_app_get();
	return glade_app_undo_redo_button_new (app, TRUE);
}

/*
 * glade_app_redo_button_new:
 *
 * Creates a new GtkButton redo widget.
 * The button will be automatically updated with @app's redo stack.
 */
GtkWidget *
glade_app_redo_button_new (void)
{
	GladeApp  *app    = glade_app_get();
	return glade_app_undo_redo_button_new (app, FALSE);
}

GList*
glade_app_get_selection (void)
{
	GList *selection = NULL, *list;
	GladeProject *project;

	for (list = glade_app_get_projects (); 
	     list && list->data; list = list->next)
	{
		/* Only one project may have selection at a time
		 */
		project = list->data;
		if (glade_project_selection_get (project))
		{
			selection = glade_project_selection_get (project);
			break;
		}
	}
	return selection;
}


gboolean
glade_app_is_selected (GObject *object)
{
	return (g_list_find (glade_app_get_selection (), object) != NULL);
}

void
glade_app_selection_set (GObject  *object,
			 gboolean  emit_signal)
{
	GList        *list;
	GladeProject *project;

	for (list = glade_app_get_projects ();
	     list && list->data; list = list->next)
	{
		project = list->data;
		if (glade_project_has_object (project, object))
			glade_project_selection_set (project, 
						     object, 
						     emit_signal);
		else
			glade_project_selection_clear (project, emit_signal);
	}

	/* Instead of calling selection_set after all
	 * the selection_clear calls (lazy).
	 */
	if (GTK_IS_WIDGET (object))
		glade_util_add_selection (GTK_WIDGET (object));
}

void
glade_app_selection_add (GObject      *object,
			 gboolean      emit_signal)
{
	GList        *list;
	GladeWidget  *widget   = glade_widget_get_from_gobject (object),  
		*selected;
	GladeProject *project  = glade_widget_get_project (widget);

	/* Ignore request if the there is a selection 
	 * from another project.
	 */
	if ((list = glade_app_get_selection ()) != NULL)
	{
		selected = glade_widget_get_from_gobject (list->data);
		if (glade_widget_get_project (selected) != project)
			return;
	}
	glade_project_selection_add (project, object, emit_signal);
}

void
glade_app_selection_remove (GObject      *object,
			    gboolean      emit_signal)
{
	GladeWidget  *widget   = glade_widget_get_from_gobject (object);
	GladeProject *project  = glade_widget_get_project (widget);;

	glade_project_selection_remove (project, object, emit_signal);
}

void
glade_app_selection_clear (gboolean      emit_signal)
{
	GList        *list;
	GladeProject *project;

	glade_util_clear_selection ();
	for (list = glade_app_get_projects ();
	     list && list->data; list = list->next)
	{
		project = list->data;
		glade_project_selection_clear (project, emit_signal);
	}
}

void
glade_app_selection_changed (void)
{
	GList        *list;
	GladeProject *project;

	for (list = glade_app_get_projects ();
	     list && list->data; list = list->next)
	{
		project = list->data;
		glade_project_selection_changed (project);
	}
}

static gboolean
selection_change_idle (GladeApp *app)
{
	glade_app_selection_changed ();
	app->priv->selection_changed_id = 0;
	return FALSE;
}

void
glade_app_queue_selection_changed (void)
{
	GladeApp  *app = glade_app_get ();

	if (app->priv->selection_changed_id == 0)
		app->priv->selection_changed_id = 
			g_idle_add ((GSourceFunc)selection_change_idle, app);
}


GladeApp*
glade_app_new (void)
{
	return g_object_new (GLADE_TYPE_APP, NULL);
}

