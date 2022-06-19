/*
 * Copyright (C) 2010 Marco Diego Aurélio Mesquita
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Authors:
 *   Marco Diego Aurélio Mesquita <marcodiegomesquita@gmail.com>
 */

#include <config.h>

#include <gladeui/glade.h>

#include <stdlib.h>
#include <locale.h>
#include <glib/gi18n-lib.h>
#include <glib/gstdio.h>

#include "glade-previewer.h"
#include "glade-preview-template.h"
#include "glade-preview-tokens.h"

typedef struct
{
  GladePreviewer *preview;
  gchar *file_name, *toplevel;
  gboolean is_template;
} GladePreviewerApp;

static GObject *
get_toplevel (GtkBuilder *builder, gchar *name)
{
  GObject *toplevel = NULL;
  GObject *object;

  if (name == NULL)
    {
      GSList *l, *objects = gtk_builder_get_objects (builder);

      /* Iterate trough objects and search for a window or widget */
      for (l = objects; l; l = g_slist_next (l))
        {
          GObject *obj = l->data;

          if (!GTK_IS_WIDGET (obj) || gtk_widget_get_parent (GTK_WIDGET (obj)))
            continue;

          if (toplevel == NULL)
            toplevel = obj;
          else if (GTK_IS_WINDOW (obj))
            toplevel = obj;
        }

      g_slist_free (objects);
      if (toplevel == NULL)
        {
          g_printerr (_("UI definition has no previewable widgets.\n"));
          exit (1);
        }
    }
  else
    {
      object = gtk_builder_get_object (builder, name);

      if (object == NULL)
        {
          g_printerr (_("Object %s not found in UI definition.\n"), name);
          exit (1);
        }

      if (!GTK_IS_WIDGET (object))
        {
          g_printerr (_("Object is not previewable.\n"));
          exit (1);
        }

      toplevel = object;
    }

  return g_object_ref_sink (toplevel);
}

static GObject *
get_toplevel_from_string (GladePreviewerApp *app, gchar *name, gchar *string, gsize size)
{
  gchar *wd = NULL;
  GObject *retval = NULL;

  /* We need to change the working directory so builder get a chance to load resources */
  if (app->file_name)
    {
      gchar *dirname = g_path_get_dirname (app->file_name);
      wd = g_get_current_dir ();
      g_chdir (dirname);
      g_free (dirname);
    }

  /* We use template flag as a hint since the user can turn on and off template
   * while the preview is live.
   */
  if (app->is_template)
    retval = glade_preview_template_object_new (string, size,
                                                  glade_previewer_connect_function,
                                                  app->preview);

  if (!retval)
    {
      GtkBuilder *builder = gtk_builder_new ();
      GError *error = NULL;

      /* We do not know if its a template yet */
      app->is_template = FALSE;

      if (gtk_builder_add_from_string (builder, string, size, &error))
        {
          gtk_builder_connect_signals_full (builder,
                                            glade_previewer_connect_function,
                                            app->preview);
          retval = get_toplevel (builder, name);
        }
      else
        {
          if (error->code == GTK_BUILDER_ERROR_UNHANDLED_TAG &&
              (retval = glade_preview_template_object_new (string, size,
                                                             glade_previewer_connect_function,
                                                             app->preview)))
            {
              /* At this point we know it is a template, so keep a hint for next time */
              app->is_template = TRUE;
            }
          else
            {
              gchar *message = g_strdup_printf (_("Couldn't load builder definition: %s"), error->message);
              glade_previewer_set_message (app->preview, GTK_MESSAGE_ERROR, message);
              g_free (message);
            }

          g_error_free (error);
        }

      g_object_unref (builder);
    }

  /* restore directory */
  if (wd)
    {
      g_chdir (wd);
      g_free (wd);
    }
  
  return retval;
}

static gchar *
read_buffer (GIOChannel * source)
{
  gchar *buffer;
  gchar *token;
  gchar *tmp;
  GError *error = NULL;

  if (g_io_channel_read_line (source, &token, NULL, NULL, &error) !=
      G_IO_STATUS_NORMAL)
    {
      g_printerr (_("Error: %s.\n"), error->message);
      g_error_free (error);
      exit (1);
    }

  /* Check for quit token */
  if (g_strcmp0 (QUIT_TOKEN, token) == 0)
    {
      g_free (token);
      return NULL;
    }

  /* Loop to load the UI */
  buffer = g_strdup (token);
  do
    {
      g_free (token);
      if (g_io_channel_read_line (source, &token, NULL, NULL, &error) !=
          G_IO_STATUS_NORMAL)
        {
          g_printerr (_("Error: %s.\n"), error->message);
          g_error_free (error);
          exit (1);
        }
      tmp = buffer;
      buffer = g_strconcat (buffer, token, NULL);
      g_free (tmp);
    }
  while (g_strcmp0 ("</interface>\n", token) != 0);
  g_free (token);

  return buffer;
}

static gboolean
on_data_incoming (GIOChannel *source, GIOCondition condition, gpointer data)
{
  GladePreviewerApp *app = data;
  GObject *new_widget;
  gchar *buffer;

  buffer = read_buffer (source);
  if (buffer == NULL)
    {
      gtk_main_quit ();
      return FALSE;
    }

  if (condition & G_IO_HUP)
    {
      g_printerr (_("Broken pipe!\n"));
      exit (1);
    }

  /* We have an update */
  if (g_str_has_prefix (buffer, UPDATE_TOKEN))
    {
     gchar **split_buffer = g_strsplit_set (buffer + UPDATE_TOKEN_SIZE, "\n", 2);

      if (!split_buffer)
        {
          g_free (buffer);
          return FALSE;
        }

      new_widget = get_toplevel_from_string (app, split_buffer[0], split_buffer[1], -1);

      g_strfreev (split_buffer);
    }
  else
    {
      new_widget = get_toplevel_from_string (app, app->toplevel, buffer, -1);
    }

  if (new_widget)
    {
      glade_previewer_set_widget (app->preview, GTK_WIDGET (new_widget));
      gtk_widget_show (GTK_WIDGET (new_widget));
    }

  glade_previewer_present (app->preview);
  
  g_free (buffer);
  
  return TRUE;
}

static GladePreviewerApp *
glade_previewer_app_new (gchar *filename, gchar *toplevel)
{
  GladePreviewerApp *app = g_new0 (GladePreviewerApp, 1);

  app->preview = GLADE_PREVIEWER (glade_previewer_new ());
  g_object_ref_sink (app->preview);

  app->file_name = g_strdup (filename);
  app->toplevel = g_strdup (toplevel);

  return app;
}

static void
glade_previewer_free (GladePreviewerApp *app)
{
  g_object_unref (app->preview);
  g_free (app->file_name);
  g_free (app->toplevel);
  g_free (app);
}

static gboolean listen = FALSE;
static gboolean version = FALSE;
static gboolean slideshow = FALSE;
static gboolean template = FALSE;
static gboolean print_handler = FALSE;
static gchar *file_name = NULL;
static gchar *toplevel_name = NULL;
static gchar *css_file_name = NULL;
static gchar *screenshot_file_name = NULL;

static GOptionEntry option_entries[] =
{
    {"filename", 'f', 0, G_OPTION_ARG_FILENAME, &file_name, N_("Name of the file to preview"), "FILENAME"},
    {"template", 0, 0, G_OPTION_ARG_NONE, &template, N_("Creates dummy widget class to load a template"), NULL},
    {"toplevel", 't', 0, G_OPTION_ARG_STRING, &toplevel_name, N_("Name of the toplevel to preview"), "TOPLEVELNAME"},
    {"screenshot", 0, 0, G_OPTION_ARG_FILENAME, &screenshot_file_name, N_("File name to save a screenshot"), NULL},
    {"css", 0, 0, G_OPTION_ARG_FILENAME, &css_file_name, N_("CSS file to use"), NULL},
    {"listen", 'l', 0, G_OPTION_ARG_NONE, &listen, N_("Listen standard input"), NULL},
    {"slideshow", 0, 0, G_OPTION_ARG_NONE, &slideshow, N_("make a slideshow of every toplevel widget by adding them in a GtkStack"), NULL},
    {"print-handler", 0, 0, G_OPTION_ARG_NONE, &print_handler, N_("Print handlers signature on invocation"), NULL},
    {"version", 'v', 0, G_OPTION_ARG_NONE, &version, N_("Display previewer version"), NULL},
    {NULL}
};

int
main (int argc, char **argv)
{
  GladePreviewerApp *app;
  GOptionContext *context;
  GError *error = NULL;
  GObject *toplevel = NULL;

#ifdef ENABLE_NLS
  setlocale (LC_ALL, "");
  bindtextdomain (GETTEXT_PACKAGE, glade_app_get_locale_dir ());
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
  textdomain (GETTEXT_PACKAGE);
#endif

  context = g_option_context_new (_("- previews a glade UI definition"));
  g_option_context_add_main_entries (context, option_entries, GETTEXT_PACKAGE);
  g_option_context_add_group (context, gtk_get_option_group (TRUE));
  
  if (!g_option_context_parse (context, &argc, &argv, &error))
    {
      g_printerr (_("%s\nRun '%s --help' to see a full list of available command line "
                   "options.\n"), error->message, argv[0]);
      g_error_free (error);
      g_option_context_free (context);
      return 1;
    }

  g_option_context_free (context);

  if (version)
    {
      g_print ("glade-previewer " VERSION "\n");
      return 0;
    }

  if (!listen && !file_name)
    {
      g_printerr (_("Either --listen or --filename must be specified.\n"));
      return 0;
    }

  gtk_init (&argc, &argv);
  glade_app_get ();

  app = glade_previewer_app_new (file_name, toplevel_name);

  app->is_template = template;

  if (print_handler)
    glade_previewer_set_print_handlers (GLADE_PREVIEWER (app->preview), TRUE);

  if (css_file_name)
    glade_previewer_set_css_file (app->preview, css_file_name);

  if (listen)
    {
#ifdef WINDOWS
      GIOChannel *input = g_io_channel_win32_new_fd (fileno (stdin));
#else
      GIOChannel *input = g_io_channel_unix_new (fileno (stdin));
#endif

      g_io_add_watch (input, G_IO_IN | G_IO_HUP, on_data_incoming, app);

      gtk_main ();
    }
  else if (template)
    {
      gchar *contents = NULL;
      gsize size;

      if (g_file_get_contents (file_name, &contents, &size, NULL))
        toplevel = get_toplevel_from_string (app, NULL, contents, size);

      g_free (contents);
    }
  else if (file_name)
    {
      GtkBuilder *builder = gtk_builder_new ();
      GError *error = NULL;

      /* Use from_file() function gives builder a chance to know where to load resources from */
      if (!gtk_builder_add_from_file (builder, app->file_name, &error))
        {
          g_printerr (_("Couldn't load builder definition: %s"), error->message);
          g_error_free (error);
          return 1;
        }

      if (slideshow)
        {
          GSList *objects = gtk_builder_get_objects (builder);

          glade_previewer_set_slideshow_widgets (app->preview, objects);
          glade_previewer_present (app->preview);

          if (screenshot_file_name)
            glade_previewer_slideshow_save (app->preview, screenshot_file_name);
          else
            gtk_main ();

          g_slist_free (objects);
        }
      else
        {
          toplevel = get_toplevel (builder, toplevel_name);

          gtk_builder_connect_signals_full (builder,
                                            glade_previewer_connect_function,
                                            app->preview);
        }

      g_object_unref (builder);
    }

  if (toplevel)
    {
      glade_previewer_set_widget (app->preview, GTK_WIDGET (toplevel));
      g_object_unref (toplevel);
      glade_previewer_present (app->preview);

      if (screenshot_file_name)
        glade_previewer_screenshot (app->preview, TRUE, screenshot_file_name);
      else
        gtk_main ();
    }

  /* free unused resources */
  g_free (file_name);
  g_free (toplevel_name);
  g_free (css_file_name);
  g_free (screenshot_file_name);
  glade_previewer_free (app);

  return 0;
}
