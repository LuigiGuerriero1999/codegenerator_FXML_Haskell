/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * glade-design-layout.c
 *
 * Copyright (C) 2006-2007 Vincent Geddes
 *
 * Authors:
 *   Vincent Geddes <vgeddes@gnome.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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
 */

#include "config.h"

#include "glade.h"
#include "glade-design-layout.h"
#include "glade-accumulators.h"
#include "glade-marshallers.h"

#include <gtk/gtk.h>

#define GLADE_DESIGN_LAYOUT_GET_PRIVATE(object) (G_TYPE_INSTANCE_GET_PRIVATE ((object),  \
						 GLADE_TYPE_DESIGN_LAYOUT,               \
						 GladeDesignLayoutPrivate))

#define OUTLINE_WIDTH     4
#define PADDING          12

typedef enum 
{
	ACTIVITY_NONE,
	ACTIVITY_RESIZE_WIDTH,
	ACTIVITY_RESIZE_HEIGHT,
	ACTIVITY_RESIZE_WIDTH_AND_HEIGHT
} Activity;

typedef enum 
{
	REGION_INSIDE,
	REGION_EAST,
	REGION_SOUTH,
	REGION_SOUTH_EAST,
	REGION_WEST_OF_SOUTH_EAST,
	REGION_NORTH_OF_SOUTH_EAST,
	REGION_OUTSIDE
} PointerRegion;

enum
{
	WIDGET_EVENT,
	LAST_SIGNAL
};

struct _GladeDesignLayoutPrivate
{
	GdkWindow *event_window;
	
	GdkCursor *cursor_resize_bottom;
	GdkCursor *cursor_resize_right;
	GdkCursor *cursor_resize_bottom_right;

	/* state machine */
	Activity        activity;   /* the current activity */
	GtkRequisition *current_size_request;
	gint dx;                    /* child.width - event.pointer.x   */
	gint dy;                    /* child.height - event.pointer.y  */
	gint new_width;             /* user's new requested width */
	gint new_height;            /* user's new requested height */
};

static guint              glade_design_layout_signals[LAST_SIGNAL] = {0};

G_DEFINE_TYPE (GladeDesignLayout, glade_design_layout, GTK_TYPE_BIN)

static PointerRegion
glade_design_layout_get_pointer_region (GladeDesignLayout *layout, gint x, gint y)
{
	GladeDesignLayoutPrivate  *priv;
	GtkAllocation              child_allocation;
	PointerRegion              region = REGION_INSIDE;

	priv  = GLADE_DESIGN_LAYOUT_GET_PRIVATE (layout);

	gtk_widget_get_allocation (gtk_bin_get_child (GTK_BIN (layout)), &child_allocation);

	if ((x >= (child_allocation.x + child_allocation.width)) &&
	    (x < (child_allocation.x + child_allocation.width + OUTLINE_WIDTH)))
	{
		if ((y < (child_allocation.y + child_allocation.height - OUTLINE_WIDTH)) &&
		    (y >= child_allocation.y - OUTLINE_WIDTH))
			region = REGION_EAST;

		else if ((y < (child_allocation.y + child_allocation.height)) &&
			 (y >= (child_allocation.y + child_allocation.height - OUTLINE_WIDTH)))
			region = REGION_NORTH_OF_SOUTH_EAST;

		else if ((y < (child_allocation.y + child_allocation.height + OUTLINE_WIDTH)) &&
			 (y >= (child_allocation.y + child_allocation.height)))
			region = REGION_SOUTH_EAST;
	}
	else if ((y >= (child_allocation.y + child_allocation.height)) &&
		 (y < (child_allocation.y + child_allocation.height + OUTLINE_WIDTH)))
	{
		if ((x < (child_allocation.x + child_allocation.width - OUTLINE_WIDTH)) &&
		    (x >= child_allocation.x - OUTLINE_WIDTH))
			region = REGION_SOUTH;

		else if ((x < (child_allocation.x + child_allocation.width)) &&
			 (x >= (child_allocation.x + child_allocation.width - OUTLINE_WIDTH)))
			region = REGION_WEST_OF_SOUTH_EAST;

		else if ((x < (child_allocation.x + child_allocation.width + OUTLINE_WIDTH)) &&
			 (x >= (child_allocation.x + child_allocation.width)))
			region = REGION_SOUTH_EAST;
	}

	if (x < PADDING || y < PADDING ||
	    x >= (child_allocation.x + child_allocation.width + OUTLINE_WIDTH) ||
	    y >= (child_allocation.y + child_allocation.height + OUTLINE_WIDTH))
		region = REGION_OUTSIDE;

	return region;
}


/* this handler ensures that the user cannot
 * resize a widget below it minimum acceptable size */
static void
child_size_request_handler (GtkWidget         *widget,
			    GtkRequisition    *req,
			    GladeDesignLayout *layout)
{
	GladeDesignLayoutPrivate *priv;
	GtkAllocation allocation;
	GtkRequisition requisition;
	gint new_width, new_height;
	gint old_width, old_height;
		
	priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (layout);

	priv->current_size_request->width = req->width;
	priv->current_size_request->height = req->height;

	gtk_widget_get_allocation (widget, &allocation);
	new_width = allocation.width;
	new_height = allocation.height;

	if (req->width > new_width)
		new_width = req->width;

	if (req->height > new_height)
		new_height = req->height;

	if ((new_width != allocation.width) ||
	    (new_height != allocation.height))
	{
		gtk_widget_size_request (widget, &requisition);
		old_width = requisition.width;
		old_height = requisition.height;

		gtk_widget_set_size_request (widget,
					     (old_width > new_width) ? old_width : new_width,
					     (old_height > new_height) ? old_height : new_height);
	}
	
	gtk_widget_queue_draw (GTK_WIDGET (layout));
	
}

static gboolean
glade_design_layout_leave_notify_event (GtkWidget *widget, GdkEventCrossing *ev)
{
	GtkWidget *child;
	GladeDesignLayoutPrivate *priv;

	if ((child = gtk_bin_get_child (GTK_BIN (widget))) == NULL)
		return FALSE;

	priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (widget);

	if (priv->activity == ACTIVITY_NONE)
		gdk_window_set_cursor (priv->event_window, NULL);
	
	return FALSE;
}


static void
glade_design_layout_update_child (GladeDesignLayout *layout, 
				  GtkWidget         *child,
				  GtkAllocation     *allocation)
{
	GladeDesignLayoutPrivate *priv;
	GladeWidget *gchild;

	priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (layout);

	/* Update GladeWidget metadata */
	gchild = glade_widget_get_from_gobject (child);
	g_object_set (gchild, 
		      "toplevel-width", allocation->width,
		      "toplevel-height", allocation->height,
		      NULL);

	gtk_widget_size_allocate (child, allocation);
	gtk_widget_queue_resize (GTK_WIDGET (layout));
}





/* A temp data struct that we use when looking for a widget inside a container
 * we need a struct, because the forall can only pass one pointer
 */
typedef struct {
	gint x;
	gint y;
	gboolean   any;
	GtkWidget *found;
	GtkWidget *toplevel;
} GladeFindInContainerData;

static void
glade_design_layout_find_inside_container (GtkWidget *widget, GladeFindInContainerData *data)
{
	GtkAllocation allocation;
	gint x;
	gint y;

	gtk_widget_translate_coordinates (data->toplevel, widget, data->x, data->y, &x, &y);
	gtk_widget_get_allocation (widget, &allocation);

	if (gtk_widget_get_mapped(widget) &&
	    x >= 0 && x < allocation.width && y >= 0 && y < allocation.height)
	{
		if (glade_widget_get_from_gobject (widget) || data->any)
			data->found = widget;
		else if (GTK_IS_CONTAINER (widget))
		{
			/* Recurse and see if any project objects exist
			 * under this container that is not in the project
			 * (mostly for dialog buttons).
			 */
			GladeFindInContainerData search;
			search.x = data->x;
			search.y = data->y;
			search.toplevel = data->toplevel;
			search.found = NULL;

			gtk_container_forall (GTK_CONTAINER (widget), (GtkCallback)
					      glade_design_layout_find_inside_container, &search);

			data->found = search.found;
		}
	}
}

static GladeWidget *
glade_design_layout_deepest_gwidget_at_position (GtkContainer *toplevel,
						 GtkContainer *container,
						 gint top_x, gint top_y)
{
	GladeFindInContainerData data;
	GladeWidget *ret_widget = NULL;

	data.x = top_x;
	data.y = top_y;
	data.any = FALSE;
	data.toplevel = GTK_WIDGET (toplevel);
	data.found = NULL;

	gtk_container_forall (container, (GtkCallback)
			      glade_design_layout_find_inside_container, &data);

	if (data.found)
	{
		if (GTK_IS_CONTAINER (data.found))
			ret_widget = glade_design_layout_deepest_gwidget_at_position
				(toplevel, GTK_CONTAINER (data.found), top_x, top_y);
		else
			ret_widget = glade_widget_get_from_gobject (data.found);
	}

	if (!ret_widget)
		ret_widget = glade_widget_get_from_gobject (container);

	return ret_widget;
}


static GtkWidget *
glade_design_layout_deepest_widget_at_position (GtkContainer *toplevel,
						GtkContainer *container,
						gint top_x, gint top_y)
{
	GladeFindInContainerData data;
	GtkWidget *ret_widget = NULL;

	data.x = top_x;
	data.y = top_y;
	data.any = TRUE;
	data.toplevel = GTK_WIDGET (toplevel);
	data.found = NULL;

	gtk_container_forall (container, (GtkCallback)
			      glade_design_layout_find_inside_container, &data);

	if (data.found && GTK_IS_CONTAINER (data.found))
		ret_widget = glade_design_layout_deepest_widget_at_position
			(toplevel, GTK_CONTAINER (data.found), top_x, top_y);
	else if (data.found)
		ret_widget = data.found;
	else
		ret_widget = GTK_WIDGET (container);

	return ret_widget;
}

static gboolean
glade_design_layout_motion_notify_event (GtkWidget *widget, GdkEventMotion *ev)
{
	GtkWidget                *child;
	GladeDesignLayoutPrivate *priv;
	GladeWidget              *child_glade_widget;
	PointerRegion        region;
	GtkAllocation             allocation;
	gint                      x, y;
	gint                      new_width, new_height;

	if ((child = gtk_bin_get_child (GTK_BIN (widget))) == NULL)
		return FALSE;
	
	priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (widget);

	gdk_window_get_pointer (priv->event_window, &x, &y, NULL);
	
	child_glade_widget = glade_widget_get_from_gobject (child);
	gtk_widget_get_allocation (child, &allocation);

	if (priv->activity == ACTIVITY_RESIZE_WIDTH)
	{
		new_width = x - priv->dx - PADDING - OUTLINE_WIDTH;
			
		if (new_width < priv->current_size_request->width)
			new_width = priv->current_size_request->width;
		
		allocation.width = new_width;

		glade_design_layout_update_child (GLADE_DESIGN_LAYOUT (widget), 
						  child, &allocation);
	}
	else if (priv->activity == ACTIVITY_RESIZE_HEIGHT)
	{
		new_height = y - priv->dy - PADDING - OUTLINE_WIDTH;
		
		if (new_height < priv->current_size_request->height)
			new_height = priv->current_size_request->height;
		
		allocation.height = new_height;

		glade_design_layout_update_child (GLADE_DESIGN_LAYOUT (widget), 
						  child, &allocation);
	}
	else if (priv->activity == ACTIVITY_RESIZE_WIDTH_AND_HEIGHT)
	{
		new_width =  x - priv->dx - PADDING - OUTLINE_WIDTH;
		new_height = y - priv->dy - PADDING - OUTLINE_WIDTH;
		
		if (new_width < priv->current_size_request->width)
			new_width = priv->current_size_request->width;
		if (new_height < priv->current_size_request->height)
			new_height = priv->current_size_request->height;
		
		
		allocation.height = new_height;
		allocation.width  = new_width;

		glade_design_layout_update_child (GLADE_DESIGN_LAYOUT (widget), 
						  child, &allocation);
	}
	else
	{
		region = glade_design_layout_get_pointer_region (GLADE_DESIGN_LAYOUT (widget), x, y);
		
		if (region == REGION_EAST)
			gdk_window_set_cursor (priv->event_window, priv->cursor_resize_right);
		
		else if (region == REGION_SOUTH)
			gdk_window_set_cursor (priv->event_window, priv->cursor_resize_bottom);
		
		else if (region == REGION_SOUTH_EAST ||
			 region == REGION_WEST_OF_SOUTH_EAST ||
			 region == REGION_NORTH_OF_SOUTH_EAST)
			gdk_window_set_cursor (priv->event_window, priv->cursor_resize_bottom_right);

		else if (region == REGION_OUTSIDE)
			gdk_window_set_cursor (priv->event_window, NULL);
	}
	
	return FALSE;
}

static gboolean
glade_design_layout_button_press_event (GtkWidget *widget, GdkEventButton *ev)
{
	GtkWidget                *child;
	GtkAllocation             child_allocation;
	PointerRegion             region;
	GladeDesignLayoutPrivate *priv;
	gint                      x, y;

	if ((child = gtk_bin_get_child (GTK_BIN (widget))) == NULL)
		return FALSE;

	priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (widget);

	gdk_window_get_pointer (priv->event_window, &x, &y, NULL);
	region = glade_design_layout_get_pointer_region (GLADE_DESIGN_LAYOUT (widget), x, y);

	if (((GdkEventButton *) ev)->button == 1) 
	{
		gtk_widget_get_allocation (child, &child_allocation);
		priv->dx = x - (child_allocation.x + child_allocation.width);
		priv->dy = y - (child_allocation.y + child_allocation.height);
		
		if (region == REGION_EAST) 
		{
			priv->activity = ACTIVITY_RESIZE_WIDTH;
			gdk_window_set_cursor (priv->event_window, priv->cursor_resize_right);
		} 
		if (region == REGION_SOUTH) 
		{
			priv->activity = ACTIVITY_RESIZE_HEIGHT;
			gdk_window_set_cursor (priv->event_window, priv->cursor_resize_bottom);
		} 
		if (region == REGION_SOUTH_EAST) 
		{
			priv->activity = ACTIVITY_RESIZE_WIDTH_AND_HEIGHT;
			gdk_window_set_cursor (priv->event_window, priv->cursor_resize_bottom_right);
		} 
		if (region == REGION_WEST_OF_SOUTH_EAST) 
		{
			priv->activity = ACTIVITY_RESIZE_WIDTH_AND_HEIGHT;
			gdk_window_set_cursor (priv->event_window, priv->cursor_resize_bottom_right);
		} 
		if (region == REGION_NORTH_OF_SOUTH_EAST) 
		{
			priv->activity = ACTIVITY_RESIZE_WIDTH_AND_HEIGHT;
			gdk_window_set_cursor (priv->event_window, priv->cursor_resize_bottom_right);
		}
	}
		
	return FALSE;
}

static gboolean
glade_design_layout_button_release_event (GtkWidget *widget, GdkEventButton *ev)
{
	GladeDesignLayoutPrivate *priv;
	GtkWidget                *child;

	if ((child = gtk_bin_get_child (GTK_BIN (widget))) == NULL)
		return FALSE;

	priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (widget);

	priv->activity = ACTIVITY_NONE;
	gdk_window_set_cursor (priv->event_window, NULL);

	return FALSE;
}

static void
glade_design_layout_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
	GladeDesignLayoutPrivate *priv;
	GtkRequisition child_requisition;
	GtkWidget *child;
	GladeWidget *gchild;
	gint child_width = 0;
	gint child_height = 0;
	guint border_width = 0;

	priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (widget);

	requisition->width = 0;
	requisition->height = 0;

	child = gtk_bin_get_child (GTK_BIN (widget));

	if (child && gtk_widget_get_visible (child))
	{
		gchild = glade_widget_get_from_gobject (child);
		g_assert (gchild);

		gtk_widget_size_request (child, &child_requisition);

		g_object_get (gchild, 
			      "toplevel-width", &child_width,
			      "toplevel-height", &child_height,
			      NULL);
			
		child_width = MAX (child_width, child_requisition.width);
		child_height = MAX (child_height, child_requisition.height);

		requisition->width = MAX (requisition->width,
		                          2 * PADDING + child_width + 2 * OUTLINE_WIDTH);

		requisition->height = MAX (requisition->height,
		                          2 * PADDING + child_height + 2 * OUTLINE_WIDTH);
	}

	border_width = gtk_container_get_border_width (GTK_CONTAINER (widget));
	requisition->width += border_width * 2;
	requisition->height += border_width * 2;
}

static void
glade_design_layout_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
	GladeDesignLayoutPrivate *priv;
	GladeWidget *gchild;
	GtkRequisition child_requisition;
	GtkAllocation child_allocation, widget_allocation;
	GtkWidget *child;
	gint border_width;
	gint child_width = 0;
	gint child_height = 0;

	priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (widget);

	gtk_widget_set_allocation (widget, allocation);
	border_width = gtk_container_get_border_width (GTK_CONTAINER (widget));

	if (gtk_widget_get_realized (widget))
	{
		if (priv->event_window)
			gdk_window_move_resize (priv->event_window,
						allocation->x,
						allocation->y,
						allocation->width,
						allocation->height);

	}

	child = gtk_bin_get_child (GTK_BIN (widget));

	if (child && gtk_widget_get_visible (child))
	{
		gchild = glade_widget_get_from_gobject (child);
		g_assert (gchild);

		gtk_widget_get_child_requisition (child, &child_requisition);

		g_object_get (gchild, 
			      "toplevel-width", &child_width,
			      "toplevel-height", &child_height,
			      NULL);

		child_width = MAX (child_width, child_requisition.width);
		child_height = MAX (child_height, child_requisition.height);

		gtk_widget_get_allocation (widget, &widget_allocation);
		child_allocation.x = widget_allocation.x + border_width + PADDING + OUTLINE_WIDTH;
		child_allocation.y = widget_allocation.y + border_width + PADDING + OUTLINE_WIDTH;

		child_allocation.width = child_width - 2 * border_width;
		child_allocation.height = child_height - 2 * border_width;

		gtk_widget_size_allocate (child, &child_allocation);
	}
}

static void
glade_design_layout_map (GtkWidget *widget)
{
	GladeDesignLayoutPrivate *priv;

	priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (widget);

	if (priv->event_window)
		gdk_window_show (priv->event_window);

	GTK_WIDGET_CLASS (glade_design_layout_parent_class)->map (widget);

}

static void
glade_design_layout_unmap (GtkWidget *widget)
{
	GladeDesignLayoutPrivate *priv;	

	priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (widget);

	GTK_WIDGET_CLASS (glade_design_layout_parent_class)->unmap (widget);

	if (priv->event_window)
		gdk_window_hide (priv->event_window);
}

static void
glade_design_layout_realize (GtkWidget *widget)
{
	GladeDesignLayoutPrivate *priv;
	GtkAllocation allocation;
	GtkStyle *style;
	GdkWindow *window;
	GdkWindowAttr attributes;
	gint attributes_mask;
	guint border_width;

	priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (widget);

	gtk_widget_set_realized (widget, TRUE);

	border_width = gtk_container_get_border_width (GTK_CONTAINER (widget));

	gtk_widget_get_allocation (widget, &allocation);
	attributes.x = allocation.x;
	attributes.y = allocation.y;
	attributes.width = allocation.width;
	attributes.height = allocation.height;

	attributes.window_type = GDK_WINDOW_CHILD;
	attributes.wclass = GDK_INPUT_ONLY;
	attributes.event_mask = 
		gtk_widget_get_events (widget) |
		GDK_POINTER_MOTION_MASK        |
		GDK_POINTER_MOTION_HINT_MASK   |
		GDK_BUTTON_PRESS_MASK          |
		GDK_BUTTON_RELEASE_MASK        |
		GDK_EXPOSURE_MASK              |
		GDK_ENTER_NOTIFY_MASK          |
		GDK_LEAVE_NOTIFY_MASK;
	attributes_mask = GDK_WA_X | GDK_WA_Y;

	window = gtk_widget_get_parent_window (widget);
	gtk_widget_set_window (widget, g_object_ref (window));

	priv->event_window = gdk_window_new (gtk_widget_get_parent_window (widget),
					     &attributes, attributes_mask);
	gdk_window_set_user_data (priv->event_window, widget);

	gtk_widget_style_attach (widget);
	style = gtk_widget_get_style (widget);
	gtk_style_set_background (style, window, GTK_STATE_NORMAL);
}

static void
glade_design_layout_unrealize (GtkWidget *widget)
{
	GladeDesignLayoutPrivate *priv;

	priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (widget);

	if (priv->event_window)
	{
		gdk_window_set_user_data (priv->event_window, NULL);
		gdk_window_destroy (priv->event_window);
		priv->event_window = NULL;
	}

	GTK_WIDGET_CLASS (glade_design_layout_parent_class)->unrealize (widget);

}

static void
glade_design_layout_add (GtkContainer *container, GtkWidget *widget)
{
	GladeDesignLayout *layout;

	layout = GLADE_DESIGN_LAYOUT (container);

	layout->priv->current_size_request->width = 0;
	layout->priv->current_size_request->height = 0;

	g_signal_connect (G_OBJECT (widget), "size-request", 
			  G_CALLBACK (child_size_request_handler),
			  container);

	GTK_CONTAINER_CLASS (glade_design_layout_parent_class)->add (container, widget);

	gdk_window_lower (layout->priv->event_window);
}

static void
glade_design_layout_remove (GtkContainer *container, GtkWidget *widget)
{
	g_signal_handlers_disconnect_by_func(G_OBJECT (widget), 
					     G_CALLBACK (child_size_request_handler), 
					     container);	

	GTK_CONTAINER_CLASS (glade_design_layout_parent_class)->remove (container, widget);
}

static void
glade_design_layout_dispose (GObject *object)
{
	GladeDesignLayoutPrivate *priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (object);

	if (priv->cursor_resize_bottom != NULL) {
		gdk_cursor_unref (priv->cursor_resize_bottom);
		priv->cursor_resize_bottom = NULL;
	}
	if (priv->cursor_resize_right != NULL) {
		gdk_cursor_unref (priv->cursor_resize_right);	
		priv->cursor_resize_right = NULL;
	}
	if (priv->cursor_resize_bottom_right != NULL) {	
		gdk_cursor_unref (priv->cursor_resize_bottom_right);
		priv->cursor_resize_bottom_right = NULL;
	}

	G_OBJECT_CLASS (glade_design_layout_parent_class)->dispose (object);
}

static void
glade_design_layout_finalize (GObject *object)
{
	GladeDesignLayoutPrivate *priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (object);

	g_slice_free (GtkRequisition, priv->current_size_request);
	
	G_OBJECT_CLASS (glade_design_layout_parent_class)->finalize (object);
}

static inline void
draw_frame (GtkWidget *widget,
	    cairo_t   *cr,
	    int x, int y, int w, int h)
{	
	cairo_set_line_width (cr, OUTLINE_WIDTH);
	cairo_set_line_join (cr, CAIRO_LINE_JOIN_ROUND);
	cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);

	gdk_cairo_set_source_color (cr, &gtk_widget_get_style (widget)->bg[GTK_STATE_SELECTED]);

	/* rectangle */
	cairo_move_to (cr, x, y);
	cairo_rel_line_to (cr, 0, h);
	cairo_rel_line_to (cr, w, 0);
	cairo_rel_line_to (cr, 0, -h);
	cairo_close_path (cr);	
	cairo_stroke (cr);
	
#if 0
	/* shadow effect */
	cairo_set_source_rgba (cr, 0, 0, 0, 0.04);
		
	cairo_move_to (cr, x + OUTLINE_WIDTH, y + h + OUTLINE_WIDTH);
	cairo_rel_line_to (cr, w, 0);
	cairo_rel_line_to (cr, 0, -h);
	cairo_stroke (cr);  	      
#endif
}

static gboolean
glade_design_layout_expose_event (GtkWidget *widget, GdkEventExpose *ev)
{
	GladeDesignLayout *layout;
	GtkStyle *style;
	GtkAllocation allocation, child_allocation;
	GtkWidget *child;
	GdkWindow *window;
	gint x, y, w, h;
	gint border_width;
	cairo_t *cr;

	layout = GLADE_DESIGN_LAYOUT (widget);

	border_width = gtk_container_get_border_width (GTK_CONTAINER (widget));

	window = gtk_widget_get_window (widget);
	style = gtk_widget_get_style (widget);
	gtk_widget_get_allocation (widget, &allocation);

	child = gtk_bin_get_child (GTK_BIN (widget));

	cr = gdk_cairo_create (window);

	/* draw a white widget background */
	glade_utils_cairo_draw_rectangle (cr,
					  &style->base [gtk_widget_get_state (widget)],
					  TRUE,
					  allocation.x + border_width,
					  allocation.y + border_width,
					  allocation.width - 2 * border_width,
					  allocation.height - 2 * border_width);

	if (child && gtk_widget_get_visible (child))
	{
		/* draw frame */
		gtk_widget_get_allocation (child, &child_allocation);
		x = child_allocation.x - OUTLINE_WIDTH / 2;
		y = child_allocation.y - OUTLINE_WIDTH / 2;
		w = child_allocation.width + OUTLINE_WIDTH;
		h = child_allocation.height + OUTLINE_WIDTH;
		
		draw_frame (widget, cr, x, y, w, h);

		/* draw a filled rectangle in case child does not draw 
		 * it's own background (a GTK_WIDGET_NO_WINDOW child). */
		glade_utils_cairo_draw_rectangle (cr,
						  &style->bg[GTK_STATE_NORMAL],
						  TRUE,
						  x + OUTLINE_WIDTH / 2, y + OUTLINE_WIDTH / 2,
						  w - OUTLINE_WIDTH, h - OUTLINE_WIDTH);

		GTK_WIDGET_CLASS (glade_design_layout_parent_class)->expose_event (widget, ev);
	}	

	cairo_destroy (cr);

	return TRUE;
}

/**
 * glade_design_layout_widget_event:
 * @layout:        A #GladeDesignLayout
 * @event_gwidget: the #GladeWidget that recieved event notification
 * @event:         the #GdkEvent
 *
 * This is called internally by a #GladeWidget recieving an event,
 * it will marshall the event to the proper #GladeWidget according
 * to its position in @layout.
 *
 * Returns: Whether or not the event was handled by the retrieved #GladeWidget
 */
gboolean
glade_design_layout_widget_event (GladeDesignLayout *layout,
				  GladeWidget       *event_gwidget,
				  GdkEvent          *event)
{
	GladeWidget *gwidget;
	GtkWidget   *child;
	gboolean     retval;
	gint         x, y;
	
	gtk_widget_get_pointer (GTK_WIDGET (layout), &x, &y);
	gwidget = glade_design_layout_deepest_gwidget_at_position
		(GTK_CONTAINER (layout), GTK_CONTAINER (layout), x, y);

	child = glade_design_layout_deepest_widget_at_position 
		(GTK_CONTAINER (layout), GTK_CONTAINER (layout), x, y);

	/* First try a placeholder */
	if (GLADE_IS_PLACEHOLDER (child) && event->type != GDK_EXPOSE)
	{
		retval = gtk_widget_event (child, event);

		if (retval)
			return retval;
	}

	/* Then we try a GladeWidget */
	if (gwidget) 
	{
		g_signal_emit_by_name (layout, "widget-event",
				       gwidget, event, &retval);

		if (retval)
			return retval;
	}

	return FALSE;
}

static gboolean
glade_design_layout_widget_event_impl (GladeProject *project,
				       GladeWidget *gwidget,
				       GdkEvent *event)
{

	if (glade_widget_event (gwidget, event))
		return GLADE_WIDGET_EVENT_STOP_EMISSION |
			GLADE_WIDGET_EVENT_RETURN_TRUE;
	else
		return 0;
}

static void
glade_design_layout_init (GladeDesignLayout *layout)
{
	GladeDesignLayoutPrivate *priv;

	layout->priv = priv = GLADE_DESIGN_LAYOUT_GET_PRIVATE (layout);

	gtk_widget_set_has_window (GTK_WIDGET (layout), FALSE);

	priv->event_window = NULL;
	priv->activity = ACTIVITY_NONE;

	priv->current_size_request = g_slice_new0 (GtkRequisition);

	priv->cursor_resize_bottom       = gdk_cursor_new (GDK_BOTTOM_SIDE);
	priv->cursor_resize_right        = gdk_cursor_new (GDK_RIGHT_SIDE);
	priv->cursor_resize_bottom_right = gdk_cursor_new (GDK_BOTTOM_RIGHT_CORNER);

	priv->new_width = -1;
	priv->new_height = -1;
}

static void
glade_design_layout_class_init (GladeDesignLayoutClass *klass)
{
	GObjectClass       *object_class;
	GtkWidgetClass     *widget_class;
	GtkContainerClass  *container_class;

	object_class = G_OBJECT_CLASS (klass);
	widget_class = GTK_WIDGET_CLASS (klass);
	container_class = GTK_CONTAINER_CLASS (klass);

	object_class->dispose               = glade_design_layout_dispose;	
	object_class->finalize              = glade_design_layout_finalize;
	
	container_class->add                = glade_design_layout_add;
	container_class->remove             = glade_design_layout_remove;

	widget_class->motion_notify_event   = glade_design_layout_motion_notify_event;
	widget_class->leave_notify_event    = glade_design_layout_leave_notify_event;
	widget_class->button_press_event    = glade_design_layout_button_press_event;
	widget_class->button_release_event  = glade_design_layout_button_release_event;
	widget_class->realize               = glade_design_layout_realize;
	widget_class->unrealize             = glade_design_layout_unrealize;
	widget_class->map                   = glade_design_layout_map;
	widget_class->unmap                 = glade_design_layout_unmap;
	widget_class->expose_event          = glade_design_layout_expose_event;
	widget_class->size_request          = glade_design_layout_size_request;
	widget_class->size_allocate         = glade_design_layout_size_allocate;

	klass->widget_event          = glade_design_layout_widget_event_impl;

	/**
	 * GladeDesignLayout::widget-event:
	 * @glade_design_layout: the #GladeDesignLayout containing the #GladeWidget.
	 * @signal_editor: the #GladeWidget which received the signal.
	 * @event: the #GdkEvent
	 *
	 * Emitted when a widget event received.
	 * Connect before the default handler to block glade logic,
	 * such as selecting or resizing, by returning #GLADE_WIDGET_EVENT_STOP_EMISSION.
	 * If you want to handle the event after it passed the glade logic,
	 * but before it reaches the widget, you should connect after the default handler.
	 *
	 * Returns: #GLADE_WIDGET_EVENT_STOP_EMISSION flag to abort futher emission of the signal.
	 * #GLADE_WIDGET_EVENT_RETURN_TRUE flag in the last handler
	 * to stop propagating of the event to the appropriate widget.
	 */
	glade_design_layout_signals[WIDGET_EVENT] =
		g_signal_new ("widget-event",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GladeDesignLayoutClass, widget_event),
			      glade_integer_handled_accumulator, NULL,
			      glade_marshal_INT__OBJECT_BOXED,
			      G_TYPE_INT,
			      2,
			      GLADE_TYPE_WIDGET,
			      GDK_TYPE_EVENT | G_SIGNAL_TYPE_STATIC_SCOPE);

	g_type_class_add_private (object_class, sizeof (GladeDesignLayoutPrivate));
}

GtkWidget*
glade_design_layout_new (void)
{
	return g_object_new (GLADE_TYPE_DESIGN_LAYOUT, NULL);
}

