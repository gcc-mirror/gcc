// This file was created by `gcjh -stubs'. -*- c++ -*-
//
// This file is intended to give you a head start on implementing native
// methods using CNI.
// Be aware: running `gcjh -stubs ' once more for this class may
// overwrite any edits you have made to this file.

#include <java/awt/Point.h>
#include <java/awt/Dimension.h>

#include <gnu/awt/gtk/GtkComponentPeer.h>
#include <gcj/cni.h>
#include <gtk/gtk.h>

#include "gtkcommon.h"

void
gnu::awt::gtk::GtkComponentPeer::dispose ()
{
  GDK_THREADS_ENTER ();
  gtk_widget_destroy (GTK_WIDGET (ptr));
  GDK_THREADS_LEAVE ();
}


::java::awt::Point *
gnu::awt::gtk::GtkComponentPeer::getLocationOnScreen ()
{
  GDK_THREADS_ENTER ();
  GDK_THREADS_LEAVE ();
  
  // FIXME
  
  return NULL;  
}


::java::awt::Dimension *
gnu::awt::gtk::GtkComponentPeer::getMinimumSize ()
{
  GtkRequisition req;
  ::java::awt::Dimension *dim = new ::java::awt::Dimension ();

  GDK_THREADS_ENTER ();

  gtk_widget_size_request (GTK_WIDGET (ptr), &req);

  GDK_THREADS_LEAVE ();

  dim->width = (jint) req.width;
  dim->height = (jint) req.height;
  return dim;  
}


::java::awt::Dimension *
gnu::awt::gtk::GtkComponentPeer::getPreferredSize ()
{
  return getMinimumSize ();
}


void
gnu::awt::gtk::GtkComponentPeer::requestFocus ()
{
  GDK_THREADS_ENTER ();

  gtk_widget_grab_focus (GTK_WIDGET (ptr));

  GDK_THREADS_LEAVE ();
}


void
gnu::awt::gtk::GtkComponentPeer::setBounds (jint x, jint y, 
                                                  jint width, jint height)
{
  GDK_THREADS_ENTER ();

  GtkWidget *widget = GTK_WIDGET (ptr);
  gtk_widget_set_usize (widget, width, height);
  //gtk_layout_move (GTK_LAYOUT (widget->parent), widget, x, y);

  GDK_THREADS_LEAVE ();
}


void
gnu::awt::gtk::GtkComponentPeer::setCursor (::java::awt::Cursor *)
{
//  JvFail ("gnu::awt::gtk::GtkComponentPeer::setCursor (::java::awt::Cursor *) not implemented");
}


void
gnu::awt::gtk::GtkComponentPeer::setEnabled (jboolean enabled)
{
  GDK_THREADS_ENTER ();

  gtk_widget_set_sensitive (GTK_WIDGET (ptr), enabled);

  GDK_THREADS_LEAVE ();
}


void
gnu::awt::gtk::GtkComponentPeer::setEventMask (jlong)
{
  // TODO
}


void
gnu::awt::gtk::GtkComponentPeer::setFont (::java::awt::Font *)
{
  // TODO
}


void
gnu::awt::gtk::GtkComponentPeer::setForeground (::java::awt::Color *color)
{
  // FIXME: This doesn't work if component is already realized/visible

  GdkColor gcolor;
  _Jv_ConvertAwtColor(color, &gcolor);
  
  GDK_THREADS_ENTER ();

  GtkStyle *style = gtk_widget_get_style (GTK_WIDGET (ptr));

  style->bg[GTK_STATE_NORMAL] = gcolor;
  style->bg[GTK_STATE_ACTIVE] = gcolor;
  style->bg[GTK_STATE_PRELIGHT] = gcolor;
  style->bg[GTK_STATE_SELECTED] = gcolor;
  style->bg[GTK_STATE_INSENSITIVE] = gcolor;
  
  gtk_widget_set_style (GTK_WIDGET (ptr), style);

  GDK_THREADS_LEAVE ();
}


void
gnu::awt::gtk::GtkComponentPeer::setBackground (::java::awt::Color *color)
{
  // FIXME: This doesn't work if component is already realized/visible

  GdkColor gcolor;
  _Jv_ConvertAwtColor(color, &gcolor);

  GDK_THREADS_ENTER ();

  GtkStyle *style = gtk_widget_get_style (GTK_WIDGET (ptr));

  style->bg[GTK_STATE_NORMAL] = gcolor;
  style->bg[GTK_STATE_ACTIVE] = gcolor;
  style->bg[GTK_STATE_PRELIGHT] = gcolor;
  style->bg[GTK_STATE_SELECTED] = gcolor;
  style->bg[GTK_STATE_INSENSITIVE] = gcolor;
  // gtk allows us to set color values for different states of the
  // widget. AWT only provides a single background color, so scale it
  // to get some reasonable values.
//  _Jv_GdkScaleColor (&gcolor, &style->bg[GTK_STATE_ACTIVE], -0.1);
//  _Jv_GdkScaleColor (&gcolor, &style->bg[GTK_STATE_PRELIGHT], 0.2);
//  _Jv_GdkScaleColor (&gcolor, &style->bg[GTK_STATE_SELECTED], -0.2);
//  _Jv_GdkScaleColor (&gcolor, &style->bg[GTK_STATE_INSENSITIVE], -0.2);  

  gtk_widget_set_style (GTK_WIDGET (ptr), style);

  GDK_THREADS_LEAVE ();
}


void
gnu::awt::gtk::GtkComponentPeer::setVisible (jboolean visible)
{
  GDK_THREADS_ENTER ();
  
  GtkWidget *widget = GTK_WIDGET (ptr);

  if (visible)
    gtk_widget_show (widget);
  else
    gtk_widget_hide (widget);

  _Jv_FlushRequests ();

  GDK_THREADS_LEAVE ();
}


void
gnu::awt::gtk::GtkComponentPeer::create ()
{
}


void
gnu::awt::gtk::GtkComponentPeer::realize ()
{
  GDK_THREADS_ENTER ();
  gtk_widget_realize (GTK_WIDGET (ptr));
  GDK_THREADS_LEAVE ();
}
