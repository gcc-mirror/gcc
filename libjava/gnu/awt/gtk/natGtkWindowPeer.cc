// This file was created by `gcjh -stubs'. -*- c++ -*-
//
// This file is intended to give you a head start on implementing native
// methods using CNI.
// Be aware: running `gcjh -stubs ' once more for this class may
// overwrite any edits you have made to this file.

#include <gnu/awt/gtk/GtkWindowPeer.h>
#include <gcj/cni.h>

#include <gtk/gtk.h>

void
gnu::awt::gtk::GtkWindowPeer::toBack ()
{
  GDK_THREADS_ENTER ();
  gdk_window_lower (GTK_WIDGET (ptr)->window);
  GDK_THREADS_LEAVE ();
}

void
gnu::awt::gtk::GtkWindowPeer::toFront ()
{
  GDK_THREADS_ENTER ();
  gdk_window_raise (GTK_WIDGET (ptr)->window);
  GDK_THREADS_LEAVE ();
}

void
gnu::awt::gtk::GtkWindowPeer::create ()
{
  if (ptr == NULL)
    {
      GDK_THREADS_ENTER ();
      ptr = (gnu::gcj::RawData *) gtk_window_new(GTK_WINDOW_POPUP);
      GDK_THREADS_LEAVE ();
    }
    
  gnu::awt::gtk::GtkContainerPeer::create();
}
