// This file was created by `gcjh -stubs'. -*- c++ -*-
//
// This file is intended to give you a head start on implementing native
// methods using CNI.
// Be aware: running `gcjh -stubs ' once more for this class may
// overwrite any edits you have made to this file.

#include <gnu/awt/gtk/GtkFramePeer.h>
#include <gcj/cni.h>

#include <gtk/gtk.h>

void
gnu::awt::gtk::GtkFramePeer::setIconImage (::java::awt::Image *)
{
  // TODO
}


void
gnu::awt::gtk::GtkFramePeer::setMenuBar (::java::awt::MenuBar *)
{
  // TODO
}


void
gnu::awt::gtk::GtkFramePeer::setResizable (jboolean)
{
  // TODO
}


void
gnu::awt::gtk::GtkFramePeer::setTitle (::java::lang::String *)
{
  // TODO
}

void
gnu::awt::gtk::GtkFramePeer::create ()
{
  if (ptr == NULL)
    {
      GDK_THREADS_ENTER ();
      ptr = (gnu::gcj::RawData *) gtk_window_new(GTK_WINDOW_TOPLEVEL);
      GDK_THREADS_LEAVE ();
    }
    
  gnu::awt::gtk::GtkContainerPeer::create();
}
