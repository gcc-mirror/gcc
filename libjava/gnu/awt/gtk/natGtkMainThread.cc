// This file was created by `gcjh -stubs'. -*- c++ -*-
//
// This file is intended to give you a head start on implementing native
// methods using CNI.
// Be aware: running `gcjh -stubs ' once more for this class may
// overwrite any edits you have made to this file.

#include <gnu/awt/gtk/GtkMainThread.h>
#include <gcj/cni.h>

#include <gtk/gtk.h>


void
gnu::awt::gtk::GtkMainThread::gtkMain ()
{
  GDK_THREADS_ENTER ();
  gtk_main ();
  GDK_THREADS_LEAVE ();
}


