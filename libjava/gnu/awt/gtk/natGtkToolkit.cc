// This file was created by `gcjh -stubs'. -*- c++ -*-
//
// This file is intended to give you a head start on implementing native
// methods using CNI.
// Be aware: running `gcjh -stubs ' once more for this class may
// overwrite any edits you have made to this file.

#include <java/awt/Dimension.h>

#include <gnu/awt/gtk/GtkToolkit.h>
#include <gcj/cni.h>

#include <gtk/gtk.h>


// GTK requires the program's argc and argv variables.
extern char **_Jv_argv;
extern int _Jv_argc;

// Call gtk_init.  It is very important that this happen before any other
// gtk calls.
void
gnu::awt::gtk::GtkToolkit::gtkInit ()
{
  // Initialize GLib in thread-safe mode. We assume that GLib is using the
  // same native threads library as libgcj. Refer to comments in 
  // GtkComponentPeer constructor.
  g_thread_init (NULL);
  gtk_init (&_Jv_argc, &_Jv_argv);
}

void
gnu::awt::gtk::GtkToolkit::beep ()
{
  GDK_THREADS_ENTER ();
  gdk_beep ();
  GDK_THREADS_LEAVE ();
}

jint
gnu::awt::gtk::GtkToolkit::getScreenResolution ()
{
  jint res;

  GDK_THREADS_ENTER ();

  res = (int) (gdk_screen_width () / (gdk_screen_width_mm () / 25.4));

  GDK_THREADS_LEAVE ();
  return res;
}

::java::awt::Dimension *
gnu::awt::gtk::GtkToolkit::getScreenSize ()
{
  ::java::awt::Dimension *dim = new ::java::awt::Dimension ();
  
  GDK_THREADS_ENTER ();

  dim->width = gdk_screen_width ();
  dim->height = gdk_screen_height ();

  GDK_THREADS_LEAVE ();
  return dim;
}

void
gnu::awt::gtk::GtkToolkit::sync ()
{
  GDK_THREADS_ENTER ();
  gdk_flush ();
  GDK_THREADS_LEAVE ();
}


