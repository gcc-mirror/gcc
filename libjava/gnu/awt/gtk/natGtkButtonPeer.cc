// Native Gtk AWT button code

#include <config.h>

#include <gcj/cni.h>

#include "gtkcommon.h"
#include <gnu/awt/gtk/GtkButtonPeer.h>
#include <java/awt/Button.h>

void
gnu::awt::gtk::GtkButtonPeer::setLabel (java::lang::String *label)
{
  _Jv_GdkThreadLock sync;
  jsize len = 0;
  if (label)
    len = JvGetStringUTFLength (label);
  char buf[len + 1];
  // FIXME: this can allocate an unbounded amount.  Should use heap
  // even though it is slower.
  if (label)
    JvGetStringUTFRegion (label, 0, len, buf);
  buf[len] = '\0';
  // The button child is a label.
  GtkBin *bin = GTK_BIN (ptr);
  gtk_label_set_text (GTK_LABEL (bin->child), buf);
}

void
gnu::awt::gtk::GtkButtonPeer::create ()
{
  if (! ptr)
    {
      _Jv_GdkThreadLock sync;
      // This is a little inefficient.
      ptr = (gnu::gcj::RawData *) gtk_button_new_with_label ("");

      using namespace ::java::awt;
      Button *button = reinterpret_cast<Button *> (awtComponent);
      setLabel (button->getLabel ());
    }

  gnu::awt::gtk::GtkComponentPeer::create ();
}

//  void
//  gnu::awt::gtk::GtkButtonPeer::clicked (::gnu::gcj::RawData *button_wrap,
//  				       ::gnu::gcj::RawData *peer_wrap)
//  {
//    GtkButtonPeer *button = reinterpret_cast<GtkButtonPeer *> (peer_wrap);
  
//  }
