// Native Gtk AWT label code.

#include <config.h>

#include <gcj/cni.h>

#include "gtkcommon.h"

#include <gnu/awt/gtk/GtkLabelPeer.h>
#include <java/awt/Label.h>

void
gnu::awt::gtk::GtkLabelPeer::setText (java::lang::String *text)
{
  _Jv_GdkThreadLock sync;
  jsize len = 0;
  if (text)
    len = JvGetStringUTFLength (text);
  // FIXME: this can allocate an unbounded amount.  Should use heap
  // even though it is slower.
  char buf[len + 1];
  if (text)
    JvGetStringUTFRegion (text, 0, len, buf);
  buf[len] = '\0';
  gtk_label_set_text (GTK_LABEL (ptr), buf);
}

void
gnu::awt::gtk::GtkLabelPeer::setAlignment (jint alignment)
{
  using namespace java::awt;

  gfloat value = 0.5;
  if (alignment == Label::LEFT)
    value = 0.0;
  else if (alignment == Label::RIGHT)
    value = 1.0;

  _Jv_GdkThreadLock sync;
  gtk_misc_set_alignment (GTK_MISC (ptr), 0.5f, value);
}

void
gnu::awt::gtk::GtkLabelPeer::create ()
{
  if (! ptr)
    {
      _Jv_GdkThreadLock sync;
      // This is a little inefficient.
      ptr = (gnu::gcj::RawData *) gtk_label_new ("");

      using namespace ::java::awt;
      Label *label = reinterpret_cast<Label *> (awtComponent);
      setText (label->getText ());
      setAlignment (label->getAlignment ());
    }

  gnu::awt::gtk::GtkComponentPeer::create ();
}
