/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <gnu/java/awt/EmbeddedWindow.h>
#include <gnu/java/awt/peer/EmbeddedWindowPeer.h>
#include <java/awt/peer/ComponentPeer.h>

void
gnu::java::awt::EmbeddedWindow::setWindowPeer (gnu::java::awt::peer::EmbeddedWindowPeer* w)
{
  if (!peer)
    peer = reinterpret_cast< ::java::awt::peer::ComponentPeer *> (w);
}
