/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <gnu/java/awt/EmbeddedWindow.h>
#include <java/awt/Window.h>

::java::awt::Window*
gnu::java::awt::EmbeddedWindow::create (jint window_id, jint width, jint height)
{
  return new ::java::awt::Window (window_id, width, height);
}
