/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

/*#define ENABLE_GTK*/

#include <gcj/cni.h>
#include <java/awt/Toolkit.h>
#ifdef ENABLE_GTK
#include <java/awt/peer/GtkToolkit.h>
#endif

void
java::awt::Toolkit::init()
{
#ifdef ENABLE_GTK
  defaultToolkit = new java::awt::peer::GtkToolkit();
#else
  JvFail("no awt (graphics) toolkit available");
#endif
}
