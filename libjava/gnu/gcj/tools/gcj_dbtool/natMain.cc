// natMain -- gcj-dbtool native code.

/* Copyright (C) 2005  Free Software Foundation

   This file is part of gcj-dbtool.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Anthony Green <green@redhat.com>.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/gcj/tools/gcj_dbtool/Main.h>

java::lang::String *
gnu::gcj::tools::gcj_dbtool::Main::getDbPathTail ()
{
  return JvNewStringLatin1 (LIBGCJ_DEFAULT_DATABASE_PATH_TAIL);
}
