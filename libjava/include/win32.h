// win32.h -- Helper functions for Microsoft-flavored OSs.

/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_WIN32_H__
#define __JV_WIN32_H__

#include <windows.h>
#undef STRICT

#undef __INSIDE_CYGWIN__
#include <winsock.h>
#include <gcj/cni.h>
#include <java/util/Properties.h>

extern void _Jv_platform_initialize (void);
extern void _Jv_platform_initProperties (java::util::Properties*);
extern jlong _Jv_platform_gettimeofday ();

inline void
_Jv_platform_close_on_exec (jint)
{
  // Ignore.
}

#define HAVE_BACKTRACE

/* Store up to SIZE return address of the current program state in
   ARRAY and return the exact number of values stored.  */
extern int backtrace (void **__array, int __size);

#endif /* __JV_WIN32_H__ */
