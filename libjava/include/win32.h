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

LONG CALLBACK win32_exception_handler (LPEXCEPTION_POINTERS e);

#endif /* __JV_WIN32_H__ */
