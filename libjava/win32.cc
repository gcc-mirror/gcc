// win32.cc - Helper functions for Microsoft-flavored OSs.

/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <jvm.h>
#include <sys/timeb.h>

#include "platform.h"
#include <java/lang/ArithmeticException.h>

static LONG CALLBACK
win32_exception_handler (LPEXCEPTION_POINTERS e)
{
  if (e->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION)
    _Jv_ThrowNullPointerException();
  else if (e->ExceptionRecord->ExceptionCode == EXCEPTION_INT_DIVIDE_BY_ZERO)
    throw new java::lang::ArithmeticException;
  else
    return EXCEPTION_CONTINUE_SEARCH;
}

// Platform-specific VM initialization.
void
_Jv_platform_initialize (void)
{
  // Initialise winsock for networking
  WSADATA data;
  if (WSAStartup (MAKEWORD (1, 1), &data))
    MessageBox (NULL, "Error initialising winsock library.", "Error",
		MB_OK | MB_ICONEXCLAMATION);
  // Install exception handler
  SetUnhandledExceptionFilter (win32_exception_handler);
}

// gettimeofday implementation.
jlong
_Jv_platform_gettimeofday ()
{
  struct timeb t;
  ftime (&t);
  return t.time * 1000LL + t.millitm;
}

// The following definitions "fake out" mingw to think that -mthreads
// was enabled and that mingwthr.dll was linked. GCJ-compiled
// applications don't need this helper library because we can safely
// detect thread death (return from Thread.run()).

int _CRT_MT = 1;

extern "C" int
__mingwthr_key_dtor (DWORD, void (*) (void *))
{
  // FIXME: for now we do nothing; this causes a memory leak of
  //        approximately 24 bytes per thread created.
  return 0;
}
