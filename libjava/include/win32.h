// win32.h -- Helper functions for Microsoft-flavored OSs.

/* Copyright (C) 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_WIN32_H__
#define __JV_WIN32_H__

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#undef STRICT

#include <ws2tcpip.h>
#include <gcj/cni.h>
#include <jvm.h>
#include <java/util/Properties.h>

#include <io.h>

// Prefix and suffix for shared libraries.
#define _Jv_platform_solib_prefix ""
#define _Jv_platform_solib_suffix ".dll"

// Separator for file name components.
#define _Jv_platform_file_separator ((jchar) '\\')
// Separator for path components.
#define _Jv_platform_path_separator ((jchar) ';')

// List of names for `JNI_OnLoad'.  On Win32, JNI_OnLoad is an
// "stdcall" function taking two pointers (8 bytes) as arguments.  It
// could also have been exported as "JNI_OnLoad@8" (MinGW) or
// "_JNI_OnLoad@8" (MSVC).
#define _Jv_platform_onload_names \
    { "JNI_OnLoad", "JNI_OnLoad@8", "_JNI_OnLoad@8", NULL }

// Type of libffi ABI used by JNICALL methods.  NOTE: This must agree
// with the JNICALL definition in jni.h
#define _Jv_platform_ffi_abi FFI_STDCALL

/* Useful helper classes and methods. */

/* A C++ wrapper around a WSAEVENT which closes the event
	 in its destructor. If dwSelFlags is non-zero, we also
	 issue an WSAEventSelect on the socket descriptor with
	 the given flags; this is undone by a corresponding call
	 to WSAEventSelect(fd, 0, 0) in our destructor. */
class WSAEventWrapper
{
public:
	WSAEventWrapper(int fd, DWORD dwSelFlags);
	~WSAEventWrapper();

	WSAEVENT getEventHandle()
	{
		return m_hEvent;
	}

private:
	WSAEVENT m_hEvent;
	int m_fd;
	DWORD m_dwSelFlags;
};

// Error string text. The int argument is compatible
// with both int WSAGetLastError() and DWORD GetLastError()
// I tried avoiding having to pass the error explicitly, but
// it didn't work this was invoked with say
// throw new SomeException(_Jv_WinStrError()).
extern jstring
_Jv_WinStrError (LPCTSTR lpszPrologue, int nErrorCode);

extern jstring
_Jv_WinStrError (int nErrorCode);

extern void
_Jv_ThrowIOException (DWORD dwErrorCode);

extern void
_Jv_ThrowIOException ();

extern void
_Jv_ThrowSocketException (DWORD dwErrorCode);

extern void
_Jv_ThrowSocketException ();

// Platform implementation
extern void _Jv_platform_initialize (void);
extern void _Jv_platform_initProperties (java::util::Properties*);
extern jlong _Jv_platform_gettimeofday ();
extern int _Jv_select (int n, fd_set *, fd_set *, fd_set *, struct timeval *);
extern int _Jv_pipe (int filedes[2]);

extern void
_Jv_platform_close_on_exec (HANDLE h);

#ifdef JV_HASH_SYNCHRONIZATION
/* Suspends the execution of the current thread for the specified
   number of microseconds.  Tries to emulate the behaviour of usleep()
   on UNIX and provides a granularity of 1 millisecond.  */
inline void
_Jv_platform_usleep (unsigned long usecs)
{
  if (usecs > 0UL)
    {
      unsigned long millis = ((usecs + 999UL) / 1000UL);
      Sleep (millis);
    }
}
#endif /* JV_HASH_SYNCHRONIZATION */

/* Store up to SIZE return address of the current program state in
   ARRAY and return the exact number of values stored.  */
extern int backtrace (void **__array, int __size);

#endif /* __JV_WIN32_H__ */
