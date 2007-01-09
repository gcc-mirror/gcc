// win32.h -- Helper functions for Microsoft-flavored OSs.

/* Copyright (C) 2002, 2003, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_WIN32_H__
#define __JV_WIN32_H__

// Enable UNICODE support?

#ifdef MINGW_LIBGCJ_UNICODE
#define UNICODE
#define _UNICODE
#endif // MINGW_LIBGCJ_UNICODE

#include <tchar.h>

// Includes
#define WIN32_LEAN_AND_MEAN
// Force Winsock 2 interface.
#include <winsock2.h>
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#undef STRICT

#include <ws2tcpip.h>
#include <gcj/cni.h>
#include <jvm.h>
#include <java/util/Properties.h>

#include <io.h>

/* Begin UNICODE Support Classes and Functions */

/* Helper class which creates a temporary, null-terminated,
   wide-character C string. */
class _Jv_Win32TempString
{
public:
  _Jv_Win32TempString(jstring jstr);
  ~_Jv_Win32TempString();

// Accessors
  operator LPCTSTR() const
  {
    return buf_;
  }
  LPCTSTR buf() const
  {
    return buf_;
  }
  LPTSTR buf()
  {
    return buf_;
  }

private:
  TCHAR stackbuf_[500];
  LPTSTR buf_;
};

// Mimics the JV_TEMP_STRING_UTF macro in jvm.h
#define JV_TEMP_STRING_WIN32(x,y) _Jv_Win32TempString x(y);

// Creates a jstring from a LPCTSTR
extern jstring _Jv_Win32NewString (LPCTSTR pcsz);

/* End UNICODE Helpers */

// Prefix and suffix for shared libraries.
#define _Jv_platform_solib_prefix ""
#define _Jv_platform_solib_suffix ".dll"

// Name of the Process implementation.
#define _Jv_platform_process ::java::lang::Win32Process

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
  // Default constructor. Call init() after this.
  WSAEventWrapper();
  WSAEventWrapper(int fd, DWORD dwSelFlags);
  ~WSAEventWrapper();

  // Used for two-step initialization after calling
  // default constructor.
  void init(int fd, DWORD dwSelFlags);

  int getFD()
  {
    return m_fd;
  }

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
extern jlong _Jv_platform_nanotime ();
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

// Forward declaration.  See java-stack.h for definition.
struct _Jv_AddrInfo;

// Given an address, determine the executable or shared object that defines
// it and the nearest named symbol.
extern int _Jv_platform_dladdr (void *addr, _Jv_AddrInfo *info);

#endif /* __JV_WIN32_H__ */
