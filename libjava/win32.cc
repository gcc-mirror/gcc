// win32.cc - Helper functions for Microsoft-flavored OSs.

/* Copyright (C) 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>
#include <sys/timeb.h>
#include <stdlib.h>
#include <fcntl.h>

#include <java/lang/ArithmeticException.h>
#include <java/lang/UnsupportedOperationException.h>
#include <java/io/IOException.h>
#include <java/net/SocketException.h>
#include <java/util/Properties.h>

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

// Platform-specific executable name
static char exec_name[MAX_PATH];
  // initialized in _Jv_platform_initialize()

const char *_Jv_ThisExecutable (void)
{
  return exec_name;
}

// Helper classes and methods implementation
  
// class WSAEventWrapper
WSAEventWrapper::WSAEventWrapper (int fd, DWORD dwSelFlags):
  m_hEvent(0),
  m_fd(fd),
  m_dwSelFlags(dwSelFlags)
{
  m_hEvent = WSACreateEvent ();
  if (dwSelFlags)
    WSAEventSelect(fd, m_hEvent, dwSelFlags);
}

WSAEventWrapper::~WSAEventWrapper ()
{
  if (m_dwSelFlags)
  {
    WSAEventSelect(m_fd, m_hEvent, 0);
    if (m_dwSelFlags & (FD_ACCEPT | FD_CONNECT))
    {
      // Set the socket back to non-blocking mode.
      // Ignore any error since we're in a destructor.
      unsigned long lSockOpt = 0L;
        // blocking mode
      ::ioctlsocket (m_fd, FIONBIO, &lSockOpt);
    }
  }
  WSACloseEvent (m_hEvent);
}

// Error string text.
jstring
_Jv_WinStrError (LPCTSTR lpszPrologue, int nErrorCode)
{
  LPTSTR lpMsgBuf = 0;

  DWORD dwFlags = FORMAT_MESSAGE_ALLOCATE_BUFFER |
    FORMAT_MESSAGE_FROM_SYSTEM |
    FORMAT_MESSAGE_IGNORE_INSERTS;

  FormatMessage (dwFlags,
    NULL,
    (DWORD) nErrorCode,
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
    (LPTSTR) &lpMsgBuf,
    0,
    NULL);

  jstring ret;
  if (lpszPrologue)
    {
      LPTSTR lpszTemp =
        (LPTSTR) _Jv_Malloc (strlen (lpszPrologue) +
          strlen (lpMsgBuf) + 3);
      strcpy (lpszTemp, lpszPrologue);
      strcat (lpszTemp, ": ");
      strcat (lpszTemp, lpMsgBuf);
      ret = JvNewStringLatin1 (lpszTemp);
    } 
  else
    {
      ret = JvNewStringLatin1 (lpMsgBuf);
    }

  LocalFree(lpMsgBuf);
  return ret;
}

jstring
_Jv_WinStrError (int nErrorCode)
{
  return _Jv_WinStrError (0, nErrorCode);
}

void _Jv_ThrowIOException (DWORD dwErrorCode)
{
  throw new java::io::IOException (_Jv_WinStrError (dwErrorCode));
}

void _Jv_ThrowIOException()
{
  DWORD dwErrorCode = WSAGetLastError ();
  _Jv_ThrowIOException (dwErrorCode);
}

void _Jv_ThrowSocketException (DWORD dwErrorCode)
{
  throw new java::net::SocketException (_Jv_WinStrError (dwErrorCode));
}

void _Jv_ThrowSocketException()
{
  DWORD dwErrorCode = WSAGetLastError ();
  _Jv_ThrowSocketException (dwErrorCode);
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

  // Initialize our executable name
  GetModuleFileName(NULL, exec_name, sizeof(exec_name));
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

// Set platform-specific System properties.
void
_Jv_platform_initProperties (java::util::Properties* newprops)
{
  // A convenience define.
#define SET(Prop,Val) \
  newprops->put(JvNewStringLatin1 (Prop), JvNewStringLatin1 (Val))

  SET ("file.separator", "\\");
  SET ("path.separator", ";");
  SET ("line.separator", "\r\n");

  // Use GetCurrentDirectory to set 'user.dir'.
  DWORD buflen = MAX_PATH;
  char *buffer = (char *) _Jv_MallocUnchecked (buflen);
  if (buffer != NULL)
    {
      if (GetCurrentDirectory (buflen, buffer))
  SET ("user.dir", buffer);

      if (GetTempPath (buflen, buffer))
  SET ("java.io.tmpdir", buffer);

      _Jv_Free (buffer);
    }

  // Use GetUserName to set 'user.name'.
  buflen = 257;  // UNLEN + 1
  buffer = (char *) _Jv_MallocUnchecked (buflen);
  if (buffer != NULL)
    {
      if (GetUserName (buffer, &buflen))
        SET ("user.name", buffer);
      _Jv_Free (buffer);
    }

  // According to the api documentation for 'GetWindowsDirectory()', the
  // environmental variable HOMEPATH always specifies the user's home
  // directory or a default directory.  On the 3 windows machines I checked
  // only 1 had it set.  If it's not set, JDK1.3.1 seems to set it to
  // the windows directory, so we'll do the same.
  char *userHome = NULL;
  if ((userHome = ::getenv ("HOMEPATH")) == NULL )
    {
      // Check HOME since it's what I use.
      if ((userHome = ::getenv ("HOME")) == NULL )
        {
          // Not found - use the windows directory like JDK1.3.1 does.
          char *winHome = (char *) _Jv_MallocUnchecked (MAX_PATH);
          if (winHome != NULL)
            {
              if (GetWindowsDirectory (winHome, MAX_PATH))
        SET ("user.home", winHome);
              _Jv_Free (winHome);
            }
        }
     }
  if (userHome != NULL)
    SET ("user.home", userHome);

  // Get and set some OS info.
  OSVERSIONINFO osvi;
  ZeroMemory (&osvi, sizeof(OSVERSIONINFO));
  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  if (GetVersionEx (&osvi))
    {
      char *buffer = (char *) _Jv_MallocUnchecked (30);
      if (buffer != NULL)
        {
          sprintf (buffer, "%d.%d", (int) osvi.dwMajorVersion,
           (int) osvi.dwMinorVersion);
          SET ("os.version", buffer);
          _Jv_Free (buffer);
        }

      switch (osvi.dwPlatformId)
        {
          case VER_PLATFORM_WIN32_WINDOWS:
            if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0)
              SET ("os.name", "Windows 95");
            else if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10)
              SET ("os.name", "Windows 98");
            else if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 90)
              SET ("os.name", "Windows Me");
            else
              SET ("os.name", "Windows ??");
            break;

          case VER_PLATFORM_WIN32_NT:
            if (osvi.dwMajorVersion <= 4 )
              SET ("os.name", "Windows NT");
            else if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0)
              SET ("os.name", "Windows 2000");
            else if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1)
              SET ("os.name", "Windows XP");
            else
              SET ("os.name", "Windows NT ??");
            break;

          default:
            SET ("os.name", "Windows UNKNOWN");
            break;
       }
  }

  // Set the OS architecture.
  SYSTEM_INFO si;
  GetSystemInfo (&si);
  switch (si.wProcessorArchitecture)
    {
      case PROCESSOR_ARCHITECTURE_INTEL:
        SET ("os.arch", "x86");
        break;
      case PROCESSOR_ARCHITECTURE_MIPS:
        SET ("os.arch", "mips");
        break;
      case PROCESSOR_ARCHITECTURE_ALPHA:
        SET ("os.arch", "alpha");
        break;
      case PROCESSOR_ARCHITECTURE_PPC:	
        SET ("os.arch", "ppc");
        break;
      case PROCESSOR_ARCHITECTURE_IA64:
        SET ("os.arch", "ia64");
        break;
      case PROCESSOR_ARCHITECTURE_UNKNOWN:
      default:
        SET ("os.arch", "unknown");
        break;
    }
}

/* Store up to SIZE return address of the current program state in
   ARRAY and return the exact number of values stored.  */
int
backtrace (void **__array, int __size)
{
  register void *_ebp __asm__ ("ebp");
  register void *_esp __asm__ ("esp");
  unsigned int *rfp;

  int i=0;
  for (rfp = *(unsigned int**)_ebp;
       rfp && i < __size;
       rfp = *(unsigned int **)rfp)
    {
      int diff = *rfp - (unsigned int)rfp;
      if ((void*)rfp < _esp || diff > 4 * 1024 || diff < 0) break;

    __array[i++] = (void*)(rfp[1]-4);
  }
  return i;
}

int
_Jv_select (int n, fd_set *readfds, fd_set  *writefds,
      fd_set *exceptfds, struct timeval *timeout)
{
  int r = ::select (n, readfds, writefds, exceptfds, timeout);
  if (r == SOCKET_ERROR)
    {
      DWORD dwErrorCode = WSAGetLastError ();
      throw new java::io::IOException (_Jv_WinStrError (dwErrorCode));
    }
  return r;      
}

int
_Jv_pipe (int filedes[2])
{
  return _pipe (filedes, 4096, _O_BINARY);
}
