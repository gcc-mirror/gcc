// win32.cc - Helper functions for Microsoft-flavored OSs.

/* Copyright (C) 2002, 2003, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>
#include <sys/timeb.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#include <java-stack.h>

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
  
#ifdef MINGW_LIBGCJ_UNICODE

// We're using the OS W (UNICODE) API, which means that we're speaking
// the same language....
jstring
_Jv_Win32NewString (LPCTSTR pcsz)
{
  return JvNewString ((jchar*) pcsz, _tcslen (pcsz));
}

#else

// We're using the OS A functions, which means we need to translate between
// UNICODE and the native character set.

// First, let's set up some helper translation functions....

// Converts the native string to any specified jstring, returning the
// length of the jstring. If the specified jstring is null, we simply
// compute and return the length.
static int nativeToUnicode(LPCSTR pcsz, jstring jstr = 0)
{
  jchar* buf = 0;
  int len = 0;
  if (jstr)
    {
      len = jstr->length();
      buf = JvGetStringChars(jstr);
    }
  return ::MultiByteToWideChar(GetACP(), 0, pcsz,
    strlen(pcsz), (LPWSTR) buf, len);
}

// Does the inverse of nativeToUnicode, with the same calling semantics.
static int unicodeToNative(jstring jstr, LPSTR buf, int buflen)
{
  return ::WideCharToMultiByte(GetACP(), 0, (LPWSTR) JvGetStringChars(jstr),
    jstr->length(), buf, buflen, NULL, NULL);
}

// Convenience function when the caller only wants to compute the length
// of the native string.
static int unicodeToNative(jstring jstr)
{
  return unicodeToNative(jstr, 0, 0);
}

jstring
_Jv_Win32NewString (LPCTSTR pcsz)
{
  // Compute the length, allocate the jstring, then perform the conversion.
  int len = nativeToUnicode(pcsz);
  jstring jstr = JvAllocString(len);
  nativeToUnicode(pcsz, jstr);
  return jstr;
}

#endif // MINGW_LIBGCJ_UNICODE

// class _Jv_Win32TempString
_Jv_Win32TempString::_Jv_Win32TempString(jstring jstr):
  buf_(0)
{
  if (jstr == 0)
    return;
    
  // We need space for the string length plus a null terminator.
  // Determine whether to use our stack-allocated buffer or a heap-
  // allocated one.
#ifdef MINGW_LIBGCJ_UNICODE
  // A UNICODE character is a UNICODE character is a UNICODE character....
  int len = jstr->length();
#else
  // Compute the length of the native character string.
  int len = unicodeToNative(jstr);
#endif // MINGW_LIBGCJ_UNICODE

  int bytesNeeded = (len + 1) * sizeof(TCHAR);
  if (bytesNeeded <= (int) sizeof(stackbuf_))
    buf_ = stackbuf_;
  else
    buf_ = (LPTSTR) _Jv_Malloc(bytesNeeded);
    
#ifdef MINGW_LIBGCJ_UNICODE
  // Copy the UNICODE characters to our buffer.
  _tcsncpy(buf_, (LPCTSTR) JvGetStringChars (jstr), len);
#else
  // Convert the UNICODE string to a native one.
  unicodeToNative(jstr, buf_, len);
#endif // MINGW_LIBGCJ_UNICODE

  buf_[len] = 0;
}

_Jv_Win32TempString::~_Jv_Win32TempString()
{
  if (buf_ && buf_ != stackbuf_)
    _Jv_Free (buf_);
}

// class WSAEventWrapper
WSAEventWrapper::WSAEventWrapper ():
  m_hEvent(0),
  m_fd(0),
  m_dwSelFlags(0)
{}

WSAEventWrapper::WSAEventWrapper (int fd, DWORD dwSelFlags):
  m_hEvent(0),
  m_fd(0),
  m_dwSelFlags(0)
{
  init(fd, dwSelFlags);
}

void WSAEventWrapper::init(int fd, DWORD dwSelFlags)
{
  m_fd = fd;
  m_dwSelFlags = dwSelFlags;
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
        (LPTSTR) _Jv_Malloc ((_tcslen (lpszPrologue) +
          _tcslen (lpMsgBuf) + 3) * sizeof(TCHAR) );
      _tcscpy (lpszTemp, lpszPrologue);
      _tcscat (lpszTemp, _T(": "));
      _tcscat (lpszTemp, lpMsgBuf);
      ret = _Jv_Win32NewString (lpszTemp);
      _Jv_Free (lpszTemp);
    } 
  else
    {
      ret = _Jv_Win32NewString (lpMsgBuf);
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
  if (WSAStartup (MAKEWORD (2, 2), &data))
    MessageBox (NULL, _T("Error initialising winsock library."), _T("Error"),
    MB_OK | MB_ICONEXCLAMATION);

  // Install exception handler
  SetUnhandledExceptionFilter (win32_exception_handler);

  // Initialize our executable name.
  // FIXME: We unconditionally use the ANSI function because
  // _Jv_ThisExecutable returns a const char*. We should really
  // change _Jv_ThisExecutable to return a jstring.
  GetModuleFileNameA(NULL, exec_name, sizeof(exec_name));
}

// gettimeofday implementation.
jlong
_Jv_platform_gettimeofday ()
{
  struct timeb t;
  ftime (&t);
  return t.time * 1000LL + t.millitm;
}

jlong
_Jv_platform_nanotime ()
{
  return _Jv_platform_gettimeofday () * 1000LL;
}

static bool dirExists (LPCTSTR dir)
{
  DWORD dwAttrs = ::GetFileAttributes (dir);
  return dwAttrs != 0xFFFFFFFF &&
    (dwAttrs & FILE_ATTRIBUTE_DIRECTORY) != 0;
}

static void getUserHome(LPTSTR userHome, LPCTSTR userId)
{
  LPTSTR uh = _tgetenv (_T("USERPROFILE"));
  if (uh)
    {
      _tcscpy(userHome, uh);
    }
  else
    {
      // Make a half-hearted attempt to support this
      // legacy version of Windows. Try %WINDIR%\Profiles\%USERNAME%
      // and failing this, use %WINDIR%.
      //
      // See:http://java.sun.com/docs/books/tutorial/security1.2/summary/files.html#UserPolicy
      //
      // To do this correctly, we'd have to factor in the
      // Windows version, but if we did that, then this attempt
      // wouldn't be half-hearted.
      TCHAR userHomePath[MAX_PATH], winHome[MAX_PATH];
      ::GetWindowsDirectory(winHome, MAX_PATH);
        // assume this call always succeeds

      _stprintf(userHomePath, _T("%s\\Profiles\\%s"), winHome, userId);
      if (dirExists (userHomePath))
        _tcscpy(userHome, userHomePath);
      else
        _tcscpy(userHome, winHome);
    }
}

// Set platform-specific System properties.
void
_Jv_platform_initProperties (java::util::Properties* newprops)
{
  // A convenience define.
#define SET(Prop,Val) \
  newprops->put(JvNewStringLatin1 (Prop), _Jv_Win32NewString (Val))

  SET ("file.separator", _T("\\"));
  SET ("path.separator", _T(";"));
  SET ("line.separator", _T("\r\n"));

  // Use GetCurrentDirectory to set 'user.dir'.
  DWORD buflen = MAX_PATH;
  TCHAR buffer[buflen];
  if (buffer != NULL)
    {
      if (GetCurrentDirectory (buflen, buffer))
  SET ("user.dir", buffer);

      if (GetTempPath (buflen, buffer))
  SET ("java.io.tmpdir", buffer);
    }

  // Use GetUserName to set 'user.name'.
  buflen = 257;  // UNLEN + 1
  TCHAR userName[buflen];
  if (GetUserName (userName, &buflen))
    SET ("user.name", userName);

  // Set user.home
  TCHAR userHome[MAX_PATH];
  getUserHome(userHome, userName);
  SET ("user.home", userHome);

  // Get and set some OS info.
  OSVERSIONINFO osvi;
  ZeroMemory (&osvi, sizeof(OSVERSIONINFO));
  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  if (GetVersionEx (&osvi))
    {
      if (buffer != NULL)
        {
          _stprintf (buffer, _T("%d.%d"), (int) osvi.dwMajorVersion,
           (int) osvi.dwMinorVersion);
          SET ("os.version", buffer);
        }

      switch (osvi.dwPlatformId)
        {
          case VER_PLATFORM_WIN32_WINDOWS:
            if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0)
              SET ("os.name", _T("Windows 95"));
            else if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10)
              SET ("os.name", _T("Windows 98"));
            else if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 90)
              SET ("os.name", _T("Windows Me"));
            else
              SET ("os.name", _T("Windows ??"));
            break;

          case VER_PLATFORM_WIN32_NT:
            if (osvi.dwMajorVersion <= 4 )
              SET ("os.name", _T("Windows NT"));
            else if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0)
              SET ("os.name", _T("Windows 2000"));
            else if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1)
              SET ("os.name", _T("Windows XP"));
            else
              SET ("os.name", _T("Windows NT ??"));
            break;

          default:
            SET ("os.name", _T("Windows UNKNOWN"));
            break;
       }
  }

  // Set the OS architecture.
  SYSTEM_INFO si;
  GetSystemInfo (&si);
  switch (si.wProcessorArchitecture)
    {
      case PROCESSOR_ARCHITECTURE_INTEL:
        SET ("os.arch", _T("x86"));
        break;
      case PROCESSOR_ARCHITECTURE_MIPS:
        SET ("os.arch", _T("mips"));
        break;
      case PROCESSOR_ARCHITECTURE_ALPHA:
        SET ("os.arch", _T("alpha"));
        break;
      case PROCESSOR_ARCHITECTURE_PPC:  
        SET ("os.arch", _T("ppc"));
        break;
      case PROCESSOR_ARCHITECTURE_IA64:
        SET ("os.arch", _T("ia64"));
        break;
      case PROCESSOR_ARCHITECTURE_UNKNOWN:
      default:
        SET ("os.arch", _T("unknown"));
        break;
    }
}

int
_Jv_pipe (int filedes[2])
{
  return _pipe (filedes, 4096, _O_BINARY);
}

void
_Jv_platform_close_on_exec (HANDLE h)
{
  // Mark the handle as non-inheritable. This has
  // no effect under Win9X.
  SetHandleInformation (h, HANDLE_FLAG_INHERIT, 0);
}

// Given an address, find the object that defines it and the nearest
// defined symbol to that address.  Returns 0 if no object defines this
// address.
int
_Jv_platform_dladdr (void *addr, _Jv_AddrInfo *info)
{
  // Since we do not have dladdr() on Windows, we use a trick involving
  // VirtualQuery() to find the module (EXE or DLL) that contains a given
  // address.  This was taken from Matt Pietrek's "Under the Hood" column
  // for the April 1997 issue of Microsoft Systems Journal.

  MEMORY_BASIC_INFORMATION mbi;
  if (!VirtualQuery (addr, &mbi, sizeof (mbi)))
  {
    return 0;
  }
  
  HMODULE hMod = (HMODULE) mbi.AllocationBase;

  char moduleName[MAX_PATH];

  // FIXME: We explicitly use the ANSI variant of the function here.
  if (!GetModuleFileNameA (hMod, moduleName, sizeof (moduleName)))
  {
    return 0;
  }

  char *file_name = (char *)(malloc (strlen (moduleName) + 1));
  strcpy (file_name, moduleName);
  info->file_name = file_name;

  // FIXME.
  info->base = NULL;
  info->sym_name = NULL;
  info->sym_addr = NULL;

  return 1;
}
