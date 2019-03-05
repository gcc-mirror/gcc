/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_rapi.d)
 */
module core.sys.windows.rapi;
version (Windows):

/* Comment from MinGW
   NOTE: This strictly does not belong in the Win32 API since it's
   really part of Platform SDK.
 */

private import core.sys.windows.winbase, core.sys.windows.windef;

extern (Windows):

enum RAPISTREAMFLAG
{
    STREAM_TIMEOUT_READ
}

interface IRAPIStream
{
    HRESULT SetRapiStat(RAPISTREAMFLAG, DWORD);
    HRESULT GetRapiStat(RAPISTREAMFLAG, DWORD*);
}

alias HRESULT function(DWORD, BYTE, DWORD, BYTE, IRAPIStream) RAPIEXT;

struct RAPIINIT
{
    DWORD   cbSize = this.sizeof;
    HANDLE  heRapiInit;
    HRESULT hrRapiInit;
}

HRESULT CeRapiInit();
HRESULT CeRapiInitEx(RAPIINIT*);
BOOL CeCreateProcess(LPCWSTR, LPCWSTR, LPSECURITY_ATTRIBUTES,
  LPSECURITY_ATTRIBUTES, BOOL, DWORD, LPVOID, LPWSTR, LPSTARTUPINFO,
  LPPROCESS_INFORMATION);
HRESULT CeRapiUninit();
BOOL CeWriteFile(HANDLE, LPCVOID, DWORD, LPDWORD, LPOVERLAPPED);
HANDLE CeCreateFile(LPCWSTR, DWORD, DWORD, LPSECURITY_ATTRIBUTES, DWORD,
  DWORD, HANDLE);
BOOL CeCreateDirectory(LPCWSTR, LPSECURITY_ATTRIBUTES);
DWORD CeGetLastError();
BOOL CeGetFileTime(HANDLE, LPFILETIME, LPFILETIME, LPFILETIME);
BOOL CeCloseHandle(HANDLE);
