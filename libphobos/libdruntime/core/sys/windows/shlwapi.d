/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_shlwapi.d)
 */
module core.sys.windows.shlwapi;
version (Windows):

version (ANSI) {} else version = Unicode;
pragma(lib, "shlwapi");

/* Changes compared to MinGW:
wnsprintf functions are not included.
// Macros unneeded for D
#define StrCmpIA lstrcmpiA;
#define StrCmpA lstrcmpA;
#define StrCpyA lstrcpyA;
#define StrCpyNA lstrcpynA;
#define MAKEDLLVERULL(major, minor, build, qfe) \
        (((ULONGLONG)(major) << 48) | \
         ((ULONGLONG)(minor) << 32) | \
         ((ULONGLONG)(build) << 16) | \
         ((ULONGLONG)(  qfe) <<  0))
*/

import core.sys.windows.objbase, core.sys.windows.shlobj;
private import core.sys.windows.basetyps, core.sys.windows.objidl, core.sys.windows.unknwn, core.sys.windows.windef,
  core.sys.windows.winbase, core.sys.windows.winreg;

enum DLLVER_PLATFORM_WINDOWS = 0x00000001;
enum DLLVER_PLATFORM_NT      = 0x00000002;

enum URL_DONT_ESCAPE_EXTRA_INFO  = 0x02000000;
enum URL_DONT_SIMPLIFY           = 0x08000000;
enum URL_ESCAPE_PERCENT          = 0x00001000;
enum URL_ESCAPE_SEGMENT_ONLY     = 0x00002000;
enum URL_ESCAPE_SPACES_ONLY      = 0x04000000;
enum URL_ESCAPE_UNSAFE           = 0x20000000;
enum URL_INTERNAL_PATH           = 0x00800000;
enum URL_PARTFLAG_KEEPSCHEME     = 0x00000001;
enum URL_PLUGGABLE_PROTOCOL      = 0x40000000;
enum URL_UNESCAPE                = 0x10000000;
enum URL_UNESCAPE_HIGH_ANSI_ONLY = 0x00400000;
enum URL_UNESCAPE_INPLACE        = 0x00100000;

align(1):
struct DLLVERSIONINFO
{
    DWORD cbSize = this.sizeof;
    DWORD dwMajorVersion;
    DWORD dwMinorVersion;
    DWORD dwBuildNumber;
    DWORD dwPlatformID;
}

struct DLLVERSIONINFO2
{
    DLLVERSIONINFO info1;
    DWORD dwFlags;
    ULONGLONG ullVersion;
}

enum ASSOCSTR {
    ASSOCSTR_COMMAND,
    ASSOCSTR_EXECUTABLE,
    ASSOCSTR_FRIENDLYDOCNAME,
    ASSOCSTR_FRIENDLYAPPNAME,
    ASSOCSTR_NOOPEN,
    ASSOCSTR_SHELLNEWVALUE,
    ASSOCSTR_DDECOMMAND,
    ASSOCSTR_DDEIFEXEC,
    ASSOCSTR_DDEAPPLICATION,
    ASSOCSTR_DDETOPIC
}

enum ASSOCKEY
{
    ASSOCKEY_SHELLEXECCLASS = 1,
    ASSOCKEY_APP,
    ASSOCKEY_CLASS,
    ASSOCKEY_BASECLASS
}

enum ASSOCDATA
{
    ASSOCDATA_MSIDESCRIPTOR = 1,
    ASSOCDATA_NOACTIVATEHANDLER,
    ASSOCDATA_QUERYCLASSSTORE
}

alias DWORD ASSOCF;

enum SHREGDEL_FLAGS
{
    SHREGDEL_DEFAULT = 0x00000000,
    SHREGDEL_HKCU    = 0x00000001,
    SHREGDEL_HKLM    = 0x00000010,
    SHREGDEL_BOTH    = 0x00000011
}

enum SHREGENUM_FLAGS
{
    SHREGENUM_DEFAULT = 0x00000000,
    SHREGENUM_HKCU    = 0x00000001,
    SHREGENUM_HKLM    = 0x00000010,
    SHREGENUM_BOTH    = 0x00000011
}

enum URLIS
{
    URLIS_URL,
    URLIS_OPAQUE,
    URLIS_NOHISTORY,
    URLIS_FILEURL,
    URLIS_APPLIABLE,
    URLIS_DIRECTORY,
    URLIS_HASQUERY
}

mixin DECLARE_HANDLE!("HUSKEY");
alias HUSKEY* PHUSKEY;

extern (Windows)
{
    alias HRESULT function (DLLVERSIONINFO *) DLLGETVERSIONPROC;
}


BOOL IntlStrEqNA(LPCSTR pStr1, LPCSTR pStr2, int nChar)
{
    return IntlStrEqWorkerA(TRUE, pStr1, pStr2, nChar);
}

BOOL IntlStrEqNW(LPCWSTR pStr1, LPCWSTR pStr2, int nChar)
{
    return IntlStrEqWorkerW(TRUE, pStr1, pStr2, nChar);
}

BOOL IntlStrEqNIA(LPCSTR pStr1, LPCSTR pStr2, int nChar)
{
    return IntlStrEqWorkerA(FALSE, pStr1, pStr2, nChar);
}

BOOL IntlStrEqNIW(LPCWSTR pStr1, LPCWSTR pStr2, int nChar)
{
    return IntlStrEqWorkerW(FALSE, pStr1, pStr2, nChar);
}

BOOL UrlIsFileUrlA(LPCSTR pszURL)
{
    return UrlIsA(pszURL, URLIS.URLIS_FILEURL);
}

BOOL UrlIsFileUrlW(LPCWSTR pszURL)
{
    return UrlIsW(pszURL, URLIS.URLIS_FILEURL);
}

HRESULT UrlUnescapeInPlaceA(LPSTR pszUrl, DWORD dwFlags)
{
    return UrlUnescapeA(pszUrl, null, null, dwFlags | URL_UNESCAPE_INPLACE);
}
HRESULT UrlUnescapeInPlaceW(LPWSTR pszUrl, DWORD dwFlags)
{
    return UrlUnescapeW(pszUrl, null, null, dwFlags | URL_UNESCAPE_INPLACE);
}

extern (Windows):
BOOL ChrCmpIA(WORD, WORD);
BOOL ChrCmpIW(WCHAR, WCHAR);
BOOL IntlStrEqWorkerA(BOOL, LPCSTR, LPCSTR, int);
BOOL IntlStrEqWorkerW(BOOL, LPCWSTR, LPCWSTR, int);
HRESULT SHStrDupA(LPCSTR, LPWSTR*);
HRESULT SHStrDupW(LPCWSTR, LPWSTR*);
LPSTR StrCatA(LPSTR, LPCSTR);
LPWSTR StrCatW(LPWSTR, LPCWSTR);
LPSTR StrCatBuffA(LPSTR, LPCSTR, int);
LPWSTR StrCatBuffW(LPWSTR, LPCWSTR, int);
DWORD StrCatChainW(LPWSTR, DWORD, DWORD, LPCWSTR);
LPSTR StrChrA(LPCSTR, WORD);
LPWSTR StrChrW(LPCWSTR, WCHAR);
LPSTR StrChrIA(LPCSTR, WORD);
LPWSTR StrChrIW(LPCWSTR, WCHAR);
int StrCmpIW(LPCWSTR, LPCWSTR);
int StrCmpW(LPCWSTR, LPCWSTR);
LPWSTR StrCpyW(LPWSTR, LPCWSTR);
LPWSTR StrCpyNW(LPWSTR, LPCWSTR, int);
int StrCmpNA(LPCSTR, LPCSTR, int);
int StrCmpNW(LPCWSTR, LPCWSTR, int);
int StrCmpNIA(LPCSTR, LPCSTR, int);
int StrCmpNIW(LPCWSTR, LPCWSTR, int);
int StrCSpnA(LPCSTR, LPCSTR);
int StrCSpnW(LPCWSTR, LPCWSTR);
int StrCSpnIA(LPCSTR, LPCSTR);
int StrCSpnIW(LPCWSTR, LPCWSTR);
LPSTR StrDupA(LPCSTR);
LPWSTR StrDupW(LPCWSTR);
LPSTR StrFormatByteSize64A(LONGLONG, LPSTR, UINT);
LPSTR StrFormatByteSizeA(DWORD, LPSTR, UINT);
LPWSTR StrFormatByteSizeW(LONGLONG, LPWSTR, UINT);
LPSTR StrFormatKBSizeA(LONGLONG, LPSTR, UINT);
LPWSTR StrFormatKBSizeW(LONGLONG, LPWSTR, UINT);
int StrFromTimeIntervalA(LPSTR, UINT, DWORD, int);
int StrFromTimeIntervalW(LPWSTR, UINT, DWORD, int);
BOOL StrIsIntlEqualA(BOOL, LPCSTR, LPCSTR, int);
BOOL StrIsIntlEqualW(BOOL, LPCWSTR, LPCWSTR, int);
LPSTR StrNCatA(LPSTR, LPCSTR, int);
LPWSTR StrNCatW(LPWSTR, LPCWSTR, int);
LPSTR StrPBrkA(LPCSTR, LPCSTR);
LPWSTR StrPBrkW(LPCWSTR, LPCWSTR);
LPSTR StrRChrA(LPCSTR, LPCSTR, WORD);
LPWSTR StrRChrW(LPCWSTR, LPCWSTR, WCHAR);
LPSTR StrRChrIA(LPCSTR, LPCSTR, WORD);
LPWSTR StrRChrIW(LPCWSTR, LPCWSTR, WCHAR);
LPSTR StrRStrIA(LPCSTR, LPCSTR, LPCSTR);
LPWSTR StrRStrIW(LPCWSTR, LPCWSTR, LPCWSTR);
int StrSpnA(LPCSTR, LPCSTR);
int StrSpnW(LPCWSTR, LPCWSTR);
LPSTR StrStrA(LPCSTR, LPCSTR);
LPSTR StrStrIA(LPCSTR, LPCSTR);
LPWSTR StrStrIW(LPCWSTR, LPCWSTR);
LPWSTR StrStrW(LPCWSTR, LPCWSTR);
int StrToIntA(LPCSTR);
int StrToIntW(LPCWSTR);
BOOL StrToIntExA(LPCSTR, DWORD, int*);
BOOL StrToIntExW(LPCWSTR, DWORD, int*);
BOOL StrTrimA(LPSTR, LPCSTR);
BOOL StrTrimW(LPWSTR, LPCWSTR);
LPSTR PathAddBackslashA(LPSTR);
LPWSTR PathAddBackslashW(LPWSTR);
BOOL PathAddExtensionA(LPSTR, LPCSTR);
BOOL PathAddExtensionW(LPWSTR, LPCWSTR);
BOOL PathAppendA(LPSTR, LPCSTR);
BOOL PathAppendW(LPWSTR, LPCWSTR);
LPSTR PathBuildRootA(LPSTR, int);
LPWSTR PathBuildRootW(LPWSTR, int);
BOOL PathCanonicalizeA(LPSTR, LPCSTR);
BOOL PathCanonicalizeW(LPWSTR, LPCWSTR);
LPSTR PathCombineA(LPSTR, LPCSTR, LPCSTR);
LPWSTR PathCombineW(LPWSTR, LPCWSTR, LPCWSTR);
int PathCommonPrefixA(LPCSTR, LPCSTR, LPSTR);
int PathCommonPrefixW(LPCWSTR, LPCWSTR, LPWSTR);
BOOL PathCompactPathA(HDC, LPSTR, UINT);
BOOL PathCompactPathW(HDC, LPWSTR, UINT);
BOOL PathCompactPathExA(LPSTR, LPCSTR, UINT, DWORD);
BOOL PathCompactPathExW(LPWSTR, LPCWSTR, UINT, DWORD);
HRESULT PathCreateFromUrlA(LPCSTR, LPSTR, LPDWORD, DWORD);
HRESULT PathCreateFromUrlW(LPCWSTR, LPWSTR, LPDWORD, DWORD);
BOOL PathFileExistsA(LPCSTR);
BOOL PathFileExistsW(LPCWSTR);
LPSTR PathFindExtensionA(LPCSTR);
LPWSTR PathFindExtensionW(LPCWSTR);
LPSTR PathFindFileNameA(LPCSTR);
LPWSTR PathFindFileNameW(LPCWSTR);
LPSTR PathFindNextComponentA(LPCSTR);
LPWSTR PathFindNextComponentW(LPCWSTR);
BOOL PathFindOnPathA(LPSTR, LPCSTR*);
BOOL PathFindOnPathW(LPWSTR, LPCWSTR*);
LPCSTR PathFindSuffixArrayA(LPCSTR, LPCSTR*, int);
LPCWSTR PathFindSuffixArrayW(LPCWSTR, LPCWSTR*, int);
LPSTR PathGetArgsA(LPCSTR);
LPWSTR PathGetArgsW(LPCWSTR);
UINT PathGetCharTypeA(UCHAR);
UINT PathGetCharTypeW(WCHAR);
int PathGetDriveNumberA(LPCSTR);
int PathGetDriveNumberW(LPCWSTR);
BOOL PathIsContentTypeA(LPCSTR, LPCSTR);
BOOL PathIsContentTypeW(LPCWSTR, LPCWSTR);
BOOL PathIsDirectoryA(LPCSTR);
BOOL PathIsDirectoryEmptyA(LPCSTR);
BOOL PathIsDirectoryEmptyW(LPCWSTR);
BOOL PathIsDirectoryW(LPCWSTR);
BOOL PathIsFileSpecA(LPCSTR);
BOOL PathIsFileSpecW(LPCWSTR);
BOOL PathIsLFNFileSpecA(LPCSTR);
BOOL PathIsLFNFileSpecW(LPCWSTR);
BOOL PathIsNetworkPathA(LPCSTR);
BOOL PathIsNetworkPathW(LPCWSTR);
BOOL PathIsPrefixA(LPCSTR, LPCSTR);
BOOL PathIsPrefixW(LPCWSTR, LPCWSTR);
BOOL PathIsRelativeA(LPCSTR);
BOOL PathIsRelativeW(LPCWSTR);
BOOL PathIsRootA(LPCSTR);
BOOL PathIsRootW(LPCWSTR);
BOOL PathIsSameRootA(LPCSTR, LPCSTR);
BOOL PathIsSameRootW(LPCWSTR, LPCWSTR);
BOOL PathIsSystemFolderA(LPCSTR, DWORD);
BOOL PathIsSystemFolderW(LPCWSTR, DWORD);
BOOL PathIsUNCA(LPCSTR);
BOOL PathIsUNCServerA(LPCSTR);
BOOL PathIsUNCServerShareA(LPCSTR);
BOOL PathIsUNCServerShareW(LPCWSTR);
BOOL PathIsUNCServerW(LPCWSTR);
BOOL PathIsUNCW(LPCWSTR);
BOOL PathIsURLA(LPCSTR);
BOOL PathIsURLW(LPCWSTR);
BOOL PathMakePrettyA(LPSTR);
BOOL PathMakePrettyW(LPWSTR);
BOOL PathMakeSystemFolderA(LPSTR);
BOOL PathMakeSystemFolderW(LPWSTR);
BOOL PathMatchSpecA(LPCSTR, LPCSTR);
BOOL PathMatchSpecW(LPCWSTR, LPCWSTR);
int PathParseIconLocationA(LPSTR);
int PathParseIconLocationW(LPWSTR);
void PathQuoteSpacesA(LPSTR);
void PathQuoteSpacesW(LPWSTR);
BOOL PathRelativePathToA(LPSTR, LPCSTR, DWORD, LPCSTR, DWORD);
BOOL PathRelativePathToW(LPWSTR, LPCWSTR, DWORD, LPCWSTR, DWORD);
void PathRemoveArgsA(LPSTR);
void PathRemoveArgsW(LPWSTR);
LPSTR PathRemoveBackslashA(LPSTR);
LPWSTR PathRemoveBackslashW(LPWSTR);
void PathRemoveBlanksA(LPSTR);
void PathRemoveBlanksW(LPWSTR);
void PathRemoveExtensionA(LPSTR);
void PathRemoveExtensionW(LPWSTR);
BOOL PathRemoveFileSpecA(LPSTR);
BOOL PathRemoveFileSpecW(LPWSTR);
BOOL PathRenameExtensionA(LPSTR, LPCSTR);
BOOL PathRenameExtensionW(LPWSTR, LPCWSTR);
BOOL PathSearchAndQualifyA(LPCSTR, LPSTR, UINT);
BOOL PathSearchAndQualifyW(LPCWSTR, LPWSTR, UINT);
void PathSetDlgItemPathA(HWND, int, LPCSTR);
void PathSetDlgItemPathW(HWND, int, LPCWSTR);
LPSTR PathSkipRootA(LPCSTR);
LPWSTR PathSkipRootW(LPCWSTR);
void PathStripPathA(LPSTR);
void PathStripPathW(LPWSTR);
BOOL PathStripToRootA(LPSTR);
BOOL PathStripToRootW(LPWSTR);
void PathUndecorateA(LPSTR);
void PathUndecorateW(LPWSTR);
BOOL PathUnExpandEnvStringsA(LPCSTR, LPSTR, UINT);
BOOL PathUnExpandEnvStringsW(LPCWSTR, LPWSTR, UINT);
BOOL PathUnmakeSystemFolderA(LPSTR);
BOOL PathUnmakeSystemFolderW(LPWSTR);
void PathUnquoteSpacesA(LPSTR);
void PathUnquoteSpacesW(LPWSTR);
HRESULT SHAutoComplete(HWND, DWORD);
BOOL SHCreateThread(LPTHREAD_START_ROUTINE, void*, DWORD, LPTHREAD_START_ROUTINE);
DWORD SHCopyKeyA(HKEY, LPCSTR, HKEY, DWORD);
DWORD SHCopyKeyW(HKEY, LPCWSTR, HKEY, DWORD);
DWORD SHDeleteEmptyKeyA(HKEY, LPCSTR);
DWORD SHDeleteEmptyKeyW(HKEY, LPCWSTR);
DWORD SHDeleteKeyA(HKEY, LPCSTR);
DWORD SHDeleteKeyW(HKEY, LPCWSTR);
DWORD SHEnumKeyExA(HKEY, DWORD, LPSTR, LPDWORD);
DWORD SHEnumKeyExW(HKEY, DWORD, LPWSTR, LPDWORD);
DWORD SHQueryInfoKeyA(HKEY, LPDWORD, LPDWORD, LPDWORD, LPDWORD);
DWORD SHQueryInfoKeyW(HKEY, LPDWORD, LPDWORD, LPDWORD, LPDWORD);
DWORD SHQueryValueExA(HKEY, LPCSTR, LPDWORD, LPDWORD, LPVOID, LPDWORD);
DWORD SHQueryValueExW(HKEY, LPCWSTR, LPDWORD, LPDWORD, LPVOID, LPDWORD);
HRESULT SHGetThreadRef(IUnknown*);
HRESULT SHSetThreadRef(IUnknown);
BOOL SHSkipJunction(IBindCtx, const(CLSID)*);
DWORD SHEnumValueA(HKEY, DWORD, LPSTR, LPDWORD, LPDWORD, LPVOID, LPDWORD);
DWORD SHEnumValueW(HKEY, DWORD, LPWSTR, LPDWORD, LPDWORD, LPVOID, LPDWORD);
DWORD SHGetValueA(HKEY, LPCSTR, LPCSTR, LPDWORD, LPVOID, LPDWORD);
DWORD SHGetValueW(HKEY, LPCWSTR, LPCWSTR, LPDWORD, LPVOID, LPDWORD);
DWORD SHSetValueA(HKEY, LPCSTR, LPCSTR, DWORD, LPCVOID, DWORD);
DWORD SHSetValueW(HKEY, LPCWSTR, LPCWSTR, DWORD, LPCVOID, DWORD);
DWORD SHDeleteValueA(HKEY, LPCSTR, LPCSTR);
DWORD SHDeleteValueW(HKEY, LPCWSTR, LPCWSTR);
HRESULT AssocCreate(CLSID, const(IID)* , const(LPVOID)*);
HRESULT AssocQueryKeyA(ASSOCF, ASSOCKEY, LPCSTR, LPCSTR, HKEY*);
HRESULT AssocQueryKeyW(ASSOCF, ASSOCKEY, LPCWSTR, LPCWSTR, HKEY*);
HRESULT AssocQueryStringA(ASSOCF, ASSOCSTR, LPCSTR, LPCSTR, LPSTR, DWORD*);
HRESULT AssocQueryStringByKeyA(ASSOCF, ASSOCSTR, HKEY, LPCSTR, LPSTR, DWORD*);
HRESULT AssocQueryStringByKeyW(ASSOCF, ASSOCSTR, HKEY, LPCWSTR, LPWSTR, DWORD*);
HRESULT AssocQueryStringW(ASSOCF, ASSOCSTR, LPCWSTR, LPCWSTR, LPWSTR, DWORD*);
HRESULT UrlApplySchemeA(LPCSTR, LPSTR, LPDWORD, DWORD);
HRESULT UrlApplySchemeW(LPCWSTR, LPWSTR, LPDWORD, DWORD);
HRESULT UrlCanonicalizeA(LPCSTR, LPSTR, LPDWORD, DWORD);
HRESULT UrlCanonicalizeW(LPCWSTR, LPWSTR, LPDWORD, DWORD);
HRESULT UrlCombineA(LPCSTR, LPCSTR, LPSTR, LPDWORD, DWORD);
HRESULT UrlCombineW(LPCWSTR, LPCWSTR, LPWSTR, LPDWORD, DWORD);
int UrlCompareA(LPCSTR, LPCSTR, BOOL);
int UrlCompareW(LPCWSTR, LPCWSTR, BOOL);
HRESULT UrlCreateFromPathA(LPCSTR, LPSTR, LPDWORD, DWORD);
HRESULT UrlCreateFromPathW(LPCWSTR, LPWSTR, LPDWORD, DWORD);
HRESULT UrlEscapeA(LPCSTR, LPSTR, LPDWORD, DWORD);
HRESULT UrlEscapeW(LPCWSTR, LPWSTR, LPDWORD, DWORD);
LPCSTR UrlGetLocationA(LPCSTR);
LPCWSTR UrlGetLocationW(LPCWSTR);
HRESULT UrlGetPartA(LPCSTR, LPSTR, LPDWORD, DWORD, DWORD);
HRESULT UrlGetPartW(LPCWSTR, LPWSTR, LPDWORD, DWORD, DWORD);
HRESULT UrlHashA(LPCSTR, LPBYTE, DWORD);
HRESULT UrlHashW(LPCWSTR, LPBYTE, DWORD);
BOOL UrlIsA(LPCSTR, URLIS);
BOOL UrlIsW(LPCWSTR, URLIS);
BOOL UrlIsNoHistoryA(LPCSTR);
BOOL UrlIsNoHistoryW(LPCWSTR);
BOOL UrlIsOpaqueA(LPCSTR);
BOOL UrlIsOpaqueW(LPCWSTR);
HRESULT UrlUnescapeA(LPSTR, LPSTR, LPDWORD, DWORD);
HRESULT UrlUnescapeW(LPWSTR, LPWSTR, LPDWORD, DWORD);
DWORD SHRegCloseUSKey(HUSKEY);
LONG SHRegCreateUSKeyA(LPCSTR, REGSAM, HUSKEY, PHUSKEY, DWORD);
LONG SHRegCreateUSKeyW(LPCWSTR, REGSAM, HUSKEY, PHUSKEY, DWORD);
LONG SHRegDeleteEmptyUSKeyA(HUSKEY, LPCSTR, SHREGDEL_FLAGS);
LONG SHRegDeleteEmptyUSKeyW(HUSKEY, LPCWSTR, SHREGDEL_FLAGS);
LONG SHRegDeleteUSValueA(HUSKEY, LPCSTR, SHREGDEL_FLAGS);
LONG SHRegDeleteUSValueW(HUSKEY, LPCWSTR, SHREGDEL_FLAGS);
HKEY SHRegDuplicateHKey(HKEY);
DWORD SHRegEnumUSKeyA(HUSKEY, DWORD, LPSTR, LPDWORD, SHREGENUM_FLAGS);
DWORD SHRegEnumUSKeyW(HUSKEY, DWORD, LPWSTR, LPDWORD, SHREGENUM_FLAGS);
DWORD SHRegEnumUSValueA(HUSKEY, DWORD, LPSTR, LPDWORD, LPDWORD, LPVOID, LPDWORD, SHREGENUM_FLAGS);
DWORD SHRegEnumUSValueW(HUSKEY, DWORD, LPWSTR, LPDWORD, LPDWORD, LPVOID, LPDWORD, SHREGENUM_FLAGS);
BOOL SHRegGetBoolUSValueA(LPCSTR, LPCSTR, BOOL, BOOL);
BOOL SHRegGetBoolUSValueW(LPCWSTR, LPCWSTR, BOOL, BOOL);
DWORD SHRegGetPathA(HKEY, LPCSTR, LPCSTR, LPSTR, DWORD);
DWORD SHRegGetPathW(HKEY, LPCWSTR, LPCWSTR, LPWSTR, DWORD);
LONG SHRegGetUSValueA(LPCSTR, LPCSTR, LPDWORD, LPVOID, LPDWORD, BOOL, LPVOID, DWORD);
LONG SHRegGetUSValueW(LPCWSTR, LPCWSTR, LPDWORD, LPVOID, LPDWORD, BOOL, LPVOID, DWORD);
LONG SHRegOpenUSKeyA(LPCSTR, REGSAM, HUSKEY, PHUSKEY, BOOL);
LONG SHRegOpenUSKeyW(LPCWSTR, REGSAM, HUSKEY, PHUSKEY, BOOL);
DWORD SHRegQueryInfoUSKeyA(HUSKEY, LPDWORD, LPDWORD, LPDWORD, LPDWORD, SHREGENUM_FLAGS);
DWORD SHRegQueryInfoUSKeyW(HUSKEY, LPDWORD, LPDWORD, LPDWORD, LPDWORD, SHREGENUM_FLAGS);
LONG SHRegQueryUSValueA(HUSKEY, LPCSTR, LPDWORD, LPVOID, LPDWORD, BOOL, LPVOID, DWORD);
LONG SHRegQueryUSValueW(HUSKEY, LPCWSTR, LPDWORD, LPVOID, LPDWORD, BOOL, LPVOID, DWORD);
DWORD SHRegSetPathA(HKEY, LPCSTR, LPCSTR, LPCSTR, DWORD);
DWORD SHRegSetPathW(HKEY, LPCWSTR, LPCWSTR, LPCWSTR, DWORD);
LONG SHRegSetUSValueA(LPCSTR, LPCSTR, DWORD, LPVOID, DWORD, DWORD);
LONG SHRegSetUSValueW(LPCWSTR, LPCWSTR, DWORD, LPVOID, DWORD, DWORD);
LONG SHRegWriteUSValueA(HUSKEY, LPCSTR, DWORD, LPVOID, DWORD, DWORD);
LONG SHRegWriteUSValueW(HUSKEY, LPCWSTR, DWORD, LPVOID, DWORD, DWORD);
HRESULT HashData(LPBYTE, DWORD, LPBYTE, DWORD);
HPALETTE SHCreateShellPalette(HDC);
COLORREF ColorHLSToRGB(WORD, WORD, WORD);
COLORREF ColorAdjustLuma(COLORREF, int, BOOL);
void ColorRGBToHLS(COLORREF, WORD*, WORD*, WORD*);
/** Should not be necessary for D?
extern (C):
int  wnsprintfA(LPSTR, int, LPCSTR, ...);
int  wnsprintfW(LPWSTR, int, LPCWSTR, ...);
extern (Windows):
int wvnsprintfA(LPSTR, int, LPCSTR, va_list);
int wvnsprintfW(LPWSTR, int, LPCWSTR, va_list);
*/

HINSTANCE MLLoadLibraryA(LPCSTR, HANDLE, DWORD, LPCSTR, BOOL);
HINSTANCE MLLoadLibraryW(LPCWSTR, HANDLE, DWORD, LPCWSTR, BOOL);

HRESULT DllInstall(BOOL, LPCWSTR);

HRESULT StrRetToBufA(LPSTRRET, LPCITEMIDLIST, LPSTR, UINT);
HRESULT StrRetToBufW(LPSTRRET, LPCITEMIDLIST, LPWSTR, UINT);
HRESULT StrRetToStrA(LPSTRRET, LPCITEMIDLIST, LPSTR*);
HRESULT StrRetToStrW(LPSTRRET, LPCITEMIDLIST, LPWSTR*);
HRESULT SHCreateStreamOnFileA(LPCSTR, DWORD, IStream*);
HRESULT SHCreateStreamOnFileW(LPCWSTR, DWORD, IStream*);
IStream SHOpenRegStream2A(HKEY, LPCSTR, LPCSTR, DWORD);
IStream SHOpenRegStream2W(HKEY, LPCWSTR, LPCWSTR, DWORD);
IStream SHOpenRegStreamA(HKEY, LPCSTR, LPCSTR, DWORD);
IStream SHOpenRegStreamW(HKEY, LPCWSTR, LPCWSTR, DWORD);

version (Unicode) {
alias ChrCmpIW ChrCmpI;
alias IntlStrEqNW IntlStrEqN;
alias IntlStrEqNIW IntlStrEqNI;
alias IntlStrEqWorkerW IntlStrEqWorker;
alias SHStrDupW SHStrDup;
alias StrCatW StrCat;
alias StrCatBuffW StrCatBuff;
alias StrChrW StrChr;
alias StrChrIW StrChrI;
alias StrCmpW StrCmp;
alias StrCmpIW StrCmpI;
alias StrCmpNIW StrCmpNI;
alias StrCmpNW StrCmpN;
alias StrCpyNW StrCpyN;
alias StrCpyW StrCpy;
alias StrCSpnIW StrCSpnI;
alias StrCSpnW StrCSpn;
alias StrDupW StrDup;
alias StrFormatByteSizeW StrFormatByteSize;
alias StrFormatKBSizeW StrFormatKBSize;
alias StrFromTimeIntervalW StrFromTimeInterval;
alias StrIsIntlEqualW StrIsIntlEqual;
alias StrNCatW StrNCat;
alias StrPBrkW StrPBrk;
alias StrRChrW StrRChr;
alias StrRChrIW StrRChrI;
alias StrRetToBufW StrRetToBuf;
alias StrRetToStrW StrRetToStr;
alias StrRStrIW StrRStrI;
alias StrSpnW StrSpn;
alias StrStrIW StrStrI;
alias StrStrW StrStr;
alias StrToIntW StrToInt;
alias StrToIntExW StrToIntEx;
alias StrTrimW StrTrim;
alias PathAddBackslashW PathAddBackslash;
alias PathAddExtensionW PathAddExtension;
alias PathAppendW PathAppend;
alias PathBuildRootW PathBuildRoot;
alias PathCanonicalizeW PathCanonicalize;
alias PathCombineW PathCombine;
alias PathCommonPrefixW PathCommonPrefix;
alias PathCompactPathW PathCompactPath;
alias PathCompactPathExW PathCompactPathEx;
alias PathCreateFromUrlW PathCreateFromUrl;
alias PathFileExistsW PathFileExists;
alias PathFindExtensionW PathFindExtension;
alias PathFindFileNameW PathFindFileName;
alias PathFindNextComponentW PathFindNextComponent;
alias PathFindOnPathW PathFindOnPath;
alias PathFindSuffixArrayW PathFindSuffixArray;
alias PathGetArgsW PathGetArgs;
alias PathGetCharTypeW PathGetCharType;
alias PathGetDriveNumberW PathGetDriveNumber;
alias PathIsContentTypeW PathIsContentType;
alias PathIsDirectoryEmptyW PathIsDirectoryEmpty;
alias PathIsDirectoryW PathIsDirectory;
alias PathIsFileSpecW PathIsFileSpec;
alias PathIsLFNFileSpecW PathIsLFNFileSpec;
alias PathIsNetworkPathW PathIsNetworkPath;
alias PathIsPrefixW PathIsPrefix;
alias PathIsRelativeW PathIsRelative;
alias PathIsRootW PathIsRoot;
alias PathIsSameRootW PathIsSameRoot;
alias PathIsSystemFolderW PathIsSystemFolder;
alias PathIsUNCServerShareW PathIsUNCServerShare;
alias PathIsUNCServerW PathIsUNCServer;
alias PathIsUNCW PathIsUNC;
alias PathIsURLW PathIsURL;
alias PathMakePrettyW PathMakePretty;
alias PathMakeSystemFolderW PathMakeSystemFolder;
alias PathMatchSpecW PathMatchSpec;
alias PathParseIconLocationW PathParseIconLocation;
alias PathQuoteSpacesW PathQuoteSpaces;
alias PathRelativePathToW PathRelativePathTo;
alias PathRemoveArgsW PathRemoveArgs;
alias PathRemoveBackslashW PathRemoveBackslash;
alias PathRemoveBlanksW PathRemoveBlanks;
alias PathRemoveExtensionW PathRemoveExtension;
alias PathRemoveFileSpecW PathRemoveFileSpec;
alias PathRenameExtensionW PathRenameExtension;
alias PathSearchAndQualifyW PathSearchAndQualify;
alias PathSetDlgItemPathW PathSetDlgItemPath;
alias PathSkipRootW PathSkipRoot;
alias PathStripPathW PathStripPath;
alias PathStripToRootW PathStripToRoot;
alias PathUndecorateW PathUndecorate;
alias PathUnExpandEnvStringsW PathUnExpandEnvStrings;
alias PathUnmakeSystemFolderW PathUnmakeSystemFolder;
alias PathUnquoteSpacesW PathUnquoteSpaces;
alias SHCreateStreamOnFileW SHCreateStreamOnFile;
alias SHOpenRegStreamW SHOpenRegStream;
alias SHOpenRegStream2W SHOpenRegStream2;
alias SHCopyKeyW SHCopyKey;
alias SHDeleteEmptyKeyW SHDeleteEmptyKey;
alias SHDeleteKeyW SHDeleteKey;
alias SHEnumKeyExW SHEnumKeyEx;
alias SHQueryInfoKeyW SHQueryInfoKey;
alias SHQueryValueExW SHQueryValueEx;
alias SHEnumValueW SHEnumValue;
alias SHGetValueW SHGetValue;
alias SHSetValueW SHSetValue;
alias SHDeleteValueW SHDeleteValue;
alias AssocQueryKeyW AssocQueryKey;
alias AssocQueryStringByKeyW AssocQueryStringByKey;
alias AssocQueryStringW AssocQueryString;
alias UrlApplySchemeW UrlApplyScheme;
alias UrlCanonicalizeW UrlCanonicalize;
alias UrlCombineW UrlCombine;
alias UrlCompareW UrlCompare;
alias UrlCreateFromPathW UrlCreateFromPath;
alias UrlEscapeW UrlEscape;
alias UrlGetLocationW UrlGetLocation;
alias UrlGetPartW UrlGetPart;
alias UrlHashW UrlHash;
alias UrlIsW UrlIs;
alias UrlIsFileUrlW UrlIsFileUrl;
alias UrlIsNoHistoryW UrlIsNoHistory;
alias UrlIsOpaqueW UrlIsOpaque;
alias UrlUnescapeW UrlUnescape;
alias UrlUnescapeInPlaceW UrlUnescapeInPlace;
alias SHRegCreateUSKeyW SHRegCreateUSKey;
alias SHRegDeleteEmptyUSKeyW SHRegDeleteEmptyUSKey;
alias SHRegDeleteUSValueW SHRegDeleteUSValue;
alias SHRegEnumUSKeyW SHRegEnumUSKey;
alias SHRegEnumUSValueW SHRegEnumUSValue;
alias SHRegGetBoolUSValueW SHRegGetBoolUSValue;
alias SHRegGetPathW SHRegGetPath;
alias SHRegGetUSValueW SHRegGetUSValue;
alias SHRegOpenUSKeyW SHRegOpenUSKey;
alias SHRegQueryInfoUSKeyW SHRegQueryInfoUSKey;
alias SHRegQueryUSValueW SHRegQueryUSValue;
alias SHRegSetPathW SHRegSetPath;
alias SHRegSetUSValueW SHRegSetUSValue;
alias SHRegWriteUSValueW SHRegWriteUSValue;
//alias wnsprintfW wnsprintf;
//alias wvnsprintfW wvnsprintf;
} else {
alias ChrCmpIA ChrCmpI;
alias IntlStrEqNA IntlStrEqN;
alias IntlStrEqNIA IntlStrEqNI;
alias IntlStrEqWorkerA IntlStrEqWorker;
alias SHStrDupA SHStrDup;
alias StrCatBuffA StrCatBuff;
alias StrChrA StrChr;
alias StrChrIA StrChrI;
alias StrCmpNIA StrCmpNI;
alias StrCmpNA StrCmpN;
alias StrCSpnIA StrCSpnI;
alias StrCSpnA StrCSpn;
alias StrDupA StrDup;
alias StrFormatByteSizeA StrFormatByteSize;
alias StrFormatKBSizeA StrFormatKBSize;
alias StrFromTimeIntervalA StrFromTimeInterval;
alias StrIsIntlEqualA StrIsIntlEqual;
alias StrNCatA StrNCat;
alias StrPBrkA StrPBrk;
alias StrRChrA StrRChr;
alias StrRChrIA StrRChrI;
alias StrRetToBufA StrRetToBuf;
alias StrRetToStrA StrRetToStr;
alias StrRStrIA StrRStrI;
alias StrSpnA StrSpn;
alias StrStrIA StrStrI;
alias StrStrA StrStr;
alias StrToIntA StrToInt;
alias StrToIntExA StrToIntEx;
alias StrTrimA StrTrim;
alias PathAddBackslashA PathAddBackslash;
alias PathAddExtensionA PathAddExtension;
alias PathAppendA PathAppend;
alias PathBuildRootA PathBuildRoot;
alias PathCanonicalizeA PathCanonicalize;
alias PathCombineA PathCombine;
alias PathCommonPrefixA PathCommonPrefix;
alias PathCompactPathA PathCompactPath;
alias PathCompactPathExA PathCompactPathEx;
alias PathCreateFromUrlA PathCreateFromUrl;
alias PathFileExistsA PathFileExists;
alias PathFindExtensionA PathFindExtension;
alias PathFindFileNameA PathFindFileName;
alias PathFindNextComponentA PathFindNextComponent;
alias PathFindOnPathA PathFindOnPath;
alias PathFindSuffixArrayA PathFindSuffixArray;
alias PathGetArgsA PathGetArgs;
alias PathGetCharTypeA PathGetCharType;
alias PathGetDriveNumberA PathGetDriveNumber;
alias PathIsContentTypeA PathIsContentType;
alias PathIsDirectoryEmptyA PathIsDirectoryEmpty;
alias PathIsDirectoryA PathIsDirectory;
alias PathIsFileSpecA PathIsFileSpec;
alias PathIsLFNFileSpecA PathIsLFNFileSpec;
alias PathIsNetworkPathA PathIsNetworkPath;
alias PathIsPrefixA PathIsPrefix;
alias PathIsRelativeA PathIsRelative;
alias PathIsRootA PathIsRoot;
alias PathIsSameRootA PathIsSameRoot;
alias PathIsSystemFolderA PathIsSystemFolder;
alias PathIsUNCServerShareA PathIsUNCServerShare;
alias PathIsUNCServerA PathIsUNCServer;
alias PathIsUNCA PathIsUNC;
alias PathIsURLA PathIsURL;
alias PathMakePrettyA PathMakePretty;
alias PathMakeSystemFolderA PathMakeSystemFolder;
alias PathMatchSpecA PathMatchSpec;
alias PathParseIconLocationA PathParseIconLocation;
alias PathQuoteSpacesA PathQuoteSpaces;
alias PathRelativePathToA PathRelativePathTo;
alias PathRemoveArgsA PathRemoveArgs;
alias PathRemoveBackslashA PathRemoveBackslash;
alias PathRemoveBlanksA PathRemoveBlanks;
alias PathRemoveExtensionA PathRemoveExtension;
alias PathRemoveFileSpecA PathRemoveFileSpec;
alias PathRenameExtensionA PathRenameExtension;
alias PathSearchAndQualifyA PathSearchAndQualify;
alias PathSetDlgItemPathA PathSetDlgItemPath;
alias PathSkipRootA PathSkipRoot;
alias PathStripPathA PathStripPath;
alias PathStripToRootA PathStripToRoot;
alias PathUndecorateA PathUndecorate;
alias PathUnExpandEnvStringsA PathUnExpandEnvStrings;
alias PathUnmakeSystemFolderA PathUnmakeSystemFolder;
alias PathUnquoteSpacesA PathUnquoteSpaces;
alias SHCreateStreamOnFileA SHCreateStreamOnFile;
alias SHOpenRegStreamA SHOpenRegStream;
alias SHOpenRegStream2A SHOpenRegStream2;
alias SHCopyKeyA SHCopyKey;
alias SHDeleteEmptyKeyA SHDeleteEmptyKey;
alias SHDeleteKeyA SHDeleteKey;
alias SHEnumKeyExA SHEnumKeyEx;
alias SHQueryInfoKeyA SHQueryInfoKey;
alias SHQueryValueExA SHQueryValueEx;
alias SHEnumValueA SHEnumValue;
alias SHGetValueA SHGetValue;
alias SHSetValueA SHSetValue;
alias SHDeleteValueA SHDeleteValue;
alias AssocQueryKeyA AssocQueryKey;
alias AssocQueryStringByKeyA AssocQueryStringByKey;
alias AssocQueryStringA AssocQueryString;
alias UrlApplySchemeA UrlApplyScheme;
alias UrlCanonicalizeA UrlCanonicalize;
alias UrlCombineA UrlCombine;
alias UrlCompareA UrlCompare;
alias UrlCreateFromPathA UrlCreateFromPath;
alias UrlEscapeA UrlEscape;
alias UrlGetLocationA UrlGetLocation;
alias UrlGetPartA UrlGetPart;
alias UrlHashA UrlHash;
alias UrlIsA UrlIs;
alias UrlIsNoHistoryA UrlIsNoHistory;
alias UrlIsOpaqueA UrlIsOpaque;
alias UrlUnescapeA UrlUnescape;
alias UrlUnescapeInPlaceA UrlUnescapeInPlace;
alias SHRegCreateUSKeyA SHRegCreateUSKey;
alias SHRegDeleteEmptyUSKeyA SHRegDeleteEmptyUSKey;
alias SHRegDeleteUSValueA SHRegDeleteUSValue;
alias SHRegEnumUSKeyA SHRegEnumUSKey;
alias SHRegEnumUSValueA SHRegEnumUSValue;
alias SHRegGetBoolUSValueA SHRegGetBoolUSValue;
alias SHRegGetPathA SHRegGetPath;
alias SHRegGetUSValueA SHRegGetUSValue;
alias SHRegOpenUSKeyA SHRegOpenUSKey;
alias SHRegQueryInfoUSKeyA SHRegQueryInfoUSKey;
alias SHRegQueryUSValueA SHRegQueryUSValue;
alias SHRegSetPathA SHRegSetPath;
alias SHRegSetUSValueA SHRegSetUSValue;
alias SHRegWriteUSValueA SHRegWriteUSValue;
//alias wnsprintfA wnsprintf;
//alias wvnsprintfA wvnsprintf;
}

alias StrToInt StrToLong;
