/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_lmcons.d)
 */
module core.sys.windows.lmcons;
version (Windows):
@system:

version (ANSI) {} else version = Unicode;

import core.sys.windows.windef;
import core.sys.windows.lmerr; // for NERR_BASE

const TCHAR[]
    MESSAGE_FILENAME = "NETMSG",
    OS2MSG_FILENAME = "BASE",
    HELP_MSG_FILENAME = "NETH";

alias DWORD NET_API_STATUS, API_RET_TYPE;

enum MIN_LANMAN_MESSAGE_ID = NERR_BASE;
enum MAX_LANMAN_MESSAGE_ID = 5799;

enum CNLEN        = 15; /* also in nddeapi.h */
enum UNCLEN       = CNLEN + 2;

enum DNLEN        = 15;
enum LM20_CNLEN   = 15;
enum LM20_DNLEN   = 15;
enum LM20_SNLEN   = 15;
enum LM20_STXTLEN = 63;
enum LM20_UNCLEN  = LM20_CNLEN + 2;
enum LM20_NNLEN   = 12;
enum LM20_RMLEN   = LM20_UNCLEN + 1 + LM20_NNLEN;
enum NNLEN        = 80;
enum RMLEN        = UNCLEN + 1 + NNLEN;
enum SNLEN        = 80;
enum STXTLEN      = 256;
enum PATHLEN      = 256;
enum LM20_PATHLEN = 256;
enum DEVLEN       = 80;
enum LM20_DEVLEN  = 8;
enum EVLEN        = 16;
enum UNLEN        = 256;
enum LM20_UNLEN   = 20;
enum GNLEN        = UNLEN;
enum LM20_GNLEN   = LM20_UNLEN;
enum PWLEN        = 256;
enum LM20_PWLEN   = 14;
enum SHPWLEN      = 8;
enum CLTYPE_LEN   = 12;
enum QNLEN        = NNLEN;
enum LM20_QNLEN   = LM20_NNLEN;

enum MAXCOMMENTSZ = 256;
enum LM20_MAXCOMMENTSZ = 48;
enum ALERTSZ      = 128;
enum MAXDEVENTRIES = 32;// (sizeof(int)*8);
enum NETBIOS_NAME_LEN = 16;
enum DWORD MAX_PREFERRED_LENGTH = -1;
enum CRYPT_KEY_LEN = 7;
enum CRYPT_TXT_LEN = 8;
enum ENCRYPTED_PWLEN = 16;
enum SESSION_PWLEN = 24;
enum SESSION_CRYPT_KLEN = 21;

enum PARMNUM_ALL = 0;
enum DWORD PARM_ERROR_UNKNOWN = -1;
enum PARM_ERROR_NONE = 0;
enum PARMNUM_BASE_INFOLEVEL = 1000;

enum PLATFORM_ID_DOS = 300;
enum PLATFORM_ID_OS2 = 400;
enum PLATFORM_ID_NT  = 500;
enum PLATFORM_ID_OSF = 600;
enum PLATFORM_ID_VMS = 700;

// this is a new typedef in W2K, but it should be harmless for earlier Windows versions.
version (Unicode) {
    alias LPWSTR LMSTR;
    alias LPCWSTR LMCSTR;
} else {
    alias LPSTR LMSTR;
    alias LPCSTR LMCSTR;
}
