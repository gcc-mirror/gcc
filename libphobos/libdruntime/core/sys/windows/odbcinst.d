/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_odbcinst.d)
 */
module core.sys.windows.odbcinst;
version (Windows):
@system:

version (ANSI) {} else version = Unicode;

import core.sys.windows.sql;
import core.sys.windows.windef;

/*  FIXME: The Unicode/Ansi functions situation is a mess. How do the xxxA
 *  versions of these functions fit into the scheme?
 */

// SQLConfigDataSource()
enum : WORD {
    ODBC_ADD_DSN            = 1,
    ODBC_CONFIG_DSN         = 2,
    ODBC_REMOVE_DSN         = 3,
    ODBC_ADD_SYS_DSN        = 4,
    ODBC_CONFIG_SYS_DSN     = 5,
    ODBC_REMOVE_SYS_DSN     = 6,
    ODBC_REMOVE_DEFAULT_DSN = 7
}

// ODBC 3.0+
enum : WORD {
    ODBC_INSTALL_INQUIRY  = 1,
    ODBC_INSTALL_COMPLETE = 2
}

// ODBC 2.5+
enum : WORD {
    ODBC_INSTALL_DRIVER    = 1,
    ODBC_REMOVE_DRIVER     = 2,
    ODBC_CONFIG_DRIVER     = 3,
    ODBC_CONFIG_DRIVER_MAX = 100
}

// ODBC 3.0+
// SQLSetConfigMode()
enum : UWORD {
    ODBC_BOTH_DSN   = 0,
    ODBC_USER_DSN   = 1,
    ODBC_SYSTEM_DSN = 2
}

enum : DWORD {
    ODBC_ERROR_GENERAL_ERR             = 1,
    ODBC_ERROR_INVALID_BUFF_LEN        = 2,
    ODBC_ERROR_INVALID_HWND            = 3,
    ODBC_ERROR_INVALID_STR             = 4,
    ODBC_ERROR_INVALID_REQUEST_TYPE    = 5,
    ODBC_ERROR_COMPONENT_NOT_FOUND     = 6,
    ODBC_ERROR_INVALID_NAME            = 7,
    ODBC_ERROR_INVALID_KEYWORD_VALUE   = 8,
    ODBC_ERROR_INVALID_DSN             = 9,
    ODBC_ERROR_INVALID_INF             = 10,
    ODBC_ERROR_REQUEST_FAILED          = 11,
    ODBC_ERROR_INVALID_PATH            = 12,
    ODBC_ERROR_LOAD_LIB_FAILED         = 13,
    ODBC_ERROR_INVALID_PARAM_SEQUENCE  = 14,
    ODBC_ERROR_INVALID_LOG_FILE        = 15,
    ODBC_ERROR_USER_CANCELED           = 16,
    ODBC_ERROR_USAGE_UPDATE_FAILED     = 17,
    ODBC_ERROR_CREATE_DSN_FAILED       = 18,
    ODBC_ERROR_WRITING_SYSINFO_FAILED  = 19,
    ODBC_ERROR_REMOVE_DSN_FAILED       = 20,
    ODBC_ERROR_OUT_OF_MEM              = 21,
    ODBC_ERROR_OUTPUT_STRING_TRUNCATED = 22
}

extern (Windows):
BOOL  ConfigDSN(HWND,WORD,LPCSTR,LPCSTR);
BOOL  ConfigDSNW(HWND,WORD,LPCWSTR,LPCWSTR);
BOOL  ConfigTranslator(HWND,DWORD*);
BOOL  SQLConfigDataSource(HWND,WORD,LPCSTR,LPCSTR);
BOOL  SQLConfigDataSourceW(HWND,WORD,LPCWSTR,LPCWSTR);
BOOL  SQLCreateDataSource(HWND,LPCSTR);
BOOL  SQLCreateDataSourceW(HWND,LPCWSTR);
BOOL  SQLGetAvailableDrivers(LPCSTR,LPSTR,WORD,WORD*);
BOOL  SQLGetAvailableDriversW(LPCWSTR,LPWSTR,WORD,WORD*);
BOOL  SQLGetInstalledDrivers(LPSTR,WORD,WORD*);
BOOL  SQLGetInstalledDriversW(LPWSTR,WORD,WORD*);
int  SQLGetPrivateProfileString(LPCSTR,LPCSTR,LPCSTR,LPSTR,int,LPCSTR);
int  SQLGetPrivateProfileStringW(LPCWSTR,LPCWSTR,LPCWSTR,LPWSTR,int,LPCWSTR);
BOOL  SQLGetTranslator(HWND,LPSTR,WORD,WORD*,LPSTR,WORD,WORD*,DWORD*);
BOOL  SQLGetTranslatorW(HWND,LPWSTR,WORD,WORD*,LPWSTR,WORD,WORD*,DWORD*);
BOOL  SQLInstallDriver(LPCSTR,LPCSTR,LPSTR,WORD,WORD*);
BOOL  SQLInstallDriverManager(LPSTR,WORD,WORD*);
BOOL  SQLInstallDriverManagerW(LPWSTR,WORD,WORD*);
BOOL  SQLInstallDriverW(LPCWSTR,LPCWSTR,LPWSTR,WORD,WORD*);
BOOL  SQLInstallODBC(HWND,LPCSTR,LPCSTR,LPCSTR);
BOOL  SQLInstallODBCW(HWND,LPCWSTR,LPCWSTR,LPCWSTR);
BOOL  SQLManageDataSources(HWND);
BOOL  SQLRemoveDefaultDataSource();
BOOL  SQLRemoveDSNFromIni(LPCSTR);
BOOL  SQLRemoveDSNFromIniW(LPCWSTR);
BOOL  SQLValidDSN(LPCSTR);
BOOL  SQLValidDSNW(LPCWSTR);
BOOL  SQLWriteDSNToIni(LPCSTR,LPCSTR);
BOOL  SQLWriteDSNToIniW(LPCWSTR,LPCWSTR);
BOOL  SQLWritePrivateProfileString(LPCSTR,LPCSTR,LPCSTR,LPCSTR);
BOOL  SQLWritePrivateProfileStringW(LPCWSTR,LPCWSTR,LPCWSTR,LPCWSTR);

static if (ODBCVER >= 0x0250) {
    BOOL  ConfigDriver(HWND,WORD,LPCSTR,LPCSTR,LPSTR,WORD,WORD*);
    BOOL  ConfigDriverW(HWND,WORD,LPCWSTR,LPCWSTR,LPWSTR,WORD,WORD*);
    BOOL  SQLConfigDriver(HWND,WORD,LPCSTR,LPCSTR,LPSTR,WORD,WORD*);
    BOOL  SQLConfigDriverW(HWND,WORD,LPCWSTR,LPCWSTR,LPWSTR,WORD,WORD*);
    deprecated ("Use SQLInstallTranslatorExW instead") {
        BOOL  SQLInstallTranslator(LPCSTR,LPCSTR,LPCSTR,LPSTR,WORD,WORD*,WORD,LPDWORD);
        BOOL  SQLInstallTranslatorW(LPCWSTR,LPCWSTR,LPCWSTR,LPWSTR,WORD,WORD*,WORD,LPDWORD);
    }
    BOOL  SQLRemoveDriver(LPCSTR,BOOL,LPDWORD);
    BOOL  SQLRemoveDriverManager(LPDWORD);
    BOOL  SQLRemoveDriverW(LPCWSTR,BOOL,LPDWORD);
    BOOL  SQLRemoveTranslator(LPCSTR,LPDWORD);
    BOOL  SQLRemoveTranslatorW(LPCWSTR,LPDWORD);
}
static if (ODBCVER >= 0x0300) {
    BOOL  SQLGetConfigMode(UWORD*);
    BOOL  SQLInstallDriverEx(LPCSTR,LPCSTR,LPSTR,WORD,WORD*,WORD,LPDWORD);
    BOOL  SQLInstallDriverExW(LPCWSTR,LPCWSTR,LPWSTR,WORD,WORD*,WORD,LPDWORD);
    SQLRETURN  SQLInstallerError(WORD,DWORD*,LPSTR,WORD,WORD*);
    SQLRETURN  SQLInstallerErrorW(WORD,DWORD*,LPWSTR,WORD,WORD*);
    BOOL  SQLInstallTranslatorEx(LPCSTR,LPCSTR,LPSTR,WORD,WORD*,WORD,LPDWORD);
    BOOL  SQLInstallTranslatorExW(LPCWSTR,LPCWSTR,LPWSTR,WORD,WORD*,WORD,LPDWORD);
    SQLRETURN  SQLPostInstallerError(DWORD,LPCSTR);
    SQLRETURN  SQLPostInstallerErrorW(DWORD,LPCWSTR);
    BOOL  SQLReadFileDSN(LPCSTR,LPCSTR,LPCSTR,LPSTR,WORD,WORD*);
    BOOL  SQLReadFileDSNW(LPCWSTR,LPCWSTR,LPCWSTR,LPWSTR,WORD,WORD*);
    BOOL  SQLSetConfigMode(UWORD);
    BOOL  SQLWriteFileDSN(LPCSTR,LPCSTR,LPCSTR,LPCSTR);
    BOOL  SQLWriteFileDSNW(LPCWSTR,LPCWSTR,LPCWSTR,LPCWSTR);
}

version (Unicode) {
    alias SQLConfigDataSourceW SQLConfigDataSource;
    alias SQLConfigDriverW SQLConfigDriver;
    alias SQLCreateDataSourceW SQLCreateDataSource;
    alias SQLGetAvailableDriversW SQLGetAvailableDrivers;
    alias SQLGetInstalledDriversW SQLGetInstalledDrivers;
    alias SQLGetPrivateProfileStringW SQLGetPrivateProfileString;
    alias SQLGetTranslatorW SQLGetTranslator;
    alias SQLInstallDriverW SQLInstallDriver;
    alias SQLInstallDriverExW SQLInstallDriverEx;
    alias SQLInstallDriverManagerW SQLInstallDriverManager;
    alias SQLInstallerErrorW SQLInstallerError;
    alias SQLInstallODBCW SQLInstallODBC;
    deprecated alias SQLInstallTranslatorW SQLInstallTranslator;
    alias SQLInstallTranslatorExW SQLInstallTranslatorEx;
    alias SQLPostInstallerErrorW SQLPostInstallerError;
    alias SQLReadFileDSNW SQLReadFileDSN;
    alias SQLRemoveDriverW SQLRemoveDriver;
    alias SQLRemoveDSNFromIniW SQLRemoveDSNFromIni;
    alias SQLRemoveTranslatorW SQLRemoveTranslator;
    alias SQLValidDSNW SQLValidDSN;
    alias SQLWriteDSNToIniW SQLWriteDSNToIni;
    alias SQLWriteFileDSNW SQLWriteFileDSN;
    alias SQLWritePrivateProfileStringW SQLWritePrivateProfileString;
}
