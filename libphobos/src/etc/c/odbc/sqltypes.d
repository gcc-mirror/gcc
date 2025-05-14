/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(PHOBOSSRC etc/c/odbc/_sqltypes.d)

Declarations for interfacing with the ODBC library.

See_Also: $(LINK2 https://docs.microsoft.com/en-us/sql/odbc/reference/syntax/odbc-api-reference,
            ODBC API Reference on MSDN)
 */
deprecated ("The ODBC 3.5 modules are deprecated. Please use the ODBC4 modules in the `etc.c.odbc` package.")
module etc.c.odbc.sqltypes;

version (ANSI) {} else version = Unicode;

/* Conversion notes:
  It's assumed that ODBC >= 0x0300.
*/

// Import windows types or declare appropriate aliases if we are not.
version (Windows)
{
    public import core.sys.windows.windef;
    public import core.sys.windows.basetyps; // for GUID
}
else
{
    alias ushort USHORT, WORD;
    alias uint ULONG, LONG, BOOL, DWORD;
    alias void VOID;
    alias void* PVOID, HANDLE, HWND;
    alias ubyte UCHAR, BYTE;
    alias wchar* LPWSTR;

    align(1) struct GUID {  // size is 16
        align(1):
        DWORD   Data1;
        WORD    Data2;
        WORD    Data3;
        BYTE[8] Data4;
    }
}
alias GUID SQLGUID;

alias byte SCHAR, SQLSCHAR;
alias int SDWORD, SLONG, SQLINTEGER;
alias short SWORD, SSHORT, RETCODE, SQLSMALLINT;
alias ULONG UDWORD;
alias USHORT UWORD, SQLUSMALLINT;
alias double SDOUBLE, LDOUBLE;
alias float SFLOAT;
alias PVOID PTR, HENV, HDBC, HSTMT, SQLPOINTER;
alias UCHAR SQLCHAR;
// #ifndef _WIN64
alias UDWORD SQLUINTEGER;
// #endif

//static if (ODBCVER >= 0x0300)
//{
alias HANDLE SQLHANDLE;
alias SQLHANDLE SQLHENV, SQLHDBC, SQLHSTMT, SQLHDESC;
/*
}
else
{
alias void* SQLHENV;
alias void* SQLHDBC;
alias void* SQLHSTMT;
}
*/
alias SQLSMALLINT SQLRETURN;
alias HWND SQLHWND;
alias ULONG BOOKMARK;

alias SQLINTEGER SQLLEN, SQLROWOFFSET;
alias SQLUINTEGER SQLROWCOUNT, SQLULEN;
alias DWORD SQLTRANSID;
alias SQLUSMALLINT SQLSETPOSIROW;
alias wchar SQLWCHAR;

version (Unicode)
{
    alias SQLWCHAR SQLTCHAR;
}
else
{
    alias SQLCHAR  SQLTCHAR;
}
//static if (ODBCVER >= 0x0300)
//{
alias ubyte  SQLDATE, SQLDECIMAL;
alias double SQLDOUBLE, SQLFLOAT;
alias ubyte  SQLNUMERIC;
alias float  SQLREAL;
alias ubyte  SQLTIME, SQLTIMESTAMP, SQLVARCHAR;
alias long   ODBCINT64, SQLBIGINT;
alias ulong  SQLUBIGINT;
//}

struct DATE_STRUCT {
    SQLSMALLINT year;
    SQLUSMALLINT month;
    SQLUSMALLINT day;
}

struct TIME_STRUCT {
    SQLUSMALLINT hour;
    SQLUSMALLINT minute;
    SQLUSMALLINT second;
}

struct TIMESTAMP_STRUCT {
    SQLSMALLINT year;
    SQLUSMALLINT month;
    SQLUSMALLINT day;
    SQLUSMALLINT hour;
    SQLUSMALLINT minute;
    SQLUSMALLINT second;
    SQLUINTEGER fraction;
}

//static if (ODBCVER >= 0x0300)
//{
alias DATE_STRUCT SQL_DATE_STRUCT;
alias TIME_STRUCT SQL_TIME_STRUCT;
alias TIMESTAMP_STRUCT SQL_TIMESTAMP_STRUCT;

enum SQLINTERVAL {
    SQL_IS_YEAR = 1,
    SQL_IS_MONTH,
    SQL_IS_DAY,
    SQL_IS_HOUR,
    SQL_IS_MINUTE,
    SQL_IS_SECOND,
    SQL_IS_YEAR_TO_MONTH,
    SQL_IS_DAY_TO_HOUR,
    SQL_IS_DAY_TO_MINUTE,
    SQL_IS_DAY_TO_SECOND,
    SQL_IS_HOUR_TO_MINUTE,
    SQL_IS_HOUR_TO_SECOND,
    SQL_IS_MINUTE_TO_SECOND
}

struct SQL_YEAR_MONTH_STRUCT {
    SQLUINTEGER year;
    SQLUINTEGER month;
}

struct SQL_DAY_SECOND_STRUCT {
    SQLUINTEGER day;
    SQLUINTEGER hour;
    SQLUINTEGER minute;
    SQLUINTEGER second;
    SQLUINTEGER fraction;
}

struct SQL_INTERVAL_STRUCT {
    SQLINTERVAL interval_type;
    SQLSMALLINT interval_sign;
    union _intval {
        SQL_YEAR_MONTH_STRUCT year_month;
        SQL_DAY_SECOND_STRUCT day_second;
    }
    _intval intval;
}

enum SQL_MAX_NUMERIC_LEN = 16;

struct SQL_NUMERIC_STRUCT {
    SQLCHAR precision;
    SQLSCHAR scale;
    SQLCHAR sign;
    SQLCHAR[SQL_MAX_NUMERIC_LEN] val;
}
// } ODBCVER >= 0x0300
