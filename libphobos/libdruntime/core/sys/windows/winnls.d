/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_winnls.d)
 */
module core.sys.windows.winnls;
version (Windows):

version (ANSI) {} else version = Unicode;
pragma(lib, "kernel32");

private import core.sys.windows.basetsd, core.sys.windows.w32api, core.sys.windows.winbase, core.sys.windows.windef;

alias DWORD LCTYPE, CALTYPE, CALID, LGRPID, GEOID, GEOTYPE, GEOCLASS;

enum size_t
    MAX_DEFAULTCHAR =  2,
    MAX_LEADBYTES   = 12;

enum LCTYPE
    LOCALE_USE_CP_ACP    = 0x40000000,
    LOCALE_RETURN_NUMBER = 0x20000000;

enum : LCTYPE {
    LOCALE_ILANGUAGE = 1,
    LOCALE_SLANGUAGE,
    LOCALE_SABBREVLANGNAME,
    LOCALE_SNATIVELANGNAME,
    LOCALE_ICOUNTRY,
    LOCALE_SCOUNTRY,
    LOCALE_SABBREVCTRYNAME,
    LOCALE_SNATIVECTRYNAME,
    LOCALE_IDEFAULTLANGUAGE,
    LOCALE_IDEFAULTCOUNTRY,
    LOCALE_IDEFAULTCODEPAGE,
    LOCALE_SLIST,
    LOCALE_IMEASURE,
    LOCALE_SDECIMAL,
    LOCALE_STHOUSAND,
    LOCALE_SGROUPING,
    LOCALE_IDIGITS,
    LOCALE_ILZERO,
    LOCALE_SNATIVEDIGITS,
    LOCALE_SCURRENCY,
    LOCALE_SINTLSYMBOL,
    LOCALE_SMONDECIMALSEP,
    LOCALE_SMONTHOUSANDSEP,
    LOCALE_SMONGROUPING,
    LOCALE_ICURRDIGITS,
    LOCALE_IINTLCURRDIGITS,
    LOCALE_ICURRENCY,
    LOCALE_INEGCURR,
    LOCALE_SDATE,
    LOCALE_STIME,
    LOCALE_SSHORTDATE,
    LOCALE_SLONGDATE,
    LOCALE_IDATE,
    LOCALE_ILDATE,
    LOCALE_ITIME,
    LOCALE_ICENTURY,
    LOCALE_ITLZERO,
    LOCALE_IDAYLZERO,
    LOCALE_IMONLZERO,
    LOCALE_S1159,
    LOCALE_S2359,
    LOCALE_SDAYNAME1,
    LOCALE_SDAYNAME2,
    LOCALE_SDAYNAME3,
    LOCALE_SDAYNAME4,
    LOCALE_SDAYNAME5,
    LOCALE_SDAYNAME6,
    LOCALE_SDAYNAME7,
    LOCALE_SABBREVDAYNAME1,
    LOCALE_SABBREVDAYNAME2,
    LOCALE_SABBREVDAYNAME3,
    LOCALE_SABBREVDAYNAME4,
    LOCALE_SABBREVDAYNAME5,
    LOCALE_SABBREVDAYNAME6,
    LOCALE_SABBREVDAYNAME7,
    LOCALE_SMONTHNAME1,
    LOCALE_SMONTHNAME2,
    LOCALE_SMONTHNAME3,
    LOCALE_SMONTHNAME4,
    LOCALE_SMONTHNAME5,
    LOCALE_SMONTHNAME6,
    LOCALE_SMONTHNAME7,
    LOCALE_SMONTHNAME8,
    LOCALE_SMONTHNAME9,
    LOCALE_SMONTHNAME10,
    LOCALE_SMONTHNAME11,
    LOCALE_SMONTHNAME12,
    LOCALE_SABBREVMONTHNAME1,
    LOCALE_SABBREVMONTHNAME2,
    LOCALE_SABBREVMONTHNAME3,
    LOCALE_SABBREVMONTHNAME4,
    LOCALE_SABBREVMONTHNAME5,
    LOCALE_SABBREVMONTHNAME6,
    LOCALE_SABBREVMONTHNAME7,
    LOCALE_SABBREVMONTHNAME8,
    LOCALE_SABBREVMONTHNAME9,
    LOCALE_SABBREVMONTHNAME10,
    LOCALE_SABBREVMONTHNAME11,
    LOCALE_SABBREVMONTHNAME12,
    LOCALE_SPOSITIVESIGN,
    LOCALE_SNEGATIVESIGN,
    LOCALE_IPOSSIGNPOSN,
    LOCALE_INEGSIGNPOSN,
    LOCALE_IPOSSYMPRECEDES,
    LOCALE_IPOSSEPBYSPACE,
    LOCALE_INEGSYMPRECEDES,
    LOCALE_INEGSEPBYSPACE,
    LOCALE_FONTSIGNATURE,
    LOCALE_SISO639LANGNAME,
    LOCALE_SISO3166CTRYNAME, // = 90
    LOCALE_SENGLANGUAGE         = 0x1001,
    LOCALE_SENGCOUNTRY          = 0x1002,
    LOCALE_IDEFAULTANSICODEPAGE = 0x1004,
    LOCALE_INEGNUMBER           = 0x1010,
    LOCALE_STIMEFORMAT          = 0x1003,
    LOCALE_ITIMEMARKPOSN        = 0x1005,
    LOCALE_ICALENDARTYPE        = 0x1009,
    LOCALE_IOPTIONALCALENDAR    = 0x100B,
    LOCALE_IFIRSTDAYOFWEEK      = 0x100C,
    LOCALE_IFIRSTWEEKOFYEAR     = 0x100D,
    LOCALE_SMONTHNAME13         = 0x100E,
    LOCALE_SABBREVMONTHNAME13   = 0x100F
}

enum : LCID {
    LOCALE_USER_DEFAULT   = 0x400,
    LOCALE_SYSTEM_DEFAULT = 0x800
}

enum DWORD
    NORM_IGNORECASE     =       1,
    NORM_IGNORENONSPACE =       2,
    NORM_IGNORESYMBOLS  =       4,
    SORT_STRINGSORT     = 0x01000,
    NORM_IGNOREKANATYPE = 0x10000,
    NORM_IGNOREWIDTH    = 0x20000;

enum DWORD
    LCMAP_LOWERCASE           = 0x00000100,
    LCMAP_UPPERCASE           = 0x00000200,
    LCMAP_SORTKEY             = 0x00000400,
    LCMAP_BYTEREV             = 0x00000800,
    LCMAP_HIRAGANA            = 0x00100000,
    LCMAP_KATAKANA            = 0x00200000,
    LCMAP_HALFWIDTH           = 0x00400000,
    LCMAP_FULLWIDTH           = 0x00800000,
    LCMAP_LINGUISTIC_CASING   = 0x01000000,
    LCMAP_SIMPLIFIED_CHINESE  = 0x02000000,
    LCMAP_TRADITIONAL_CHINESE = 0x04000000;

enum CALID ENUM_ALL_CALENDARS = -1;

enum DWORD
    DATE_SHORTDATE        =          1,
    DATE_LONGDATE         =          2,
    DATE_USE_ALT_CALENDAR =          4,
    LOCALE_NOUSEROVERRIDE = 0x80000000;

enum : DWORD {
    CP_INSTALLED = 1,
    CP_SUPPORTED
}

enum : DWORD {
    LCID_INSTALLED       = 1,
    LCID_SUPPORTED       = 2,
    LCID_ALTERNATE_SORTS = 4
}

enum DWORD
    MAP_FOLDCZONE   =  16,
    MAP_PRECOMPOSED =  32,
    MAP_COMPOSITE   =  64,
    MAP_FOLDDIGITS  = 128;

enum : UINT {
    CP_ACP,
    CP_OEMCP,
    CP_MACCP,
    CP_THREAD_ACP, // =     3
    CP_SYMBOL         =    42,
    CP_UTF7           = 65000,
    CP_UTF8           = 65001
}

enum : DWORD {
    CT_CTYPE1 = 1,
    CT_CTYPE2 = 2,
    CT_CTYPE3 = 4
}

enum WORD
    C1_UPPER  =   1,
    C1_LOWER  =   2,
    C1_DIGIT  =   4,
    C1_SPACE  =   8,
    C1_PUNCT  =  16,
    C1_CNTRL  =  32,
    C1_BLANK  =  64,
    C1_XDIGIT = 128,
    C1_ALPHA  = 256;

enum : WORD {
    C2_NOTAPPLICABLE,
    C2_LEFTTORIGHT,
    C2_RIGHTTOLEFT,
    C2_EUROPENUMBER,
    C2_EUROPESEPARATOR,
    C2_EUROPETERMINATOR,
    C2_ARABICNUMBER,
    C2_COMMONSEPARATOR,
    C2_BLOCKSEPARATOR,
    C2_SEGMENTSEPARATOR,
    C2_WHITESPACE,
    C2_OTHERNEUTRAL  // = 11
}

enum WORD
    C3_NOTAPPLICABLE =      0,
    C3_NONSPACING    =      1,
    C3_DIACRITIC     =      2,
    C3_VOWELMARK     =      4,
    C3_SYMBOL        =      8,
    C3_KATAKANA      = 0x0010,
    C3_HIRAGANA      = 0x0020,
    C3_HALFWIDTH     = 0x0040,
    C3_FULLWIDTH     = 0x0080,
    C3_IDEOGRAPH     = 0x0100,
    C3_KASHIDA       = 0x0200,
    C3_LEXICAL       = 0x0400,
    C3_ALPHA         = 0x8000;

enum DWORD
    TIME_NOMINUTESORSECONDS = 1,
    TIME_NOSECONDS          = 2,
    TIME_NOTIMEMARKER       = 4,
    TIME_FORCE24HOURFORMAT  = 8;

enum DWORD
    MB_PRECOMPOSED       = 1,
    MB_COMPOSITE         = 2,
    MB_USEGLYPHCHARS     = 4,
    MB_ERR_INVALID_CHARS = 8;

enum DWORD
    WC_DISCARDNS      =  16,
    WC_SEPCHARS       =  32,
    WC_DEFAULTCHAR    =  64,
    WC_COMPOSITECHECK = 512;

enum : LONG {
    CTRY_DEFAULT            =   0,
    CTRY_DOMINICAN_REPUBLIC =   1,
    CTRY_PUERTO_RICO        =   1,
    CTRY_CARIBBEAN          =   1,
    CTRY_JAMAICA            =   1,
    CTRY_UNITED_STATES      =   1,
    CTRY_TRINIDAD_Y_TOBAGO  =   1,
    CTRY_CANADA             =   2,
    CTRY_RUSSIA             =   7,
    CTRY_UZBEKISTAN         =   7,
    CTRY_KAZAKSTAN          =   7,
    CTRY_TATARSTAN          =   7,
    CTRY_EGYPT              =  20,
    CTRY_SOUTH_AFRICA       =  27,
    CTRY_GREECE             =  30,
    CTRY_NETHERLANDS        =  31,
    CTRY_BELGIUM            =  32,
    CTRY_FRANCE             =  33,
    CTRY_MONACO             =  33,
    CTRY_SPAIN              =  34,
    CTRY_HUNGARY            =  36,
    CTRY_ITALY              =  39,
    CTRY_ROMANIA            =  40,
    CTRY_SWITZERLAND        =  41,
    CTRY_LIECHTENSTEIN      =  41,
    CTRY_AUSTRIA            =  43,
    CTRY_UNITED_KINGDOM     =  44,
    CTRY_DENMARK            =  45,
    CTRY_SWEDEN             =  46,
    CTRY_NORWAY             =  47,
    CTRY_POLAND             =  48,
    CTRY_GERMANY            =  49,
    CTRY_PERU               =  51,
    CTRY_MEXICO             =  52,
    CTRY_ARGENTINA          =  54,
    CTRY_BRAZIL             =  55,
    CTRY_CHILE              =  56,
    CTRY_COLOMBIA           =  57,
    CTRY_VENEZUELA          =  58,
    CTRY_MALAYSIA           =  60,
    CTRY_AUSTRALIA          =  61,
    CTRY_INDONESIA          =  62,
    CTRY_PHILIPPINES        =  63,
    CTRY_NEW_ZEALAND        =  64,
    CTRY_SINGAPORE          =  65,
    CTRY_THAILAND           =  66,
    CTRY_JAPAN              =  81,
    CTRY_SOUTH_KOREA        =  82,
    CTRY_VIET_NAM           =  84,
    CTRY_PRCHINA            =  86,
    CTRY_TURKEY             =  90,
    CTRY_INDIA              =  91,
    CTRY_PAKISTAN           =  92,
    CTRY_MOROCCO            = 212,
    CTRY_ALGERIA            = 213,
    CTRY_TUNISIA            = 216,
    CTRY_LIBYA              = 218,
    CTRY_KENYA              = 254,
    CTRY_ZIMBABWE           = 263,
    CTRY_FAEROE_ISLANDS     = 298,
    CTRY_PORTUGAL           = 351,
    CTRY_LUXEMBOURG         = 352,
    CTRY_IRELAND            = 353,
    CTRY_ICELAND            = 354,
    CTRY_ALBANIA            = 355,
    CTRY_FINLAND            = 358,
    CTRY_BULGARIA           = 359,
    CTRY_LITHUANIA          = 370,
    CTRY_LATVIA             = 371,
    CTRY_ESTONIA            = 372,
    CTRY_ARMENIA            = 374,
    CTRY_BELARUS            = 375,
    CTRY_UKRAINE            = 380,
    CTRY_SERBIA             = 381,
    CTRY_CROATIA            = 385,
    CTRY_SLOVENIA           = 386,
    CTRY_MACEDONIA          = 389,
    CTRY_CZECH              = 420,
    CTRY_SLOVAK             = 421,
    CTRY_BELIZE             = 501,
    CTRY_GUATEMALA          = 502,
    CTRY_EL_SALVADOR        = 503,
    CTRY_HONDURAS           = 504,
    CTRY_NICARAGUA          = 505,
    CTRY_COSTA_RICA         = 506,
    CTRY_PANAMA             = 507,
    CTRY_BOLIVIA            = 591,
    CTRY_ECUADOR            = 593,
    CTRY_PARAGUAY           = 595,
    CTRY_URUGUAY            = 598,
    CTRY_BRUNEI_DARUSSALAM  = 673,
    CTRY_HONG_KONG          = 852,
    CTRY_MACAU              = 853,
    CTRY_TAIWAN             = 886,
    CTRY_MALDIVES           = 960,
    CTRY_LEBANON            = 961,
    CTRY_JORDAN             = 962,
    CTRY_SYRIA              = 963,
    CTRY_IRAQ               = 964,
    CTRY_KUWAIT             = 965,
    CTRY_SAUDI_ARABIA       = 966,
    CTRY_YEMEN              = 967,
    CTRY_OMAN               = 968,
    CTRY_UAE                = 971,
    CTRY_ISRAEL             = 972,
    CTRY_BAHRAIN            = 973,
    CTRY_QATAR              = 974,
    CTRY_MONGOLIA           = 976,
    CTRY_IRAN               = 981,
    CTRY_AZERBAIJAN         = 994,
    CTRY_GEORGIA            = 995,
    CTRY_KYRGYZSTAN         = 996
}

enum : CALTYPE {
    CAL_ICALINTVALUE          = 1,
    CAL_SCALNAME,
    CAL_IYEAROFFSETRANGE,
    CAL_SERASTRING,
    CAL_SSHORTDATE,
    CAL_SLONGDATE,
    CAL_SDAYNAME1,
    CAL_SDAYNAME2,
    CAL_SDAYNAME3,
    CAL_SDAYNAME4,
    CAL_SDAYNAME5,
    CAL_SDAYNAME6,
    CAL_SDAYNAME7,
    CAL_SABBREVDAYNAME1,
    CAL_SABBREVDAYNAME2,
    CAL_SABBREVDAYNAME3,
    CAL_SABBREVDAYNAME4,
    CAL_SABBREVDAYNAME5,
    CAL_SABBREVDAYNAME6,
    CAL_SABBREVDAYNAME7,
    CAL_SMONTHNAME1,
    CAL_SMONTHNAME2,
    CAL_SMONTHNAME3,
    CAL_SMONTHNAME4,
    CAL_SMONTHNAME5,
    CAL_SMONTHNAME6,
    CAL_SMONTHNAME7,
    CAL_SMONTHNAME8,
    CAL_SMONTHNAME9,
    CAL_SMONTHNAME10,
    CAL_SMONTHNAME11,
    CAL_SMONTHNAME12,
    CAL_SMONTHNAME13,
    CAL_SABBREVMONTHNAME1,
    CAL_SABBREVMONTHNAME2,
    CAL_SABBREVMONTHNAME3,
    CAL_SABBREVMONTHNAME4,
    CAL_SABBREVMONTHNAME5,
    CAL_SABBREVMONTHNAME6,
    CAL_SABBREVMONTHNAME7,
    CAL_SABBREVMONTHNAME8,
    CAL_SABBREVMONTHNAME9,
    CAL_SABBREVMONTHNAME10,
    CAL_SABBREVMONTHNAME11,
    CAL_SABBREVMONTHNAME12,
    CAL_SABBREVMONTHNAME13 // = 46
}


enum : CALTYPE {
    CAL_GREGORIAN                =  1,
    CAL_GREGORIAN_US,
    CAL_JAPAN,
    CAL_TAIWAN,
    CAL_KOREA,
    CAL_HIJRI,
    CAL_THAI,
    CAL_HEBREW,
    CAL_GREGORIAN_ME_FRENCH,
    CAL_GREGORIAN_ARABIC,
    CAL_GREGORIAN_XLIT_ENGLISH,
    CAL_GREGORIAN_XLIT_FRENCH // = 12
}

enum : int {
    CSTR_LESS_THAN    = 1,
    CSTR_EQUAL,
    CSTR_GREATER_THAN
}

enum : DWORD {
    LGRPID_INSTALLED = 1,
    LGRPID_SUPPORTED
}

enum : LGRPID {
    LGRPID_WESTERN_EUROPE = 1,
    LGRPID_CENTRAL_EUROPE,
    LGRPID_BALTIC,
    LGRPID_GREEK,
    LGRPID_CYRILLIC,
    LGRPID_TURKISH,
    LGRPID_JAPANESE,
    LGRPID_KOREAN,
    LGRPID_TRADITIONAL_CHINESE,
    LGRPID_SIMPLIFIED_CHINESE,
    LGRPID_THAI,
    LGRPID_HEBREW,
    LGRPID_ARABIC,
    LGRPID_VIETNAMESE,
    LGRPID_INDIC,
    LGRPID_GEORGIAN,
    LGRPID_ARMENIAN // = 17
}

static if (_WIN32_WINNT >= 0x500) {
    enum : LCTYPE {
        LOCALE_SYEARMONTH             = 0x1006,
        LOCALE_SENGCURRNAME           = 0x1007,
        LOCALE_SNATIVECURRNAME        = 0x1008,
        LOCALE_IDEFAULTEBCDICCODEPAGE = 0x1012,
        LOCALE_SSORTNAME              = 0x1013,
        LOCALE_IDIGITSUBSTITUTION     = 0x1014,
        LOCALE_IPAPERSIZE             = 0x100A
    }

enum DWORD
        DATE_YEARMONTH  =  8,
        DATE_LTRREADING = 16,
        DATE_RTLREADING = 32;

enum DWORD MAP_EXPAND_LIGATURES = 0x2000;
enum DWORD WC_NO_BEST_FIT_CHARS = 1024;

    enum : CALTYPE {
        CAL_SYEARMONTH       = 47,
        CAL_ITWODIGITYEARMAX = 48,
        CAL_NOUSEROVERRIDE   = LOCALE_NOUSEROVERRIDE,
        CAL_RETURN_NUMBER    = LOCALE_RETURN_NUMBER,
        CAL_USE_CP_ACP       = LOCALE_USE_CP_ACP
    }
} // (_WIN32_WINNT >= 0x500)

extern (Windows) {
    alias BOOL function(LPSTR) CALINFO_ENUMPROCA;
    alias BOOL function(LPWSTR) CALINFO_ENUMPROCW;
    alias BOOL function(LPSTR, CALID) CALINFO_ENUMPROCEXA;
    alias BOOL function(LPWSTR, CALID) CALINFO_ENUMPROCEXW;
    alias BOOL function(LGRPID, LPSTR, LPSTR, DWORD, LONG_PTR)
      LANGUAGEGROUP_ENUMPROCA;
    alias BOOL function(LGRPID, LPWSTR, LPWSTR, DWORD, LONG_PTR)
      LANGUAGEGROUP_ENUMPROCW;
    alias BOOL function(LGRPID, LCID, LPSTR, LONG_PTR)
      LANGGROUPLOCALE_ENUMPROCA;
    alias BOOL function(LGRPID, LCID, LPWSTR, LONG_PTR)
      LANGGROUPLOCALE_ENUMPROCW;
    alias BOOL function(LPWSTR, LONG_PTR) UILANGUAGE_ENUMPROCW;
    alias BOOL function(LPSTR, LONG_PTR) UILANGUAGE_ENUMPROCA;
    alias BOOL function(LPSTR) LOCALE_ENUMPROCA;
    alias BOOL function(LPWSTR) LOCALE_ENUMPROCW;
    alias BOOL function(LPSTR) CODEPAGE_ENUMPROCA;
    alias BOOL function(LPWSTR) CODEPAGE_ENUMPROCW;
    alias BOOL function(LPSTR) DATEFMT_ENUMPROCA;
    alias BOOL function(LPWSTR) DATEFMT_ENUMPROCW;
    alias BOOL function(LPSTR, CALID) DATEFMT_ENUMPROCEXA;
    alias BOOL function(LPWSTR, CALID) DATEFMT_ENUMPROCEXW;
    alias BOOL function(LPSTR) TIMEFMT_ENUMPROCA;
    alias BOOL function(LPWSTR) TIMEFMT_ENUMPROCW;
    alias BOOL function(GEOID) GEO_ENUMPROC;
}

enum NLS_FUNCTION {
    COMPARE_STRING = 0x0001
}

enum SYSGEOCLASS {
    GEOCLASS_NATION = 16,
    GEOCLASS_REGION = 14
}

enum SYSGEOTYPE {
    GEO_NATION            = 0x0001,
    GEO_LATITUDE          = 0x0002,
    GEO_LONGITUDE         = 0x0003,
    GEO_ISO2              = 0x0004,
    GEO_ISO3              = 0x0005,
    GEO_RFC1766           = 0x0006,
    GEO_LCID              = 0x0007,
    GEO_FRIENDLYNAME      = 0x0008,
    GEO_OFFICIALNAME      = 0x0009,
    GEO_TIMEZONES         = 0x000a,
    GEO_OFFICIALLANGUAGES = 0x000a
}

struct CPINFO {
    UINT                  MaxCharSize;
    BYTE[MAX_DEFAULTCHAR] DefaultChar;
    BYTE[MAX_LEADBYTES]   LeadByte;
}
alias CPINFO* LPCPINFO;

struct CPINFOEXA {
    UINT                  MaxCharSize;
    BYTE[MAX_DEFAULTCHAR] DefaultChar;
    BYTE[MAX_LEADBYTES]   LeadByte;
    WCHAR                 UnicodeDefaultChar = 0;
    UINT                  CodePage;
    CHAR[MAX_PATH]        CodePageName = 0;
}
alias CPINFOEXA* LPCPINFOEXA;

struct CPINFOEXW {
    UINT                  MaxCharSize;
    BYTE[MAX_DEFAULTCHAR] DefaultChar;
    BYTE[MAX_LEADBYTES]   LeadByte;
    WCHAR                 UnicodeDefaultChar = 0;
    UINT                  CodePage;
    WCHAR[MAX_PATH]       CodePageName = 0;
}
alias CPINFOEXW* LPCPINFOEXW;

struct CURRENCYFMTA {
    UINT  NumDigits;
    UINT  LeadingZero;
    UINT  Grouping;
    LPSTR lpDecimalSep;
    LPSTR lpThousandSep;
    UINT  NegativeOrder;
    UINT  PositiveOrder;
    LPSTR lpCurrencySymbol;
}
alias CURRENCYFMTA* LPCURRENCYFMTA;

struct CURRENCYFMTW {
    UINT   NumDigits;
    UINT   LeadingZero;
    UINT   Grouping;
    LPWSTR lpDecimalSep;
    LPWSTR lpThousandSep;
    UINT   NegativeOrder;
    UINT   PositiveOrder;
    LPWSTR lpCurrencySymbol;
}
alias CURRENCYFMTW* LPCURRENCYFMTW;

struct NLSVERSIONINFO {
    DWORD dwNLSVersionInfoSize;
    DWORD dwNLSVersion;
    DWORD dwDefinedVersion;
}
alias NLSVERSIONINFO* LPNLSVERSIONINFO;

struct NUMBERFMTA {
    UINT  NumDigits;
    UINT  LeadingZero;
    UINT  Grouping;
    LPSTR lpDecimalSep;
    LPSTR lpThousandSep;
    UINT  NegativeOrder;
}
alias NUMBERFMTA* LPNUMBERFMTA;

struct NUMBERFMTW {
    UINT   NumDigits;
    UINT   LeadingZero;
    UINT   Grouping;
    LPWSTR lpDecimalSep;
    LPWSTR lpThousandSep;
    UINT   NegativeOrder;
}
alias NUMBERFMTW* LPNUMBERFMTW;

extern (Windows) nothrow @nogc {
    int CompareStringA(LCID, DWORD, LPCSTR, int, LPCSTR, int);
    int CompareStringW(LCID, DWORD, LPCWSTR, int, LPCWSTR, int);
    LCID ConvertDefaultLocale(LCID);
    BOOL EnumCalendarInfoA(CALINFO_ENUMPROCA, LCID, CALID, CALTYPE);
    BOOL EnumCalendarInfoW(CALINFO_ENUMPROCW, LCID, CALID, CALTYPE);
    BOOL EnumDateFormatsA(DATEFMT_ENUMPROCA, LCID, DWORD);
    BOOL EnumDateFormatsW(DATEFMT_ENUMPROCW, LCID, DWORD);
    BOOL EnumSystemCodePagesA(CODEPAGE_ENUMPROCA, DWORD);
    BOOL EnumSystemCodePagesW(CODEPAGE_ENUMPROCW, DWORD);
    BOOL EnumSystemGeoID(GEOCLASS, GEOID, GEO_ENUMPROC);
    BOOL EnumSystemLocalesA(LOCALE_ENUMPROCA, DWORD);
    BOOL EnumSystemLocalesW(LOCALE_ENUMPROCW, DWORD);
    BOOL EnumTimeFormatsA(TIMEFMT_ENUMPROCA, LCID, DWORD);
    BOOL EnumTimeFormatsW(TIMEFMT_ENUMPROCW, LCID, DWORD);
    int FoldStringA(DWORD, LPCSTR, int, LPSTR, int);
    int FoldStringW(DWORD, LPCWSTR, int, LPWSTR, int);
    UINT GetACP();
    int GetCalendarInfoA(LCID, CALID, CALTYPE, LPSTR, int, LPDWORD);
    int GetCalendarInfoW(LCID, CALID, CALTYPE, LPWSTR, int, LPDWORD);
    BOOL GetCPInfo(UINT, LPCPINFO);
    BOOL GetCPInfoExA(UINT, DWORD, LPCPINFOEXA);
    BOOL GetCPInfoExW(UINT, DWORD, LPCPINFOEXW);
    int GetCurrencyFormatA(LCID, DWORD, LPCSTR,  const(CURRENCYFMTA)*, LPSTR, int);
    int GetCurrencyFormatW(LCID, DWORD, LPCWSTR,  const(CURRENCYFMTW)*, LPWSTR,
      int);
    int GetDateFormatA(LCID, DWORD,  const(SYSTEMTIME)*, LPCSTR, LPSTR, int);
    int GetDateFormatW(LCID, DWORD,  const(SYSTEMTIME)*, LPCWSTR, LPWSTR, int);
    int GetGeoInfoA(GEOID, GEOTYPE, LPSTR, int, LANGID);
    int GetGeoInfoW(GEOID, GEOTYPE, LPWSTR, int, LANGID);
    int GetLocaleInfoA(LCID, LCTYPE, LPSTR, int);
    int GetLocaleInfoW(LCID, LCTYPE, LPWSTR, int);
    BOOL GetNLSVersion(NLS_FUNCTION, LCID, LPNLSVERSIONINFO);
    int GetNumberFormatA(LCID, DWORD, LPCSTR,  const(NUMBERFMTA)*, LPSTR, int);
    int GetNumberFormatW(LCID, DWORD, LPCWSTR,  const(NUMBERFMTW)*, LPWSTR, int);
    UINT GetOEMCP();
    BOOL GetStringTypeA(LCID, DWORD, LPCSTR, int, LPWORD);
    BOOL GetStringTypeW(DWORD, LPCWSTR, int, LPWORD);
    BOOL GetStringTypeExA(LCID, DWORD, LPCSTR, int, LPWORD);
    BOOL GetStringTypeExW(LCID, DWORD, LPCWSTR, int, LPWORD);
    LANGID GetSystemDefaultLangID();
    LCID GetSystemDefaultLCID();
    LCID GetThreadLocale();
    int GetTimeFormatA(LCID, DWORD,  const(SYSTEMTIME)*, LPCSTR, LPSTR, int);
    int GetTimeFormatW(LCID, DWORD,  const(SYSTEMTIME)*, LPCWSTR, LPWSTR, int);
    LANGID GetUserDefaultLangID();
    LCID GetUserDefaultLCID();
    GEOID GetUserGeoID(GEOCLASS);
    BOOL IsDBCSLeadByte(BYTE);
    BOOL IsDBCSLeadByteEx(UINT, BYTE);
    BOOL IsNLSDefinedString(NLS_FUNCTION, DWORD, LPNLSVERSIONINFO, LPCWSTR,
      int);
    BOOL IsValidCodePage(UINT);
    BOOL IsValidLocale(LCID, DWORD);
    int LCMapStringA(LCID, DWORD, LPCSTR, int, LPSTR, int);
    int LCMapStringW(LCID, DWORD, LPCWSTR, int, LPWSTR, int);
    int MultiByteToWideChar(UINT, DWORD, LPCSTR, int, LPWSTR, int);
    int SetCalendarInfoA(LCID, CALID, CALTYPE, LPCSTR);
    int SetCalendarInfoW(LCID, CALID, CALTYPE, LPCWSTR);
    BOOL SetLocaleInfoA(LCID, LCTYPE, LPCSTR);
    BOOL SetLocaleInfoW(LCID, LCTYPE, LPCWSTR);
    BOOL SetThreadLocale(LCID);
    BOOL SetUserGeoID(GEOID);
    int WideCharToMultiByte(UINT, DWORD, LPCWSTR, int, LPSTR, int, LPCSTR,
      LPBOOL);

    static if (_WIN32_WINNT >= 0x410) {
        BOOL EnumCalendarInfoExA(CALINFO_ENUMPROCEXA, LCID, CALID, CALTYPE);
        BOOL EnumCalendarInfoExW(CALINFO_ENUMPROCEXW, LCID, CALID, CALTYPE);
        BOOL EnumDateFormatsExA(DATEFMT_ENUMPROCEXA, LCID, DWORD);
        BOOL EnumDateFormatsExW(DATEFMT_ENUMPROCEXW, LCID, DWORD);
        BOOL IsValidLanguageGroup(LGRPID, DWORD);
    }

    static if (_WIN32_WINNT >= 0x500) {
        LANGID GetSystemDefaultUILanguage();
        LANGID GetUserDefaultUILanguage();

        BOOL EnumSystemLanguageGroupsA(LANGUAGEGROUP_ENUMPROCA, DWORD,
          LONG_PTR);
        BOOL EnumSystemLanguageGroupsW(LANGUAGEGROUP_ENUMPROCW, DWORD,
          LONG_PTR);
        BOOL EnumLanguageGroupLocalesA(LANGGROUPLOCALE_ENUMPROCA, LGRPID,
          DWORD, LONG_PTR);
        BOOL EnumLanguageGroupLocalesW(LANGGROUPLOCALE_ENUMPROCW, LGRPID,
          DWORD, LONG_PTR);
        BOOL EnumUILanguagesA(UILANGUAGE_ENUMPROCA, DWORD, LONG_PTR);
        BOOL EnumUILanguagesW(UILANGUAGE_ENUMPROCW, DWORD, LONG_PTR);
    }
}

version (Unicode) {
    alias CALINFO_ENUMPROCW CALINFO_ENUMPROC;
    alias CALINFO_ENUMPROCEXW CALINFO_ENUMPROCEX;
    alias LOCALE_ENUMPROCW LOCALE_ENUMPROC;
    alias CODEPAGE_ENUMPROCW CODEPAGE_ENUMPROC;
    alias DATEFMT_ENUMPROCW DATEFMT_ENUMPROC;
    alias DATEFMT_ENUMPROCEXW DATEFMT_ENUMPROCEX;
    alias TIMEFMT_ENUMPROCW TIMEFMT_ENUMPROC;
    alias LANGUAGEGROUP_ENUMPROCW LANGUAGEGROUP_ENUMPROC;
    alias LANGGROUPLOCALE_ENUMPROCW LANGGROUPLOCALE_ENUMPROC;
    alias UILANGUAGE_ENUMPROCW UILANGUAGE_ENUMPROC;
    alias CPINFOEXW CPINFOEX;
    alias LPCPINFOEXW LPCPINFOEX;
    alias CURRENCYFMTW CURRENCYFMT;
    alias LPCURRENCYFMTW LPCURRENCYFMT;
    alias NUMBERFMTW NUMBERFMT;
    alias LPNUMBERFMTW LPNUMBERFMT;
    alias CompareStringW CompareString;
    alias EnumCalendarInfoW EnumCalendarInfo;
    alias EnumSystemCodePagesW EnumSystemCodePages;
    alias EnumSystemLocalesW EnumSystemLocales;
    alias EnumTimeFormatsW EnumTimeFormats;
    alias FoldStringW FoldString;
    alias GetCalendarInfoW GetCalendarInfo;
    alias GetCPInfoExW GetCPInfoEx;
    alias GetCurrencyFormatW GetCurrencyFormat;
    alias GetDateFormatW GetDateFormat;
    alias GetGeoInfoW GetGeoInfo;
    alias GetLocaleInfoW GetLocaleInfo;
    alias GetNumberFormatW GetNumberFormat;
    alias GetStringTypeExW GetStringTypeEx;
    alias GetTimeFormatW GetTimeFormat;
    alias LCMapStringW LCMapString;
    alias SetCalendarInfoW SetCalendarInfo;
    alias SetLocaleInfoW SetLocaleInfo;

    static if (_WIN32_WINNT >= 0x410) {
        alias EnumCalendarInfoExW EnumCalendarInfoEx;
        alias EnumDateFormatsExW EnumDateFormatsEx;
    }

    static if (_WIN32_WINNT >= 0x500) {
        alias EnumSystemLanguageGroupsW EnumSystemLanguageGroups;
        alias EnumLanguageGroupLocalesW EnumLanguageGroupLocales;
        alias EnumUILanguagesW EnumUILanguages;
    }

} else {
    alias CALINFO_ENUMPROCA CALINFO_ENUMPROC;
    alias CALINFO_ENUMPROCEXA CALINFO_ENUMPROCEX;
    alias LOCALE_ENUMPROCA LOCALE_ENUMPROC;
    alias CODEPAGE_ENUMPROCA CODEPAGE_ENUMPROC;
    alias DATEFMT_ENUMPROCA DATEFMT_ENUMPROC;
    alias DATEFMT_ENUMPROCEXA DATEFMT_ENUMPROCEX;
    alias TIMEFMT_ENUMPROCA TIMEFMT_ENUMPROC;
    alias LANGUAGEGROUP_ENUMPROCA LANGUAGEGROUP_ENUMPROC;
    alias LANGGROUPLOCALE_ENUMPROCA LANGGROUPLOCALE_ENUMPROC;
    alias UILANGUAGE_ENUMPROCA UILANGUAGE_ENUMPROC;
    alias CPINFOEXA CPINFOEX;
    alias LPCPINFOEXA LPCPINFOEX;
    alias CURRENCYFMTA CURRENCYFMT;
    alias LPCURRENCYFMTA LPCURRENCYFMT;
    alias NUMBERFMTA NUMBERFMT;
    alias LPNUMBERFMTA LPNUMBERFMT;
    alias CompareStringA CompareString;
    alias EnumCalendarInfoA EnumCalendarInfo;
    alias EnumSystemCodePagesA EnumSystemCodePages;
    alias EnumSystemLocalesA EnumSystemLocales;
    alias EnumTimeFormatsA EnumTimeFormats;
    alias FoldStringA FoldString;
    alias GetCalendarInfoA GetCalendarInfo;
    alias GetCPInfoExA GetCPInfoEx;
    alias GetCurrencyFormatA GetCurrencyFormat;
    alias GetDateFormatA GetDateFormat;
    alias GetGeoInfoA GetGeoInfo;
    alias GetLocaleInfoA GetLocaleInfo;
    alias GetNumberFormatA GetNumberFormat;
    alias GetStringTypeExA GetStringTypeEx;
    alias GetTimeFormatA GetTimeFormat;
    alias LCMapStringA LCMapString;
    alias SetCalendarInfoA SetCalendarInfo;
    alias SetLocaleInfoA SetLocaleInfo;

    static if (_WIN32_WINNT >= 0x410) {
        alias EnumCalendarInfoExA EnumCalendarInfoEx;
        alias EnumDateFormatsExA EnumDateFormatsEx;
    }

    static if (_WIN32_WINNT >= 0x500) {
        alias EnumSystemLanguageGroupsA EnumSystemLanguageGroups;
        alias EnumLanguageGroupLocalesA EnumLanguageGroupLocales;
        alias EnumUILanguagesA EnumUILanguages;
    }
}
