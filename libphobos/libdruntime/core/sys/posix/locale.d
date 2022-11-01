/**
 * D header file for POSIX's <locale.h>.
 *
 * See_Also:  https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/locale.h.html
 * Copyright: D Language Foundation, 2019
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Mathias 'Geod24' Lang
 * Standards: The Open Group Base Specifications Issue 7, 2018 edition
 * Source:    $(DRUNTIMESRC core/sys/posix/_locale.d)
 */
module core.sys.posix.locale;

version (Posix):
extern(C):
nothrow:
@nogc:

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin)
    version = DarwinBSDLocale;
version (FreeBSD)
    version = DarwinBSDLocale;
version (NetBSD)
    version = DarwinBSDLocale;
version (DragonFlyBSD)
    version = DarwinBSDLocale;

version (CRuntime_Glibc)
    version = GNULinuxLocale;
version (CRuntime_Bionic)
    version = GNULinuxLocale;
version (CRuntime_UClibc)
    version = GNULinuxLocale;

version (DarwinBSDLocale)
{
    ///
    struct lconv
    {
        char*   decimal_point;
        char*   thousands_sep;
        char*   grouping;
        char*   int_curr_symbol;
        char*   currency_symbol;
        char*   mon_decimal_point;
        char*   mon_thousands_sep;
        char*   mon_grouping;
        char*   positive_sign;
        char*   negative_sign;
        char    int_frac_digits;
        char    frac_digits;
        char    p_cs_precedes;
        char    p_sep_by_space;
        char    n_cs_precedes;
        char    n_sep_by_space;
        char    p_sign_posn;
        char    n_sign_posn;
        char    int_p_cs_precedes;
        char    int_n_cs_precedes;
        char    int_p_sep_by_space;
        char    int_n_sep_by_space;
        char    int_p_sign_posn;
        char    int_n_sign_posn;
    }

    ///
    enum
    {
        LC_ALL      = 0,
        LC_COLLATE  = 1,
        LC_CTYPE    = 2,
        LC_MESSAGES = 6,
        LC_MONETARY = 3,
        LC_NUMERIC  = 4,
        LC_TIME     = 5,
    }

    private struct _xlocale;

    ///
    alias locale_t = _xlocale*;

    version (NetBSD)
        enum LC_ALL_MASK = (cast(int)~0);
    else
        enum LC_ALL_MASK = (
            LC_COLLATE_MASK | LC_CTYPE_MASK | LC_MESSAGES_MASK |
            LC_MONETARY_MASK | LC_NUMERIC_MASK | LC_TIME_MASK);


    ///
    enum
    {
        LC_COLLATE_MASK  = (1 << 0),
        LC_CTYPE_MASK    = (1 << 1),
        LC_MESSAGES_MASK = (1 << 2),
        LC_MONETARY_MASK = (1 << 3),
        LC_NUMERIC_MASK  = (1 << 4),
        LC_TIME_MASK     = (1 << 5),
    }

    ///
    enum LC_GLOBAL_LOCALE = (cast(locale_t)-1);

    /// Duplicate existing locale
    locale_t duplocale(locale_t locale);
    /// Free an allocated locale
    void     freelocale(locale_t locale);
    /// Natural language formatting for C
    lconv*   localeconv();
    /// Create a new locale
    locale_t newlocale(int mask, const char* locale, locale_t base);
    /// Set the C library's notion of natural language formatting style
    char*    setlocale(int category, const char* locale);
    /// Set the per-thread locale
    locale_t uselocale (locale_t locale);
}
else version (GNULinuxLocale)
{
    ///
    struct lconv
    {
        char*   decimal_point;
        char*   thousands_sep;
        char*   grouping;
        char*   int_curr_symbol;
        char*   currency_symbol;
        char*   mon_decimal_point;
        char*   mon_thousands_sep;
        char*   mon_grouping;
        char*   positive_sign;
        char*   negative_sign;
        char    int_frac_digits;
        char    frac_digits;
        char    p_cs_precedes;
        char    p_sep_by_space;
        char    n_cs_precedes;
        char    n_sep_by_space;
        char    p_sign_posn;
        char    n_sign_posn;
        char    int_p_cs_precedes;
        char    int_p_sep_by_space;
        char    int_n_cs_precedes;
        char    int_n_sep_by_space;
        char    int_p_sign_posn;
        char    int_n_sign_posn;
    }

    ///
    enum
    {
        LC_ALL      = 6,
        LC_COLLATE  = 3,
        LC_CTYPE    = 0,
        LC_MESSAGES = 5,
        LC_MONETARY = 4,
        LC_NUMERIC  = 1,
        LC_TIME     = 2,

        // Linux-specific
        LC_PAPER          =  7,
        LC_NAME           =  8,
        LC_ADDRESS        =  9,
        LC_TELEPHONE      = 10,
        LC_MEASUREMENT    = 11,
        LC_IDENTIFICATION = 12,
    }

    ///
    enum
    {
        LC_ALL_MASK = (LC_CTYPE_MASK | LC_NUMERIC_MASK | LC_TIME_MASK |
                       LC_COLLATE_MASK | LC_MONETARY_MASK | LC_MESSAGES_MASK |
                       LC_PAPER_MASK | LC_NAME_MASK | LC_ADDRESS_MASK |
                       LC_TELEPHONE_MASK | LC_MEASUREMENT_MASK |
                       LC_IDENTIFICATION_MASK),

        LC_COLLATE_MASK  = (1 << LC_COLLATE),
        LC_CTYPE_MASK    = (1 << LC_CTYPE),
        LC_MESSAGES_MASK = (1 << LC_MESSAGES),
        LC_MONETARY_MASK = (1 << LC_MONETARY),
        LC_NUMERIC_MASK  = (1 << LC_NUMERIC),
        LC_TIME_MASK     = (1 << LC_TIME),

        // Linux specific
        LC_PAPER_MASK          = (1 << LC_PAPER),
        LC_NAME_MASK           = (1 << LC_NAME),
        LC_ADDRESS_MASK        = (1 << LC_ADDRESS),
        LC_TELEPHONE_MASK      = (1 << LC_TELEPHONE),
        LC_MEASUREMENT_MASK    = (1 << LC_MEASUREMENT),
        LC_IDENTIFICATION_MASK = (1 << LC_IDENTIFICATION),
    }

    private struct __locale_struct;

    ///
    alias locale_t = __locale_struct*;

    ///
    enum LC_GLOBAL_LOCALE = (cast(locale_t)-1);

    /// Duplicate existing locale
    locale_t duplocale(locale_t locale);
    /// Free an allocated locale
    void     freelocale(locale_t locale);
    /// Natural language formatting for C
    lconv*   localeconv();
    /// Create a new locale
    locale_t newlocale(int mask, const char* locale, locale_t base);
    /// Set the C library's notion of natural language formatting style
    char*    setlocale(int category, const char* locale);
    /// Set the per-thread locale
    locale_t uselocale (locale_t locale);
}
else version (CRuntime_Musl)
{
    ///
    struct lconv
    {
        char*   decimal_point;
        char*   thousands_sep;
        char*   grouping;
        char*   int_curr_symbol;
        char*   currency_symbol;
        char*   mon_decimal_point;
        char*   mon_thousands_sep;
        char*   mon_grouping;
        char*   positive_sign;
        char*   negative_sign;
        char    int_frac_digits;
        char    frac_digits;
        char    p_cs_precedes;
        char    p_sep_by_space;
        char    n_cs_precedes;
        char    n_sep_by_space;
        char    p_sign_posn;
        char    n_sign_posn;
        char    int_p_cs_precedes;
        char    int_p_sep_by_space;
        char    int_n_cs_precedes;
        char    int_n_sep_by_space;
        char    int_p_sign_posn;
        char    int_n_sign_posn;
    }

    ///
    enum
    {
        LC_CTYPE    = 0,
        LC_NUMERIC  = 1,
        LC_TIME     = 2,
        LC_COLLATE  = 3,
        LC_MONETARY = 4,
        LC_MESSAGES = 5,
        LC_ALL      = 6,
    }

    ///
    enum
    {
        LC_CTYPE_MASK    = (1 << LC_CTYPE),
        LC_NUMERIC_MASK  = (1 << LC_NUMERIC),
        LC_TIME_MASK     = (1 << LC_TIME),
        LC_COLLATE_MASK  = (1 << LC_COLLATE),
        LC_MONETARY_MASK = (1 << LC_MONETARY),
        LC_MESSAGES_MASK = (1 << LC_MESSAGES),
        LC_ALL_MASK      = 0x7fffffff,
    }

    private struct __locale_struct;

    ///
    alias locale_t = __locale_struct*;

    ///
    enum LC_GLOBAL_LOCALE = (cast(locale_t)-1);

    /// Duplicate existing locale
    locale_t duplocale(locale_t locale);
    /// Free an allocated locale
    void     freelocale(locale_t locale);
    /// Natural language formatting for C
    lconv*   localeconv();
    /// Create a new locale
    locale_t newlocale(int mask, const char* locale, locale_t base);
    /// Set the C library's notion of natural language formatting style
    char*    setlocale(int category, const char* locale);
    /// Set the per-thread locale
    locale_t uselocale (locale_t locale);
}
else version (OpenBSD)
{
    ///
    struct lconv
    {
        char*   decimal_point;
        char*   thousands_sep;
        char*   grouping;
        char*   int_curr_symbol;
        char*   currency_symbol;
        char*   mon_decimal_point;
        char*   mon_thousands_sep;
        char*   mon_grouping;
        char*   positive_sign;
        char*   negative_sign;
        char    int_frac_digits;
        char    frac_digits;
        char    p_cs_precedes;
        char    p_sep_by_space;
        char    n_cs_precedes;
        char    n_sep_by_space;
        char    p_sign_posn;
        char    n_sign_posn;
        char    int_p_cs_precedes;
        char    int_n_cs_precedes;
        char    int_p_sep_by_space;
        char    int_n_sep_by_space;
        char    int_p_sign_posn;
        char    int_n_sign_posn;
    }

    ///
    enum
    {
        LC_ALL      = 0,
        LC_COLLATE  = 1,
        LC_CTYPE    = 2,
        LC_MONETARY = 3,
        LC_NUMERIC  = 4,
        LC_TIME     = 5,
        LC_MESSAGES = 6,
    }
    private enum _LC_LAST = 7;

    ///
    enum
    {
        LC_COLLATE_MASK  = (1 << LC_COLLATE),
        LC_CTYPE_MASK    = (1 << LC_CTYPE),
        LC_MONETARY_MASK = (1 << LC_MONETARY),
        LC_NUMERIC_MASK  = (1 << LC_NUMERIC),
        LC_TIME_MASK     = (1 << LC_TIME),
        LC_MESSAGES_MASK = (1 << LC_MESSAGES),
        LC_ALL_MASK      = ((1 << _LC_LAST) - 2),
    }

    ///
    alias locale_t = void*;

    ///
    enum LC_GLOBAL_LOCALE = (cast(locale_t)-1);

    /// Duplicate existing locale
    locale_t duplocale(locale_t locale);
    /// Free an allocated locale
    void     freelocale(locale_t locale);
    /// Natural language formatting for C
    lconv*   localeconv();
    /// Create a new locale
    locale_t newlocale(int mask, const char* locale, locale_t base);
    /// Set the C library's notion of natural language formatting style
    char*    setlocale(int category, const char* locale);
    /// Set the per-thread locale
    locale_t uselocale (locale_t locale);
}
else version (Solaris)
{
    ///
    struct lconv
    {
        char*   decimal_point;
        char*   thousands_sep;
        char*   grouping;
        char*   int_curr_symbol;
        char*   currency_symbol;
        char*   mon_decimal_point;
        char*   mon_thousands_sep;
        char*   mon_grouping;
        char*   positive_sign;
        char*   negative_sign;
        char    int_frac_digits;
        char    frac_digits;
        char    p_cs_precedes;
        char    p_sep_by_space;
        char    n_cs_precedes;
        char    n_sep_by_space;
        char    p_sign_posn;
        char    n_sign_posn;
        char    int_p_cs_precedes;
        char    int_n_cs_precedes;
        char    int_p_sep_by_space;
        char    int_n_sep_by_space;
        char    int_p_sign_posn;
        char    int_n_sign_posn;
    }

    ///
    enum
    {
        LC_CTYPE    = 0,
        LC_NUMERIC  = 1,
        LC_TIME     = 2,
        LC_COLLATE  = 3,
        LC_MONETARY = 4,
        LC_MESSAGES = 5,
        LC_ALL      = 6,
    }

    ///
    enum
    {
        LC_CTYPE_MASK    = (1 << LC_CTYPE),
        LC_NUMERIC_MASK  = (1 << LC_NUMERIC),
        LC_TIME_MASK     = (1 << LC_TIME),
        LC_COLLATE_MASK  = (1 << LC_COLLATE),
        LC_MONETARY_MASK = (1 << LC_MONETARY),
        LC_MESSAGES_MASK = (1 << LC_MESSAGES),
        LC_ALL_MASK      = 0x3f,
    }

    private struct _LC_locale_t;

    ///
    alias locale_t = _LC_locale_t**;

    ///
    enum LC_GLOBAL_LOCALE = (cast(locale_t)-1);

    /// Duplicate existing locale
    locale_t duplocale(locale_t locale);
    /// Free an allocated locale
    void     freelocale(locale_t locale);
    /// Natural language formatting for C
    lconv*   localeconv();
    /// Create a new locale
    locale_t newlocale(int mask, const char* locale, locale_t base);
    /// Set the C library's notion of natural language formatting style
    char*    setlocale(int category, const char* locale);
    /// Set the per-thread locale
    locale_t uselocale (locale_t locale);
}
else
    static assert(false, "unimplemented platform");
