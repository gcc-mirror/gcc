/**
 * D header file for C99.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_wchar.h.html, _wchar.h)
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/stdc/_wchar_.d)
 * Standards: ISO/IEC 9899:1999 (E)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module core.stdc.wchar_;

private import core.stdc.config;
private import core.stdc.stdarg; // for va_list
private import core.stdc.stdio;  // for FILE, not exposed per spec
public import core.stdc.stddef;  // for wchar_t
public import core.stdc.time;    // for tm
public import core.stdc.stdint;  // for WCHAR_MIN, WCHAR_MAX

extern (C):
@system:
nothrow:
@nogc:

version (CRuntime_Glibc)
{
    ///
    struct mbstate_t
    {
        int __count;
        union ___value
        {
            wint_t __wch = 0;
            char[4] __wchb;
        }
        ___value __value;
    }
}
else version (FreeBSD)
{
    ///
    union __mbstate_t // <sys/_types.h>
    {
        char[128]   _mbstate8 = 0;
        long        _mbstateL;
    }

    ///
    alias mbstate_t = __mbstate_t;
}
else version (NetBSD)
{
    ///
    union __mbstate_t
    {
        int64_t   __mbstateL;
        char[128] __mbstate8;
    }

    ///
    alias mbstate_t = __mbstate_t;
}
else version (OpenBSD)
{
    ///
    union __mbstate_t
    {
        char[128] __mbstate8 = 0;
        int64_t   __mbstateL;
    }

    ///
    alias mbstate_t = __mbstate_t;
}
else version (DragonFlyBSD)
{
    ///
    union __mbstate_t                   // <sys/stdint.h>
    {
        char[128]   _mbstate8 = 0;
        long        _mbstateL;
    }

    ///
    alias mbstate_t = __mbstate_t;
}
else version (Solaris)
{
    ///
    struct __mbstate_t
    {
        version (D_LP64)
        {
            long[4] __filler;
        }
        else
        {
            int[6] __filler;
        }
    }

    ///
    alias mbstate_t = __mbstate_t;
}
else version (CRuntime_UClibc)
{
    ///
    struct mbstate_t
    {
        wchar_t __mask = 0;
        wchar_t __wc = 0;
    }
}
else
{
    ///
    alias int mbstate_t;
}

///
alias wchar_t wint_t;

///
enum wchar_t WEOF = 0xFFFF;

///
int fwprintf(FILE* stream, in wchar_t* format, ...);
///
int fwscanf(FILE* stream, in wchar_t* format, ...);
int swscanf(in wchar_t* s, in wchar_t* format, ...);
///
int vfwprintf(FILE* stream, in wchar_t* format, va_list arg);
///
int vfwscanf(FILE* stream, in wchar_t* format, va_list arg);
int vswscanf(in wchar_t* s, in wchar_t* format, va_list arg);
///
int vwprintf(in wchar_t* format, va_list arg);
///
int vwscanf(in wchar_t* format, va_list arg);
///
int wprintf(in wchar_t* format, ...);
///
int wscanf(in wchar_t* format, ...);

/*
 * Windows has 2 versions of swprintf and vswprintf.  MinGW defaults to the
 * Microsoft signature.  Alias to match DMD/ANSI signature.
 */
version (MinGW)
{
    ///
    int _snwprintf(wchar_t* s, size_t n, in wchar_t* format, ...);
    alias _snwprintf swprintf;
    ///
    int _vsnwprintf(wchar_t* s, size_t n, in wchar_t* format, va_list arg);
    alias _vsnwprintf vswprintf;	
}
else
{
    ///
    int swprintf(wchar_t* s, size_t n, in wchar_t* format, ...);
    ///
    int vswprintf(wchar_t* s, size_t n, in wchar_t* format, va_list arg);
}

// No unsafe pointer manipulation.
@trusted
{
    ///
    wint_t fgetwc(FILE* stream);
    ///
    wint_t fputwc(wchar_t c, FILE* stream);
}

///
wchar_t* fgetws(wchar_t* s, int n, FILE* stream);
///
int      fputws(in wchar_t* s, FILE* stream);

// No unsafe pointer manipulation.
extern (D) @trusted
{
    ///
    wint_t getwchar()                     { return fgetwc(stdin);     }
    ///
    wint_t putwchar(wchar_t c)            { return fputwc(c,stdout);  }
    ///
    wint_t getwc(FILE* stream)            { return fgetwc(stream);    }
    ///
    wint_t putwc(wchar_t c, FILE* stream) { return fputwc(c, stream); }
}

// No unsafe pointer manipulation.
@trusted
{
    ///
    wint_t ungetwc(wint_t c, FILE* stream);
    ///
    version (CRuntime_Microsoft)
    {
        // MSVC defines this as an inline function.
        int fwide(FILE* stream, int mode) { return mode; }
    }
    else
    {
        int    fwide(FILE* stream, int mode);
    }
}

///
double  wcstod(in wchar_t* nptr, wchar_t** endptr);
///
float   wcstof(in wchar_t* nptr, wchar_t** endptr);
///
real    wcstold(in wchar_t* nptr, wchar_t** endptr);
///
c_long  wcstol(in wchar_t* nptr, wchar_t** endptr, int base);
///
long    wcstoll(in wchar_t* nptr, wchar_t** endptr, int base);
///
c_ulong wcstoul(in wchar_t* nptr, wchar_t** endptr, int base);
///
ulong   wcstoull(in wchar_t* nptr, wchar_t** endptr, int base);

///
pure wchar_t* wcscpy(return wchar_t* s1, scope const wchar_t* s2);
///
pure wchar_t* wcsncpy(return wchar_t* s1, scope const wchar_t* s2, size_t n);
///
pure wchar_t* wcscat(return wchar_t* s1, scope const wchar_t* s2);
///
pure wchar_t* wcsncat(return wchar_t* s1, scope const wchar_t* s2, size_t n);
///
pure int wcscmp(scope const wchar_t* s1, scope const wchar_t* s2);
///
int      wcscoll(scope const wchar_t* s1, scope const wchar_t* s2);
///
pure int wcsncmp(scope const wchar_t* s1, scope const wchar_t* s2, size_t n);
///
size_t   wcsxfrm(scope wchar_t* s1, scope const wchar_t* s2, size_t n);
///
pure inout(wchar_t)* wcschr(return inout(wchar_t)* s, wchar_t c);
///
pure size_t wcscspn(scope const wchar_t* s1, scope const wchar_t* s2);
///
pure inout(wchar_t)* wcspbrk(return inout(wchar_t)* s1, scope const wchar_t* s2);
///
pure inout(wchar_t)* wcsrchr(return inout(wchar_t)* s, wchar_t c);
///
pure size_t wcsspn(scope const wchar_t* s1, scope const wchar_t* s2);
///
pure inout(wchar_t)* wcsstr(return inout(wchar_t)* s1, scope const wchar_t* s2);
///
wchar_t* wcstok(return wchar_t* s1, scope const wchar_t* s2, wchar_t** ptr);
///
pure size_t wcslen(scope const wchar_t* s);

///
pure wchar_t* wmemchr(return const wchar_t* s, wchar_t c, size_t n);
///
pure int      wmemcmp(scope const wchar_t* s1, scope const wchar_t* s2, size_t n);
///
pure wchar_t* wmemcpy(return wchar_t* s1, scope const wchar_t* s2, size_t n);
///
pure wchar_t* wmemmove(return wchar_t* s1, scope const wchar_t* s2, size_t n);
///
pure wchar_t* wmemset(return wchar_t* s, wchar_t c, size_t n);

///
size_t wcsftime(wchar_t* s, size_t maxsize, in wchar_t* format, in tm* timeptr);

version (Windows)
{
    ///
    wchar_t* _wasctime(tm*);      // non-standard
    ///
    wchar_t* _wctime(time_t*);    // non-standard
    ///
    wchar_t* _wstrdate(wchar_t*); // non-standard
    ///
    wchar_t* _wstrtime(wchar_t*); // non-standard
}

// No unsafe pointer manipulation.
@trusted
{
    ///
    wint_t btowc(int c);
    ///
    int    wctob(wint_t c);
}

///
int    mbsinit(in mbstate_t* ps);
///
size_t mbrlen(in char* s, size_t n, mbstate_t* ps);
///
size_t mbrtowc(wchar_t* pwc, in char* s, size_t n, mbstate_t* ps);
///
size_t wcrtomb(char* s, wchar_t wc, mbstate_t* ps);
///
size_t mbsrtowcs(wchar_t* dst, in char** src, size_t len, mbstate_t* ps);
///
size_t wcsrtombs(char* dst, in wchar_t** src, size_t len, mbstate_t* ps);
