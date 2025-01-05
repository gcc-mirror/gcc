/**
 * D header file for C99.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_wctype.h.html, _wctype.h)
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/stdc/_wctype.d)
 * Standards: ISO/IEC 9899:1999 (E)
 */

module core.stdc.wctype;

import core.stdc.config;
public  import core.stdc.wchar_; // for wint_t, WEOF

extern (C):
@trusted: // Only a couple of functions below operate on unsafe C strings.
nothrow:
@nogc:

version (CRuntime_Glibc)
{
    ///
    alias wctype_t = c_ulong;
    ///
    alias wctrans_t = const(int)*;
}
else version (CRuntime_Musl)
{
    ///
    alias wctype_t = c_ulong;
    ///
    alias wctrans_t = const(int)*;
}
else version (FreeBSD)
{
    ///
    alias wctype_t = c_ulong;
    ///
    alias wctrans_t = int;
}
else version (CRuntime_Bionic)
{
    ///
    alias wctype_t = c_long;
    ///
    alias wctrans_t = const(void)*;
}
else
{
    ///
    alias wchar_t wctrans_t;
    ///
    alias wchar_t wctype_t;
}

///
pure int iswalnum(wint_t wc);
///
pure int iswalpha(wint_t wc);
///
pure int iswblank(wint_t wc);
///
pure int iswcntrl(wint_t wc);
///
pure int iswdigit(wint_t wc);
///
pure int iswgraph(wint_t wc);
///
pure int iswlower(wint_t wc);
///
pure int iswprint(wint_t wc);
///
pure int iswpunct(wint_t wc);
///
pure int iswspace(wint_t wc);
///
pure int iswupper(wint_t wc);
///
pure int iswxdigit(wint_t wc);

///
int       iswctype(wint_t wc, wctype_t desc);
///
@system wctype_t  wctype(const scope char* property);
///
pure wint_t    towlower(wint_t wc);
///
pure wint_t    towupper(wint_t wc);
///
wint_t    towctrans(wint_t wc, wctrans_t desc);
///
@system wctrans_t wctrans(const scope char* property);

unittest
{
    assert(iswalpha('A'));
    assert(!iswalpha('0'));
    wctype_t alpha = wctype("alpha");
    assert(alpha);
    wctrans_t tolower = wctrans("tolower");
    assert(tolower);
    assert(iswctype('A', alpha));
    assert(!iswctype('0', alpha));
    assert(towctrans('A', tolower) == 'a');
    assert(towctrans('0', tolower) == '0');
}
