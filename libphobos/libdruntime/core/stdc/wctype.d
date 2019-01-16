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

public  import core.stdc.wchar_; // for wint_t, WEOF

extern (C):
@trusted: // Only a couple of functions below operate on unsafe C strings.
nothrow:
@nogc:

///
alias wchar_t wctrans_t;
///
alias wchar_t wctype_t;

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
@system wctype_t  wctype(in char* property);
///
pure wint_t    towlower(wint_t wc);
///
pure wint_t    towupper(wint_t wc);
///
wint_t    towctrans(wint_t wc, wctrans_t desc);
///
@system wctrans_t wctrans(in char* property);
