// -*- C++ -*- header wrapper.

// Copyright (C) 1997-1999 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

//
// ISO C++ 14882: 21

// XXX: this file still needs hackery for system version dependencies

#ifndef _CPP_CWCHAR
# define _CPP_CWCHAR 1

# include <bits/std_cstddef.h>  /* size_t, NULL */
# include <bits/std_cstdio.h>   /* FILE */
# include <bits/std_ctime.h>    /* struct tm */
# include <bits/std_cstring.h>  /* memset */

  namespace _C_Swamp {
    extern "C" {
#     define _IN_C_SWAMP_
#     include_next <wchar.h>
    }
    // NULL, size_t handled in <cstddef>

    // wchar_t
    typedef wint_t _CPP_wint_t_capture;
    typedef mbstate_t _CPP_mbstate_t_capture;
    const wint_t _CPP_WEOF_capture = (wint_t)(WEOF);

#if 0 /* XXX glibc-2.0 does not implement these. */
    inline wint_t _CPP_getwc_capture(FILE* __f)
      { return getwc(__f); }
    inline wint_t _CPP_getwchar_capture()
      { return getwchar(); }
    inline wint_t _CPP_putwc_capture(wint_t __c, FILE* __f)
      { return putwc(__c,__f); }
    inline wint_t _CPP_putwchar_capture(wint_t __c)
      { return putwchar(__c); }
#endif

    namespace _C_Shadow { }
  } // close namespace ::_C_Swamp::

// #  undef size_t  /* handled in <cstddef> */
# undef wchar_t
# undef wint_t
# undef mbstate_t
# undef WEOF
# define WEOF ::_C_Swamp::_CPP_WEOF_capture

// the following are not in glibc-2.0
# undef fgetwc
# undef fgetws
# undef fputwc
# undef fputws
# undef ungetwc
# undef getwc
# undef getwchar
# undef putwc
# undef putwchar
# undef wprintf
# undef wsprintf
# undef wvsprintf
# undef wfsprintf
# undef wscanf
# undef wsscanf
# undef wvsscanf
# undef wfscanf
// XXX etc.

# undef wcscpy
# undef wcscat
# undef wcscmp
# undef wcscoll
# undef wcsxfrm
# undef wcsdup
# undef wcschr
# undef wcscspn
# undef wcspbrk
# undef wcsstr
# undef wcstok
# undef wcslen
# undef wmemchr
# undef wmemcmp
# undef wmemcpy
# undef wmemmove
# undef wmemset
# undef btowc
# undef wctob
# undef mbsinit
# undef mbrtowc
# undef wcrtomb
# undef mbrlen
# undef mbsrtowcs
# undef wcsrtombs
#ifdef __USE_GNU
# undef mbsnrtowcs
# undef mbsnrtombs
# undef wcwidth
# undef wcswidth
# undef wcscmpy
#endif
# undef wcstod
# undef wcstol
# undef wcstoul
# undef wcsncat
# undef wcsncmp
# undef wcsncpy
# undef wcsrchr
# undef wcsspn

  // XXX a bunch more names are required under C89 Amendment 1, 
  // but they are not uniformly implemented.

  // XXX the following are not supposed to be defined in <wchar.h>, 
  //  but Sun does anyway.
# undef iswalpha
# undef iswupper
# undef iswlower
# undef iswdigit
# undef iswxdigit
# undef iswalnum
# undef iswspace
# undef iswpunct
# undef iswprint
# undef iswgraph
# undef iswcntrl
# undef iswctype
# undef towlower
# undef towupper
# undef wctype_t
# undef wctype
# undef wcspbrk
# undef wcswcs

  namespace _C_Swamp {
    namespace _C_Shadow {
      typedef ::_C_Swamp::_CPP_wint_t_capture wint_t; 
      // typedef ::_C_Swamp::_CPP_wctype_t_capture wctype_t;
    }
  }
  namespace std {

    // using ::_C_Swamp::wchar_t;
    using ::_C_Swamp::_C_Shadow::wint_t;
    // using ::_C_Swamp::WEOF;

   // XXX this might better be replaced with one unrelated to the C mbstate_t.

   struct mbstate_t { 
     _C_Swamp::_CPP_mbstate_t_capture _M_dum; 
     mbstate_t() { std::memset(&_M_dum,0,sizeof(_M_dum)); }
   };

#if 0 /* glibc-2.0 does not implement these. */
    inline wint_t fgetwc(FILE* __f)
      { return ::_C_Swamp::fgetwc(__f); }
    inline wchar_t* fgetws(wchar_t* __s, int __n, FILE* __f)
      { return ::_C_Swamp::fgetws(__s,__n,__f); }
    inline wint_t fputwc(wint_t __c, FILE* __f)
      { return ::_C_Swamp::fputwc(__c,__f); }
    inline int fputws(const wchar_t* __s, FILE* __f)
      { return ::_C_Swamp::fputws(__s,__f); }
    inline wint_t ungetwc(wint_t __c, FILE* __f)
      { return ::_C_Swamp::ungetwc(__c,__f); }

    inline wint_t getwc(FILE* __f)
      { return ::_C_Swamp::_CPP_getwc_capture(__f); }
    inline wint_t getwchar()
      { return ::_C_Swamp::_CPP_getwchar_capture(); }
    inline wint_t putwc(wint_t __c, FILE* __f)
      { return ::_C_Swamp::_CPP_putwc_capture(__c,__f); }
    inline wint_t putwchar(wint_t __c)
      { return ::_C_Swamp::_CPP_putwchar_capture(__c); }

    // similarly wprintf etc.
#endif

    using ::_C_Swamp::wcscpy;
    using ::_C_Swamp::wcscat;
    using ::_C_Swamp::wcscmp;
    using ::_C_Swamp::wcscoll;
    using ::_C_Swamp::wcsxfrm;
    using ::_C_Swamp::wcschr;
    using ::_C_Swamp::wcscspn;
    using ::_C_Swamp::wcspbrk;
    using ::_C_Swamp::wcstok;
    using ::_C_Swamp::wcslen;
#ifndef __sun
    using ::_C_Swamp::wcsdup;
    using ::_C_Swamp::wcsstr;
    using ::_C_Swamp::wmemchr;
    using ::_C_Swamp::wmemcmp;
    using ::_C_Swamp::wmemcpy;
    using ::_C_Swamp::wmemmove;
    using ::_C_Swamp::wmemset;
    using ::_C_Swamp::btowc;
    using ::_C_Swamp::wctob;
    using ::_C_Swamp::mbsinit;
    using ::_C_Swamp::mbrtowc;
    using ::_C_Swamp::wcrtomb;
    using ::_C_Swamp::mbrlen;
#endif
#ifdef __USE_GNU
    using ::_C_Swamp::mbsrtowcs;
    using ::_C_Swamp::wcsrtombs;
    using ::_C_Swamp::mbsnrtowcs;
    using ::_C_Swamp::mbsnrtombs;
    using ::_C_Swamp::wcscmpy
    using ::_C_Swamp::wcwidth;
    using ::_C_Swamp::wcswidth;
#endif
    using ::_C_Swamp::wcstod;
    using ::_C_Swamp::wcstol;
    using ::_C_Swamp::wcstoul;
    using ::_C_Swamp::wcsncat;
    using ::_C_Swamp::wcsncmp;
    using ::_C_Swamp::wcsncpy;
    using ::_C_Swamp::wcsrchr;
    using ::_C_Swamp::wcsspn;
    // using ::_C_Swamp::wcswcs;

#if 0  /* not implemented in glibc-2 */
    inline size_t wcsftime(wchar_t* __s, size_t __n, 
		           char const* __fmt, struct tm const* __tmb)
      { return ::_C_Swamp::wcsftime(__s,__n,__fmt,__tmb); }

    using ::_C_Swamp::wctype;
#endif

  }
  
  namespace _C_Swamp {
    namespace _C_Shadow {
#if 0 /* XXX glibc-2.0 does not implement these. */
      using ::std::fgetwc;
      using ::std::fgetws;
      using ::std::fputwc;
      using ::std::fputws;
      using ::std::ungetwc;
      using ::std::getwc;
      using ::std::getwchar;
      using ::std::putwc;
      using ::std::putwchar;
      using ::std::wcsftime;
      // XXX also wprintf etc.
#endif
    }
  }

# undef _IN_C_SWAMP_

#endif
