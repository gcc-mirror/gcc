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


#ifndef  _INCLUDED_CPP_WCHAR_H_
# undef _SHADOW_NAME
# define _SHADOW_NAME <cwchar>
# include <bits/generic_shadow.h>
# undef _SHADOW_NAME

# ifndef _IN_C_SWAMP_
  using ::std::size_t;  /* handled in <cstddef> */
  using ::std::wint_t;
  using ::std::mbstate_t;

# if 0  /* glibc-2.0 doesn't define these */
  using ::std::fgetwc;
  using ::std::fgetws;
  using ::std::fputwc;
  using ::std::fputws;
  using ::std::ungetwc;
  using ::std::getwc;
  using ::std::getwchar;
  using ::std::putwc;
  using ::std::putwchar;
  using ::std::wprintf;
  using ::std::wsprintf;
  using ::std::wvsprintf;
  using ::std::wfsprintf;
  using ::std::wscanf;
  using ::std::wsscanf;
  using ::std::wvsscanf;
  using ::std::wfscanf;
    // XXX etc.
  using ::std::wcsftime;
# endif 

  using ::std::wcscpy;
  using ::std::wcscat;
  using ::std::wcscmp;
  using ::std::wcscoll;
  using ::std::wcsxfrm;
  using ::std::wcsdup;
  using ::std::wcschr;
  using ::std::wcscspn;
  using ::std::wcspbrk;
  using ::std::wcsstr;
  using ::std::wcstok;
  using ::std::wcslen;
# ifndef __sun
    using ::std::wmemchr;
    using ::std::wmemcmp;
    using ::std::wmemcpy;
    using ::std::wmemmove;
    using ::std::wmemset;
    using ::std::btowc;
    using ::std::wctob;
    using ::std::mbsinit;
    using ::std::mbrtowc;
    using ::std::wcrtomb;
    using ::std::mbrlen;
# endif
# ifdef __USE_GNU
    using ::std::mbsrtowcs;
    using ::std::wcsrtombs;
    using ::std::mbsnrtowcs;
    using ::std::mbsnrtombs;
    using ::std::wcwidth;
    using ::std::wcswidth;
    using ::std::wcscmpy;
# endif
  using ::std::wcstod;
  using ::std::wcstol;
  using ::std::wcstoul;
  using ::std::wcsncat;
  using ::std::wcsncmp;
  using ::std::wcsncpy;
  using ::std::wcsrchr;
  using ::std::wcsspn;
# define _INCLUDED_CPP_WCHAR_H_ 1
# endif /* _IN_C_SWAMP_ */

#endif /* _INCLUDED_CPP_WCHAR_H_ */
