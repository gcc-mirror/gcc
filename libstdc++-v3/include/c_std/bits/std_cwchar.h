// -*- C++ -*- forwarding header.

// Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
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
// ISO C++ 14882: ???
//

#ifndef _CPP_CWCHAR
#define _CPP_CWCHAR 1

#include <bits/c++config.h>

#if _GLIBCPP_HAVE_WCHAR_H
#pragma GCC system_header
#include_next <wchar.h>
#endif

// Need to do a bit of trickery here with mbstate_t as char_traits
// assumes it is in wchar.h, regardless of wchar_t specializations.
#ifndef _GLIBCPP_HAVE_MBSTATE_T
extern "C" 
{
  typedef struct 
  {
    int __fill[6];
  } mbstate_t;
}
#endif

namespace std 
{
  using ::mbstate_t;
}

// Get rid of those macros defined in <wchar.h> in lieu of real functions.
#undef btowc
#undef wctob
#undef fgetwc
#undef fgetwc
#undef fgetws
#undef fputwc
#undef fputws
#undef fwide
#undef fwprintf
#undef fwscanf
#undef swprintf
#undef swscanf
#undef vfwprintf
#undef vfwscanf
#undef vswprintf
#undef vswscanf
#undef vwprintf
#undef vwscanf
#undef wprintf
#undef wscanf
#undef getwc
#undef getwchar
#undef mbsinit
#undef mbrlen
#undef mbrtowc
#undef mbsrtowcs
#undef wcsrtombs
#undef putwc
#undef putwchar
#undef ungetwc
#undef wcrtomb
#undef wcstod
#undef wcstof
#undef wcstol
#undef wcstoul
#undef wcscpy
#undef wcsncpy
#undef wcscat
#undef wcsncat
#undef wcscmp
#undef wcscoll
#undef wcsncmp
#undef wcsxfrm
#undef wcscspn
#undef wcslen
#undef wcsspn
#undef wcstok
#undef wmemcmp
#undef wmemcpy
#undef wmemmove
#undef wmemset
#undef wcsftime

#undef wcschr
#undef wcspbrk
#undef wcsrchr
#undef wcsstr
#undef wmemchr

#if _GLIBCPP_USE_WCHAR_T
namespace std
{
  using ::size_t;	// cstddef

  using ::wint_t;

  using ::btowc;
  using ::wctob;
#if _GLIBCPP_HAVE_FGETWC
  using ::fgetwc;
#endif
#if _GLIBCPP_HAVE_FGETWS
  using ::fgetws;
#endif
  using ::fputwc;
  using ::fputws;
  using ::fwide;
  using ::fwprintf;
  using ::fwscanf;
  using ::swprintf;
  using ::swscanf;
  using ::vfwprintf;
  using ::vfwscanf;
  using ::vswprintf;
  using ::vswscanf;
  using ::vwprintf;
  using ::vwscanf;
  using ::wprintf;
  using ::wscanf;
  using ::getwc;
  using ::getwchar;
  using ::mbsinit;
  using ::mbrlen;
  using ::mbrtowc;
  using ::mbsrtowcs;
  using ::wcsrtombs;
  using ::putwc;
  using ::putwchar;
  using ::ungetwc;
  using ::wcrtomb;
  using ::wcstod;
  using ::wcstof;
  using ::wcstol;
  using ::wcstoul;
  using ::wcscpy;
  using ::wcsncpy;
  using ::wcscat;
  using ::wcsncat;
  using ::wcscmp;
  using ::wcscoll;
  using ::wcsncmp;
  using ::wcsxfrm;
  using ::wcscspn;
  using ::wcslen;
  using ::wcsspn;
  using ::wcstok;
  using ::wmemcmp;
  using ::wmemcpy;
  using ::wmemmove;
  using ::wmemset;
  using ::wcsftime;

  using ::wcschr;

  inline wchar_t*
  wcschr(wchar_t* __p, wchar_t __c)
  { return wcschr(const_cast<const wchar_t*>(__p), __c); }

  using ::wcspbrk;

  inline wchar_t*
  wcspbrk(wchar_t* __s1, wchar_t* __s2)
  { return wcspbrk(const_cast<const wchar_t*>(__s1), __s2); }

  using ::wcsrchr;

  inline wchar_t*
  wcsrchr(wchar_t* __p, wchar_t __c)
  { return wcsrchr(const_cast<const wchar_t*>(__p), __c); }

  using ::wcsstr;

  inline wchar_t*
  wcsstr(wchar_t* __s1, wchar_t* __s2)
  { return wcsstr(const_cast<const wchar_t*>(__s1), __s2); }

  using ::wmemchr;

  inline wchar_t*
  wmemchr(wchar_t* __p, wchar_t __c, size_t __n)
  { return wmemchr(const_cast<const wchar_t*>(__p), __c, __n); }
}

#if _GLIBCPP_USE_C99
namespace __gnu_cxx
{
  using ::wcstold;
  using ::wcstoll;
  using ::wcstoull;
}

namespace std
{
  using __gnu_cxx::wcstold;
  using __gnu_cxx::wcstoll;
  using __gnu_cxx::wcstoull;
}
#endif

#endif //_GLIBCPP_USE_WCHAR_T

#endif 
