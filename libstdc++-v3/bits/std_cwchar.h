// -*- C++ -*- forwarding header.

// Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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

// Note: this is not a conforming implementation.

#ifndef _CPP_CWCHAR
#define _CPP_CWCHAR 1

#include <bits/c++config.h>
#ifdef _GLIBCPP_USE_WCHAR_T
 # include_next <wchar.h>
#endif 

extern "C" {

#ifdef _GLIBCPP_USE_WCHAR_T

#ifndef _GLIBCPP_HAVE_WMEMCMP
  int wmemcmp(const wchar_t* __s1, const wchar_t* __s2, size_t __n);
#endif

#ifndef _GLIBCPP_HAVE_WCSLEN
  size_t wcslen(const wchar_t* __s);
#endif

#ifndef _GLIBCPP_HAVE_WMEMCHR
  wchar_t* wmemchr(const wchar_t* __s, wchar_t __c, size_t __n);
#endif

#ifndef _GLIBCPP_HAVE_WMEMMOVE
  wchar_t* wmemmove(wchar_t* __s1, const wchar_t* __s2, size_t __n);
#endif

#ifndef _GLIBCPP_HAVE_WMEMCPY
  wchar_t* wmemcpy(wchar_t* __s1, const wchar_t* __s2, size_t __n);
#endif

#ifndef _GLIBCPP_HAVE_WMEMSET
  wchar_t* wmemset(wchar_t* __s, wchar_t __c, size_t __n);
#endif

#endif //_GLIBCPP_USE_WCHAR_T

// NB: mbstate_t should be defined in <cwchar>, as per 
// 21.2 p5
// If not, autoconf will
// detect this with some configure time magic and define
// _GLIBCPP_NEED_MBSTATE_T (see config.h in the build directory.)
#ifdef _GLIBCPP_NEED_MBSTATE_T
  typedef struct 
  {
    int __fill[6];
  } mbstate_t;
#endif
}


#endif // _CPP_CWCHAR




















