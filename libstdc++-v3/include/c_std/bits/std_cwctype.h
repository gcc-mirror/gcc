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
// ISO C++ 14882: <cwctype>
//

// Note: This is not a conforming implementation.

#ifndef _CPP_CWCTYPE
#define _CPP_CWCTYPE 1

#include <bits/std_cwchar.h>

#pragma GCC system_header
#include <wctype.h>

// Get rid of those macros defined in <wctype.h> in lieu of real functions.
#undef iswalnum
#undef iswalpha
#undef iswblank
#undef iswcntrl
#undef iswdigit
#undef iswgraph
#undef iswlower
#undef iswprint
#undef iswprint
#undef iswpunct
#undef iswspace
#undef iswupper
#undef iswxdigit
#undef iswctype  
#undef towlower
#undef towupper
#undef towctrans
#undef wctrans

namespace std
{
  using ::wctype_t;
  using ::wctrans_t;

  extern "C" int iswalnum(wint_t); 
  extern "C" int iswalpha(wint_t); 
  extern "C" int iswblank(wint_t); 
  extern "C" int iswcntrl(wint_t); 
  extern "C" int iswdigit(wint_t); 
  extern "C" int iswgraph(wint_t); 
  extern "C" int iswlower(wint_t); 
  extern "C" int iswprint(wint_t); 
  extern "C" int iswpunct(wint_t); 
  extern "C" int iswspace(wint_t); 
  extern "C" int iswupper(wint_t); 
  extern "C" int iswxdigit(wint_t);
  extern "C" int iswctype(wint_t, wctype_t); 
  extern "C" wctype_t wctype(const char *); 
  extern "C" wint_t towlower(wint_t); 
  extern "C" wint_t towupper(wint_t); 
  extern "C" wint_t towctrans(wint_t, wctrans_t); 
  extern "C" wctrans_t wctrans(const char*);
}

#endif 







