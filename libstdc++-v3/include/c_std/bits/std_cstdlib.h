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
// ISO C++ 14882: 20.4.6  C library
//

#ifndef _CPP_CSTDLIB
#define _CPP_CSTDLIB 1

#include <bits/c++config.h>

#pragma GCC system_header
#include_next <stdlib.h>

// Get rid of those macros defined in <stdlib.h> in lieu of real functions.
#undef atof
#undef atoi
#undef atol
#undef strtod
//#undef strtof
#undef strtol
#undef strtoul
#undef rand
#undef srand
#undef calloc
#undef free
#undef malloc
#undef realloc
#undef abort
#undef atexit
#undef exit
//#undef _Exit
#undef getenv
#undef system
#undef bsearch
#undef qsort
#undef abs
#undef labs
#undef llabs
#undef div
#undef ldiv
#undef lldiv
#undef mblen
#undef mbtowc
#undef wctomb
#undef mbstowcs
#undef wcstombs
#undef atoll
#undef strtoll
#undef strtoull
#undef strtold

namespace std 
{
  using ::size_t;	// cstddef

  using ::div_t;
  using ::ldiv_t;

  using ::atof;
  using ::atoi;
  using ::atol;
  using ::strtod;
  //  using ::strtof;
  using ::strtol;
  using ::strtoul;
  using ::rand;
  using ::srand;
  using ::calloc;
  using ::free;
  using ::malloc;
  using ::realloc;
  using ::abort;
  using ::atexit;
  using ::exit;
  // using ::_Exit;
  using ::getenv;
  using ::system;
  using ::bsearch;
  using ::qsort;
  using ::abs;
  using ::labs;
  using ::div;
  using ::ldiv;
  using ::mblen;
  using ::mbtowc;
  using ::wctomb;
  using ::mbstowcs;
  using ::wcstombs;

  inline long 
  abs(long __i) { return labs(__i); }

  inline ldiv_t
  div(long __i, long __j) { return ldiv(__i, __j); }
} // namespace std

#if _GLIBCPP_USE_C99
namespace __gnu_cxx
{
  using ::lldiv_t;

  inline long long 
  abs(long long __x) { return __x >= 0 ? __x : -__x; }

  inline long long 
  llabs(long long __x) { return __x >= 0 ? __x : -__x; }

  inline lldiv_t 
  div(long long __n, long long __d)
  { lldiv_t __q; __q.quot = __n / __d; __q.rem = __n % __d; return __q; }

  inline lldiv_t 
  lldiv(long long __n, long long __d)
  { lldiv_t __q; __q.quot = __n / __d; __q.rem = __n % __d; return __q; }

  using ::atoll;
  using ::strtoll;
  using ::strtoull;

#ifdef _GLIBCPP_HAVE_STRTOLD
  using ::strtold; 
#endif
} // namespace __gnu_cxx

namespace std
{
  using __gnu_cxx::lldiv_t;
  using __gnu_cxx::abs;
  using __gnu_cxx::llabs; 
  using __gnu_cxx::div;
  using __gnu_cxx::lldiv;
  using __gnu_cxx::atoll;
  using __gnu_cxx::strtoll;
  using __gnu_cxx::strtoull;
#ifdef _GLIBCPP_HAVE_STRTOLD
  using __gnu_cxx::strtold;
#endif
}
#endif

#endif 
