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

// Note: This is not a conforming implementation.

#ifndef _CPP_CSTDLIB
#define _CPP_CSTDLIB 1

#include <bits/c++config.h>
#include <bits/std_cstddef.h>

#pragma GCC system_header
#include <stdlib.h>

// Get rid of those macros defined in <stdlib.h> in lieu of real functions.
#undef atof
#undef atoi
#undef atol
#undef strtod
#undef strtof
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
#undef _Exit
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
  using ::div_t;
  using ::ldiv_t;

  extern "C" double atof(const char*); 
  extern "C" int atoi(const char*); 
  extern "C" long int atol(const char*); 
  extern "C" double strtod(const char*, char**); 
  extern "C" float strtof(const char*, char**); 
  extern "C" long int strtol(const char*, char**, int); 
  extern "C" unsigned long int strtoul(const char*, char**, int);
  extern "C" int rand(void); 
  extern "C" void srand(unsigned int); 
  extern "C" void* calloc(size_t, size_t); 
  extern "C" void free(void*); 
  extern "C" void* malloc(size_t); 
  extern "C" void* realloc(void*, size_t); 
  extern "C" void abort(void); 
  extern "C" int atexit(void (*func)(void)); 
  extern "C" void exit(int); 
  extern "C" void _Exit(int); 
  extern "C" char*getenv(const char*); 
  extern "C" int system(const char*); 
  extern "C" void* bsearch(const void*, const void*, size_t, size_t, 
			   int (*comp)(const void *, const void *)); 
  extern "C" void qsort(void*, size_t, size_t, 
			int (*comp)(const void *, const void *)); 
  extern "C" int abs(int); 
  extern "C" long int labs(long int); 
  extern "C" div_t div(int, int); 
  extern "C" ldiv_t ldiv(long int, long int); 
  extern "C" int mblen(const char*, size_t); 
  extern "C" int mbtowc(wchar_t*, const char*, size_t); 
  extern "C" int wctomb(char*, wchar_t); 
  extern "C" size_t mbstowcs(wchar_t*, const char*, size_t); 
  extern "C" size_t wcstombs(char*, const wchar_t*, size_t);

  inline long 
  abs(long __i) { return ::labs(__i); }

  inline ldiv_t
  div(long __i, long __j) { return ::ldiv(__i, __j); }
} // namespace std

#if _GLIBCPP_USE_C99
namespace c99
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

  extern "C" long long int atoll(const char*); 
  extern "C" long long int strtoll(const char*, char**, int); 
  extern "C" unsigned long long int strtoull(const char*, char**, int); 

#ifdef _GLIBCPP_HAVE_STRTOLD
  extern "C" long double strtold(const char*, char**); 
#endif
} // namespace c99

namespace std
{
  using c99::lldiv_t;
  using c99::abs;
  //using c99::llabs; // XXX ???
  using c99::div;
  using c99::lldiv;
  using c99::atoll;
  using c99::strtoll;
  using c99::strtoull;
#ifdef _GLIBCPP_HAVE_STRTOLD
  using c99::strtold;
#endif
}
#endif

#endif 
