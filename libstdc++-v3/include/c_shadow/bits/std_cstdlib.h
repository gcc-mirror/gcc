// -*- C++ -*- header wrapper

// Copyright (C) 1997-1999, 2000 Free Software Foundation, Inc.
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

// Function decls in this header are overloaded on the 
// extern "C"-ness of arguments.  This is a rich ground
// for compiler bugs.

#ifndef _CPP_CSTDLIB
#define _CPP_CSTDLIB 1

# include <bits/c++config.h>
# include <bits/std_cstddef.h>  

namespace _C_legacy {
  extern "C" {
#     define _IN_C_LEGACY_
#     pragma GCC system_header
#     include_next <stdlib.h>
      typedef int (*_C_cmp_fun_ptr)(const void*, const void*);  // C fn ptr
    }

  typedef div_t   _CPP_div_t_capture;
  typedef ldiv_t  _CPP_ldiv_t_capture;

# if _GLIBCPP_HAVE_LLDIV_T
  typedef lldiv_t  _CPP_lldiv_t_capture;
# endif
} // namespace _C_legacy

#  undef wchar_t
#  undef div_t
#  undef ldiv_t

#  undef atof
#  undef atoi
#  undef atol
#  undef strtod
#  undef strtol
#  undef strtoul
#ifdef _GLIBCPP_HAVE_STRTOLD
#  undef strtold
#endif
#  undef rand
#  undef srand
#  undef calloc
#  undef free
#  undef malloc
#  undef realloc
#  undef abort
#  undef atexit
#  undef exit
#  undef getenv
#  undef system
#  undef bsearch
#  undef qsort
#  undef abs
#  undef div
#  undef labs
#  undef ldiv
#ifdef _GLIBCPP_USE_C99
#  undef llabs
#  undef lldiv
#endif
#  undef mblen
#  undef mbtowc
#  undef wctomb
#  undef mbstowcs
#  undef wcstombs

namespace std {
  struct div_t : _C_legacy::_CPP_div_t_capture { };
  struct ldiv_t : _C_legacy::_CPP_ldiv_t_capture { };

#ifdef _GLIBCPP_USE_C99
# ifdef _GLIBCPP_HAVE_LLDIV_T
  struct lldiv_t : _C_legacy::_CPP_lldiv_t_capture { };
# else
  struct lldiv_t
  {
    long long quot;
    long long rem;
  };
# endif
#endif

  using _C_legacy::atof;
  using _C_legacy::atoi;
  using _C_legacy::atol;
  using _C_legacy::strtod;
  using _C_legacy::strtol;
  using _C_legacy::strtoul;
  using _C_legacy::rand;
  using _C_legacy::srand;
  using _C_legacy::calloc;
  using _C_legacy::free;
  using _C_legacy::malloc;
  using _C_legacy::realloc;

  //  using _C_legacy::abort;
  using _C_legacy::atexit;
  //  using _C_legacy::exit;
  using _C_legacy::bsearch;
  using _C_legacy::qsort; 

  using _C_legacy::getenv;
  using _C_legacy::system;
  using _C_legacy::mbtowc;
  using _C_legacy::wctomb;
  using _C_legacy::mbstowcs;
  using _C_legacy::wcstombs;

  using _C_legacy::strtof;

#ifdef _GLIBCPP_USE_LONG_LONG
  using _C_legacy::strtoll;
  using _C_legacy::strtoull;
#endif

#ifdef _GLIBCPP_HAVE_STRTOLD
  using _C_legacy::strtold;
#endif

  using _C_legacy::mblen;

  inline int 
  abs(int __x) { return __x >= 0 ? __x : -__x; }

  inline div_t 
  div(int __n, int __d)
  { div_t __q; __q.quot = __n / __d; __q.rem = __n % __d; return __q; }

  inline long 
  labs(long __x) { return __x >= 0 ? __x : -__x; }

  inline long 
  abs(long __x) { return __x >= 0 ? __x : -__x; }

  inline ldiv_t 
  ldiv(long __n, long __d)
  { ldiv_t __q; __q.quot = __n / __d; __q.rem = __n % __d; return __q; }

  inline ldiv_t 
  div(long __n, long __d)
  { ldiv_t __q; __q.quot = __n / __d; __q.rem = __n % __d; return __q; }

#ifdef _GLIBCPP_USE_C99
  inline long long 
  llabs(long long __x) { return __x >= 0 ? __x : -__x; }

  inline long long 
  abs(long long __x) { return __x >= 0 ? __x : -__x; }

  inline lldiv_t 
  lldiv(long long __n, long long __d)
  { lldiv_t __q; __q.quot = __n / __d; __q.rem = __n % __d; return __q; }

  inline lldiv_t 
  div(long long __n, long long __d)
  { lldiv_t __q; __q.quot = __n / __d; __q.rem = __n % __d; return __q; }
#endif
} // namespace std
  
# undef _IN_C_LEGACY_

#endif


