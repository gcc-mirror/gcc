// -*- C++ -*- header wrapper

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
// ISO C++ 14882: 20.4.6  C library
//

// Function decls in this header are overloaded on the 
// extern "C"-ness of arguments.  This is a rich ground
// for compiler bugs.

#ifndef _CPP_CSTDLIB
#define _CPP_CSTDLIB 1

# include <bits/std_cstddef.h>  /* pick up NULL, size_t */

  namespace _C_Swamp {
    extern "C" {
#     define _IN_C_SWAMP_
#     include_next <stdlib.h>
      typedef int (*_C_cmp_fun_ptr)(const void*, const void*);  // C fn ptr
    }
    const int _CPP_EXIT_SUCCESS_capture = int(EXIT_SUCCESS);
    const int _CPP_EXIT_FAILURE_capture = int(EXIT_FAILURE);
    const int _CPP_RAND_MAX_capture     = int(RAND_MAX);
    inline int _CPP_MB_CUR_MAX_capture() { return int(MB_CUR_MAX); }
    // typedef size_t  _CPP_size_t_capture;
    // typedef wchar_t _CPP_wchar_t_capture;
    typedef div_t   _CPP_div_t_capture;
    typedef ldiv_t  _CPP_ldiv_t_capture;

    namespace _C_Shadow { 
    }
  } // close namespace ::_C_Swamp::

#  undef size_t
#  undef wchar_t
#  undef div_t
#  undef ldiv_t

// <cstddef>
// #  undef  NULL
// #  define NULL         0
#  undef  EXIT_SUCCESS
#  define EXIT_SUCCESS (::_C_Swamp::_CPP_EXIT_SUCCESS_capture)
#  undef  EXIT_FAILURE
#  define EXIT_FAILURE (::_C_Swamp::_CPP_EXIT_FAILURE_capture)
#  undef  RAND_MAX
#  define RAND_MAX     (::_C_Swamp::_CPP_RAND_MAX_capture)
#  undef  MB_CUR_MAX
#  define MB_CUR_MAX   (::_C_Swamp::_CPP_MB_CUR_MAX_capture())

#  undef atof
#  undef atoi
#  undef atol
#  undef strtod
#  undef strtol
#  undef strtoul
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
#  undef mblen
#  undef mbtowc
#  undef wctomb
#  undef mbstowcs
#  undef wcstombs

  namespace _C_Swamp {
    namespace _C_Shadow {
      // these come from <cstddef>
      // typedef ::_C_Swamp::_CPP_size_t_capture  size_t;
      // typedef ::_C_Swamp::_CPP_wchar_t_capture wchar_t;
    }
  }
  namespace std {

    // Adopt C names into std::
    // using ::_C_Swamp::_C_Shadow::size_t;  
    // using ::_C_Swamp::_C_Shadow::wchar_t;

    typedef ::_C_Swamp::_CPP_div_t_capture _CPP_div_t;
    struct div_t  : _CPP_div_t {};
    typedef ::_C_Swamp::_CPP_ldiv_t_capture _CPP_ldiv_t;
    struct ldiv_t : _CPP_ldiv_t {};
    // note: div_t and ldiv_t are still POD types 

    // EXIT_SUCCESS, EXIT_FAILURE, RAND_MAX, MB_CUR_MAX, NULL

    using ::_C_Swamp::atof;
    using ::_C_Swamp::atoi;
    using ::_C_Swamp::atol;
    using ::_C_Swamp::strtod;
    using ::_C_Swamp::strtol;
    using ::_C_Swamp::strtoul;
    using ::_C_Swamp::rand;
    using ::_C_Swamp::srand;
    using ::_C_Swamp::calloc;
    using ::_C_Swamp::free;
    using ::_C_Swamp::malloc;
    using ::_C_Swamp::realloc;
    void abort();     // XXX must define this correctly for C++.

#define _FUN_OVERLOAD_BUG
    // extern "C" funs declared void f(int (*)()) overload OK with C++ funs,
    //  but not if declared void f(int (*)(void)).

#if !defined(__GNUC__) || !defined(_FUN_OVERLOAD_BUG)
    using ::_C_Swamp::atexit;     // XXX must redefine this one!
#endif
    int atexit(void (*)());       // XXX must define this.
      // More precisely, we need an extern "C" atexit() that
      //   implements the correct semantics, but hides the link-name
      //   atexit found in the C library, and a C++ atexit too.  If
      //   C and C++ calling conventions differ, each pointer must
      //   be marked to be called using the proper convention.

    void exit(int __i)   // XXX must define correctly for C++, like atexit().
      { ::_C_Swamp::exit(__i); }

    using ::_C_Swamp::getenv;
    using ::_C_Swamp::system;

    // XXX note that the raw conversion between C and C++ function 
    // pointers may not be portable to some targets; those targets need
    // a forwarding function.

#if !defined(__GNUC__) || !defined(_FUN_OVERLOAD_BUG)
    using ::_C_Swamp::bsearch;  // overload
#endif
    inline void* bsearch(const void* __key, const void* __base, size_t __n, 
		  size_t __size, int (*__cmp)(const void*, const void*))
      { return ::_C_Swamp::bsearch(__key,__base,__n,__size,
          reinterpret_cast< ::_C_Swamp::_C_cmp_fun_ptr>(__cmp)); }  // XXX

#if !defined(__GNUC__) || !defined(_FUN_OVERLOAD_BUG)
    using ::_C_Swamp::qsort;    // overload
#endif
    inline void qsort(void* __base, size_t __n, size_t __size,
	              int (*__cmp)(const void*, const void*)) 
      { return ::_C_Swamp::qsort(__base,__n,__size,
          reinterpret_cast< ::_C_Swamp::_C_cmp_fun_ptr>(__cmp)); }  // XXX


    inline int abs(int __x) 
      { return __x >= 0 ? __x : -__x; }
    inline div_t div(int __n, int __d)
      { _div_t __q; __q.quot = __n / __d; __q.rem = __n % __d; return __q; }
    inline long labs(long __x)
      { return __x >= 0 ? __x : -__x; }
    inline ldiv_t ldiv(long __num, long __den)
      { _ldiv_t __q; __q.quot = __n / __d; __q.rem = __n % __d; return __q; }

    using ::_C_Swamp::mblen;
    using ::_C_Swamp::mbtowc;;
    using ::_C_Swamp::wctomb;
    using ::_C_Swamp::mbstowcs;
    using ::_C_Swamp::wcstombs;

  } // close namespace std::
  
  namespace _C_Swamp {
    namespace _C_Shadow {
      using ::std::div_t;
      using ::std::ldiv_t;

      using ::std::abort;
      using ::std::atexit;
      using ::std::exit;
      using ::std::bsearch;
      using ::std::qsort;
      using ::std::abs;
      using ::std::div;
      using ::std::labs;
      using ::std::ldiv;
    }
  }

# undef _IN_C_SWAMP_

#endif

