// Utility subroutines for the C++ library testsuite.
//
// Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// This file provides the following:
//
// 1)  VERIFY(), via DEBUG_ASSERT, from Brent Verner <brent@rcfile.org>.
//   This file is included in the various testsuite programs to provide
//   #define(able) assert() behavior for debugging/testing. It may be
//   a suitable location for other furry woodland creatures as well.
//
// 2)  __set_testsuite_memlimit()
//   __set_testsuite_memlimit() uses setrlimit() to restrict dynamic memory
//   allocation.  We provide a default memory limit if none is passed by the
//   calling application.  The argument to __set_testsuite_memlimit() is the
//   limit in megabytes (a floating-point number).  If _GLIBCPP_MEM_LIMITS is
//   not #defined before including this header, then no limiting is attempted.
//
// 3)  gnu_counting_struct
//   This is a POD with a static data member, gnu_counting_struct::count,
//   which starts at zero, increments on instance construction, and decrements
//   on instance destruction.  "assert_count(n)" can be called to VERIFY()
//   that the count equals N.
//
// 4)  gnu_copy_tracker, from Stephen M. Webb <stephen@bregmasoft.com>.
//   A class with nontrivial ctor/dtor that provides the ability to track the
//   number of copy ctors and dtors, and will throw on demand during copy.
//
// 5) gnu_char, gnu_char_traits, abstract character classes and
// char_traits specializations for testing instantiations.

#ifndef _GLIBCPP_TESTSUITE_HOOKS_H
#define _GLIBCPP_TESTSUITE_HOOKS_H

#include <bits/c++config.h>
#include <cstddef>

#ifdef DEBUG_ASSERT
# include <cassert>
# define VERIFY(fn) assert(fn)
#else
# define VERIFY(fn) test &= (fn)
#endif

// Defined in GLIBCPP_CONFIGURE_TESTSUITE.
#ifndef _GLIBCPP_MEM_LIMITS
// Don't do memory limits.
extern void
__set_testsuite_memlimit(float x = 0);

#else

// Do memory limits.
#ifndef MEMLIMIT_MB
#define MEMLIMIT_MB 16.0
#endif

extern void
__set_testsuite_memlimit(float __size = MEMLIMIT_MB);
#endif


struct gnu_counting_struct
{
    // Specifically and glaringly-obviously marked 'signed' so that when
    // COUNT mistakenly goes negative, we can track the patterns of
    // deletions more easily.
    typedef  signed int     size_type;
    static size_type   count;
    gnu_counting_struct() { ++count; }
    gnu_counting_struct (const gnu_counting_struct&) { ++count; }
    ~gnu_counting_struct() { --count; }
};

#define assert_count(n)   VERIFY(gnu_counting_struct::count == n)


class gnu_copy_tracker
{
  public:
    // Cannot be explicit.  Conversion ctor used by list_modifiers.cc's
    // test03(), "range fill at beginning".
    gnu_copy_tracker (int anId, bool throwOnDemand = false)
    : itsId(anId), willThrow(throwOnDemand)
    {}

    gnu_copy_tracker (const gnu_copy_tracker& rhs)
    : itsId(rhs.id()), willThrow(rhs.willThrow)
    {
      ++itsCopyCount;
      if (willThrow) throw "copy tracker exception";
    }

    gnu_copy_tracker& operator=(const gnu_copy_tracker& rhs)
    {
      itsId = rhs.id();
      // willThrow must obviously already be false to get this far
    }

    ~gnu_copy_tracker() { ++itsDtorCount; }

    int
    id() const
    { return itsId; }

  private:
          int   itsId;
    const bool  willThrow;

  public:
    static void
    reset()
    { itsCopyCount = 0; itsDtorCount = 0; }

    static int
    copyCount() 
    { return itsCopyCount; }

    static int
    dtorCount() 
    { return itsDtorCount; }

  private:
    static int itsCopyCount;
    static int itsDtorCount;
};

struct gnu_char
{
  unsigned long c;
};

struct gnu_int
{
  unsigned long i;
};

struct gnu_state
{
  unsigned long l;
  unsigned long l2;
};

// char_traits specialization
namespace std
{
  template<class _CharT>
    struct char_traits;

  template<>
    struct char_traits<gnu_char>
    {
      typedef gnu_char 		char_type;
      typedef gnu_int  		int_type;
      typedef long 		pos_type;
      typedef unsigned long 	off_type;
      typedef gnu_state 	state_type;
      
      static void 
      assign(char_type& __c1, const char_type& __c2);

      static bool 
      eq(const char_type& __c1, const char_type& __c2);

      static bool 
      lt(const char_type& __c1, const char_type& __c2);

      static int 
      compare(const char_type* __s1, const char_type* __s2, size_t __n);

      static size_t
      length(const char_type* __s);

      static const char_type* 
      find(const char_type* __s, size_t __n, const char_type& __a);

      static char_type* 
      move(char_type* __s1, const char_type* __s2, size_t __n);

      static char_type* 
      copy(char_type* __s1, const char_type* __s2, size_t __n);

      static char_type* 
      assign(char_type* __s, size_t __n, char_type __a);

      static char_type 
      to_char_type(const int_type& __c);

      static int_type 
      to_int_type(const char_type& __c);

      static bool 
      eq_int_type(const int_type& __c1, const int_type& __c2);

      static int_type 
      eof();

      static int_type 
      not_eof(const int_type& __c);
    };
} // namespace std

#endif // _GLIBCPP_TESTSUITE_HOOKS_H

