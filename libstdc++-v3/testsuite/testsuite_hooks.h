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
// 4) gnu_char, gnu_char_traits, abstract character classes and
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
void
__set_testsuite_memlimit(float x = 0)
{ }

#else

// Do memory limits.
#include <sys/resource.h>
#include <unistd.h>

#ifndef MEMLIMIT_MB
#define MEMLIMIT_MB 16.0
#endif

void
__set_testsuite_memlimit(float __size = MEMLIMIT_MB)
{
    struct rlimit r;
    // Cater to the absence of rlim_t.
    __typeof__ (r.rlim_cur) limit
      = (__typeof__ (r.rlim_cur))(__size * 1048576);

    // Heap size, seems to be common.
#if _GLIBCPP_HAVE_MEMLIMIT_DATA
    getrlimit(RLIMIT_DATA, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_DATA, &r);
#endif

    // Resident set size.
#if _GLIBCPP_HAVE_MEMLIMIT_RSS
    getrlimit(RLIMIT_RSS, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_RSS, &r);
#endif

    // Mapped memory (brk + mmap).
#if _GLIBCPP_HAVE_MEMLIMIT_VMEM
    getrlimit(RLIMIT_VMEM, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_VMEM, &r);
#endif

    // Virtual memory.
#if _GLIBCPP_HAVE_MEMLIMIT_AS
    getrlimit(RLIMIT_AS, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_AS, &r);
#endif
}
#endif


struct gnu_counting_struct
{
    // Specifically and glaringly-obviously marked 'signed' so that when
    // count mistakenly goes negative, we can track the patterns of
    // deletions easier.
    typedef  signed int     size_type;
    static size_type   count;
    gnu_counting_struct() { ++count; }
    gnu_counting_struct (const gnu_counting_struct&) { ++count; }
    ~gnu_counting_struct() { --count; }
};

#define assert_count(n)   VERIFY(gnu_counting_struct::count == n)

gnu_counting_struct::size_type  gnu_counting_struct::count = 0;

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

