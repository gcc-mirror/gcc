// { dg-do compile }

// 2006-01-29  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 8.22 Header <cstdint>

#include <tr1/cstdint>

void test01()
{
#if _GLIBCXX_USE_C99_STDINT_TR1

  typedef int8_t          my_int8_t;
  typedef int16_t         my_int16_t;
  typedef int32_t         my_int32_t;
  typedef int64_t         my_int64_t;
  typedef int_fast8_t     my_int_fast8_t;
  typedef int_fast16_t    my_int_fast16_t;
  typedef int_fast32_t    my_int_fast32_t;
  typedef int_fast64_t    my_int_fast64_t;	
  typedef int_least8_t    my_int_least8_t;
  typedef int_least16_t   my_int_least16_t;
  typedef int_least32_t   my_int_least32_t;
  typedef int_least64_t   my_int_least64_t;
  typedef intmax_t        my_intmax_t;
  typedef intptr_t        my_intptr_t;
  typedef uint8_t         my_uint8_t;
  typedef uint16_t        my_uint16_t;
  typedef uint32_t        my_uint32_t;
  typedef uint64_t        my_uint64_t;
  typedef uint_fast8_t    my_uint_fast8_t;
  typedef uint_fast16_t   my_uint_fast16_t;
  typedef uint_fast32_t   my_uint_fast32_t;
  typedef uint_fast64_t   my_uint_fast64_t;	
  typedef uint_least8_t   my_uint_least8_t;
  typedef uint_least16_t  my_uint_least16_t;
  typedef uint_least32_t  my_uint_least32_t;
  typedef uint_least64_t  my_uint_least64_t;
  typedef uintmax_t       my_uintmax_t;
  typedef uintptr_t       my_uintptr_t;
  
#endif
}
