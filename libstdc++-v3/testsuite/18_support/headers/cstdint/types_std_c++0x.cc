// { dg-do compile { target c++11 } }
// { dg-require-cstdint "" }

// Copyright (C) 2007-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <cstdint>

void test01()
{
  typedef std::int8_t          my_int8_t;
  typedef std::int16_t         my_int16_t;
  typedef std::int32_t         my_int32_t;
  typedef std::int64_t         my_int64_t;
  typedef std::int_fast8_t     my_int_fast8_t;
  typedef std::int_fast16_t    my_int_fast16_t;
  typedef std::int_fast32_t    my_int_fast32_t;
  typedef std::int_fast64_t    my_int_fast64_t;	
  typedef std::int_least8_t    my_int_least8_t;
  typedef std::int_least16_t   my_int_least16_t;
  typedef std::int_least32_t   my_int_least32_t;
  typedef std::int_least64_t   my_int_least64_t;
  typedef std::intmax_t        my_intmax_t;
  typedef std::intptr_t        my_intptr_t;
  typedef std::uint8_t         my_uint8_t;
  typedef std::uint16_t        my_uint16_t;
  typedef std::uint32_t        my_uint32_t;
  typedef std::uint64_t        my_uint64_t;
  typedef std::uint_fast8_t    my_uint_fast8_t;
  typedef std::uint_fast16_t   my_uint_fast16_t;
  typedef std::uint_fast32_t   my_uint_fast32_t;
  typedef std::uint_fast64_t   my_uint_fast64_t;	
  typedef std::uint_least8_t   my_uint_least8_t;
  typedef std::uint_least16_t  my_uint_least16_t;
  typedef std::uint_least32_t  my_uint_least32_t;
  typedef std::uint_least64_t  my_uint_least64_t;
  typedef std::uintmax_t       my_uintmax_t;
  typedef std::uintptr_t       my_uintptr_t;
}
