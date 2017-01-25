// -*- C++ -*-

// Copyright (C) 2006-2017 Free Software Foundation, Inc.
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
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <ext/type_traits.h>
#include <tr1/type_traits>
#include <testsuite_hooks.h>

template<typename T>
  void
  check_add_unsigned()
  {
    typedef typename __gnu_cxx::__add_unsigned<T>::__type unsigned_type;
    VERIFY( std::tr1::is_unsigned<unsigned_type>::value );
  }

int main()
{
  check_add_unsigned<char>();
  check_add_unsigned<unsigned char>();
  check_add_unsigned<signed char>();
  check_add_unsigned<int>();
  check_add_unsigned<unsigned int>();
  check_add_unsigned<signed int>();
  check_add_unsigned<long>();
  check_add_unsigned<unsigned long>();
  check_add_unsigned<signed long>();
  check_add_unsigned<long long>();
  check_add_unsigned<unsigned long long>();
  check_add_unsigned<signed long long>();
  return 0;
}
