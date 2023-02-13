// -*- C++ -*-

// Copyright (C) 2006-2023 Free Software Foundation, Inc.
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
  check_remove_unsigned()
  {
    typedef typename __gnu_cxx::__remove_unsigned<T>::__type signed_type;
    VERIFY( std::tr1::is_signed<signed_type>::value );
  }

int main()
{
  check_remove_unsigned<char>();
  check_remove_unsigned<unsigned char>();
  check_remove_unsigned<signed char>();
  check_remove_unsigned<int>();
  check_remove_unsigned<unsigned int>();
  check_remove_unsigned<signed int>();
  check_remove_unsigned<long>();
  check_remove_unsigned<unsigned long>();
  check_remove_unsigned<signed long>();
  check_remove_unsigned<long long>();
  check_remove_unsigned<unsigned long long>();
  check_remove_unsigned<signed long long>();
  return 0;
}
