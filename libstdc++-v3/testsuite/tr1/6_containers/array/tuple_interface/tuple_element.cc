// 2005-08-26  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

// 6.2.2 Class template array

#include <tr1/array>
#include <tr1/type_traits>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std::tr1;

  {
    const size_t len = 3;
    typedef array<int, len> array_type;
    VERIFY( (is_same<tuple_element<0, array_type>::type, int>::value == true) );
    VERIFY( (is_same<tuple_element<1, array_type>::type, int>::value == true) );
    VERIFY( (is_same<tuple_element<2, array_type>::type, int>::value == true) );
  }

  {
    const size_t len = 0;
    typedef array<int, len> array_type;
    VERIFY( (is_same<tuple_element<0, array_type>::type, int>::value == true) );
  }
}

int main()
{
  test01();
  return 0;
}
