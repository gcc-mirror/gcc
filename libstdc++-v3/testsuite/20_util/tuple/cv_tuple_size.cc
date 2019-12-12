// { dg-do run { target c++11 } }

// Copyright (C) 2011-2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// Tuple

#include <tuple>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std;

  VERIFY( tuple_size<const tuple<> >::value == 0 );
  VERIFY( tuple_size<volatile tuple<int> >::value == 1 );
  VERIFY( tuple_size<const volatile tuple<void> >::value == 1 );

  typedef tuple<int, const int&, void> test_tuple1;
  VERIFY( tuple_size<const test_tuple1>::value == 3 );
  VERIFY( tuple_size<const volatile test_tuple1>::value == 3 );
  VERIFY( tuple_size<volatile tuple<tuple<void> > >::value == 1 );
}

int main()
{
  test01();
  return 0;
}

// LWG DR 2770. tuple_size<const T> specialization is not SFINAE compatible
template<typename T, typename = void>
struct has_value : std::false_type { };

template<typename T>
struct has_value<T, std::__void_t<decltype(T::value)>> : std::true_type { };

static_assert( !has_value<std::tuple_size<int>>::value, "" );
static_assert( !has_value<std::tuple_size<const int>>::value, "" );
