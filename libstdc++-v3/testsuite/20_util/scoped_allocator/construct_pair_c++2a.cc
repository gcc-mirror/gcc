// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

// P0591R4 makes uses-allocator construction apply recursively for nested pairs

#include <scoped_allocator>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct X
{
  using allocator_type = __gnu_test::uneq_allocator<int>;

  X(int personality) : a(personality) { }
  X(std::allocator_arg_t, allocator_type a) : a(a) { }
  X(std::allocator_arg_t, allocator_type a, const X&) : a(a) { }

  allocator_type a;
};

void
test01()
{
  using value_type = std::pair<std::pair<X, int>, std::pair<int, X>>;
  using scoped_alloc
    = std::scoped_allocator_adaptor<__gnu_test::uneq_allocator<value_type>>;

  const scoped_alloc a(10);
  std::vector<value_type, scoped_alloc> v(a);
  VERIFY( v.get_allocator().get_personality() == a.get_personality() );

  value_type val( { X(1), 2 }, { 3, X(4) } );
  v.push_back(val);
  X& x1 = v.back().first.first;
  VERIFY( x1.a.get_personality() != val.first.first.a.get_personality() );
  VERIFY( x1.a.get_personality() == a.get_personality() );

  X& x2 = v.back().second.second;
  VERIFY( x2.a.get_personality() != val.second.second.a.get_personality() );
  VERIFY( x2.a.get_personality() == a.get_personality() );

  // Check other members of the pairs are correctly initialized too:
  VERIFY( v.back().first.second == val.first.second );
  VERIFY( v.back().second.first == val.second.first );
}

void
test02()
{
  using value_type = std::pair<std::pair<X, int>, std::pair<int, X>>;
  using scoped_alloc
    = std::scoped_allocator_adaptor<__gnu_test::uneq_allocator<value_type>,
				    X::allocator_type>;

  const scoped_alloc a(10, 20);
  std::vector<value_type, scoped_alloc> v(a);
  VERIFY( v.get_allocator().get_personality() == a.get_personality() );

  value_type val( { X(1), 2 }, { 3, X(4) } );
  v.push_back(val);
  X& x1 = v.back().first.first;
  VERIFY( x1.a.get_personality() != val.first.first.a.get_personality() );
  VERIFY( x1.a.get_personality() != a.get_personality() );
  VERIFY( x1.a.get_personality() == a.inner_allocator().get_personality() );

  X& x2 = v.back().second.second;
  VERIFY( x2.a.get_personality() != val.second.second.a.get_personality() );
  VERIFY( x2.a.get_personality() != a.get_personality() );
  VERIFY( x2.a.get_personality() == a.inner_allocator().get_personality() );

  // Check other members of the pairs are correctly initialized too:
  VERIFY( v.back().first.second == val.first.second );
  VERIFY( v.back().second.first == val.second.first );
}

int
main()
{
  test01();
  test02();
}
