// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }

#include <experimental/buffer>
#include <testsuite_hooks.h>

using std::experimental::net::mutable_buffer;

void
test01()
{
  using B = mutable_buffer;
  const B b;

  static_assert( std::is_nothrow_default_constructible<B>::value,
      "const_mutable is nothrow default constructible" );
  static_assert( std::is_copy_assignable<B>::value,
      "const_mutable is copy assignable" );
  static_assert( std::is_nothrow_constructible<B, void*, size_t>::value,
      "const_mutable is nothrow default constructible" );
  static_assert( std::is_same<decltype(b.data()), void*>::value,
      "data() return const void*" );
  static_assert( noexcept(b.data()),
      "data() is nothrow" );
  static_assert( std::is_same<decltype(b.size()), size_t>::value,
      "size() return size_t" );
  static_assert( noexcept(b.size()),
      "size() is nothrow" );
}

void
test02()
{
  bool test __attribute__((unused)) = false;
  char c[4];

  mutable_buffer b;
  VERIFY( b.data() == nullptr );
  VERIFY( b.size() == 0 );

  b = mutable_buffer(c, sizeof(c));
  VERIFY( b.data() == c );
  VERIFY( b.size() == sizeof(c) );

  b = mutable_buffer{};
  VERIFY( b.data() == nullptr );
  VERIFY( b.size() == 0 );
}

int
main()
{
  test01();
  test02();
}
