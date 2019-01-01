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

namespace net = std::experimental::net;

template<typename T>
bool is_mutable(const T&)
{ return std::is_same<T, net::mutable_buffer>::value; }

template<typename T>
bool is_const(const T&)
{ return std::is_same<T, net::const_buffer>::value; }

void
test01()
{
  bool test = false;

  auto b1 = net::buffer((void*)&test, sizeof(test));
  VERIFY( is_mutable(b1) );
  VERIFY( b1.data() == &test );
  VERIFY( b1.size() == sizeof(test) );

  auto b2 = net::buffer((const void*)&test, sizeof(test));
  VERIFY( is_const(b2) );
  VERIFY( b2.data() == &test );
  VERIFY( b1.size() == sizeof(test) );

  auto b3 = net::buffer(b1);
  VERIFY( is_mutable(b3) );
  VERIFY( b3.data() == b1.data() );
  VERIFY( b3.size() == b1.size() );

  auto b4 = net::buffer(b2);
  VERIFY( is_const(b4) );
  VERIFY( b4.data() == b2.data() );
  VERIFY( b4.size() == b2.size() );

  auto b5 = net::buffer(b1, 0);
  VERIFY( is_mutable(b5) );
  VERIFY( b5.data() == b1.data() );
  VERIFY( b5.size() == 0 );

  auto b6 = net::buffer(b2, 0);
  VERIFY( is_const(b6) );
  VERIFY( b6.data() == b2.data() );
  VERIFY( b6.size() == 0 );

  int a7[7];
  auto b7 = net::buffer(a7);
  VERIFY( is_mutable(b7) );
  VERIFY( b7.data() == a7 );
  VERIFY( b7.size() == sizeof(a7) );

  auto b7x = net::buffer(a7, 2);
  VERIFY( is_mutable(b7x) );
  VERIFY( b7x.data() == a7 );
  VERIFY( b7x.size() == sizeof(a7[0]) * 2 );

  const short a8[8] = { };
  auto b8 = net::buffer(a8);
  VERIFY( is_const(b8) );
  VERIFY( b8.data() == a8 );
  VERIFY( b8.size() == sizeof(a8) );

  auto b8x = net::buffer(a8, 3);
  VERIFY( is_const(b8x) );
  VERIFY( b8x.data() == a8 );
  VERIFY( b8x.size() == sizeof(a8[0]) * 3 );

  std::array<short, 9> a9;
  auto b9 = net::buffer(a9);
  VERIFY( is_mutable(b9) );
  VERIFY( b9.data() == a9.data() );
  VERIFY( b9.size() == sizeof(a9) );

  auto b9x = net::buffer(a9, 4);
  VERIFY( is_mutable(b9x) );
  VERIFY( b9x.data() == a9.data() );
  VERIFY( b9x.size() == sizeof(a9[0]) * 4 );

  const std::array<long long, 10> a10{};
  auto b10 = net::buffer(a10);
  VERIFY( is_const(b10) );
  VERIFY( b10.data() == a10.data() );
  VERIFY( b10.size() == sizeof(a10) );

  auto b10x = net::buffer(a10, 5);
  VERIFY( is_const(b10x) );
  VERIFY( b10x.data() == a10.data() );
  VERIFY( b10x.size() == sizeof(a10[0]) * 5 );

  std::array<const int, 11> a11{};
  auto b11 = net::buffer(a11);
  VERIFY( is_const(b11) );
  VERIFY( b11.data() == a11.data() );
  VERIFY( b11.size() == sizeof(a11) );

  auto b11x = net::buffer(a11, 6);
  VERIFY( is_const(b11x) );
  VERIFY( b11x.data() == a11.data() );
  VERIFY( b11x.size() == sizeof(a11[0]) * 6 );

  std::vector<short> a12(12);
  auto b12 = net::buffer(a12);
  VERIFY( is_mutable(b12) );
  VERIFY( b12.data() == a12.data() );
  VERIFY( b12.size() == sizeof(a12[0]) * a12.size() );

  auto b12x = net::buffer(a12, 7);
  VERIFY( is_mutable(b12x) );
  VERIFY( b12x.data() == a12.data() );
  VERIFY( b12x.size() == sizeof(a12[0]) * 7 );

  const std::vector<long long> a13(13);
  auto b13 = net::buffer(a13);
  VERIFY( is_const(b13) );
  VERIFY( b13.data() == a13.data() );
  VERIFY( b13.size() == sizeof(a13[0]) * a13.size() );

  auto b13x = net::buffer(a13, 7);
  VERIFY( is_const(b13x) );
  VERIFY( b13x.data() == a13.data() );
  VERIFY( b13x.size() == sizeof(a13[0]) * 7 );

  std::u32string a14(14, ' ');
  auto b14 = net::buffer(a14);
  VERIFY( is_mutable(b14) );
  VERIFY( b14.data() == a14.data() );
  VERIFY( b14.size() == sizeof(a14[0]) * a14.size() );

  auto b14x = net::buffer(a14, 8);
  VERIFY( is_mutable(b14x) );
  VERIFY( b14x.data() == a14.data() );
  VERIFY( b14x.size() == sizeof(a14[0]) * 8 );

  const std::u16string a15(15, ' ');
  auto b15 = net::buffer(std::experimental::u16string_view(a15));
  VERIFY( is_const(b15) );
  VERIFY( b15.data() == a15.data() );
  VERIFY( b15.size() == sizeof(a15[0]) * a15.size() );

  auto b15x = net::buffer(std::experimental::u16string_view(a15), 9);
  VERIFY( is_const(b15x) );
  VERIFY( b15x.data() == a15.data() );
  VERIFY( b15x.size() == sizeof(a15[0]) * 9 );
}

int
main()
{
  test01();
}
