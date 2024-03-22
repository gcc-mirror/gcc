// Copyright (C) 2008-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <sstream>
#include <string>
#include <testsuite_hooks.h>

void
test01()
{
  int i = 1742;
  std::ostringstream() << i;
  std::string result ("1742");
  int i2;
  std::istringstream(result) >> i2;
  VERIFY (i == i2);
}

struct X { bool as_rvalue; };

void operator>>(std::istream&, X& x) { x.as_rvalue = false; }
void operator>>(std::istream&, X&& x) { x.as_rvalue = true; }

// LWG 2328 Rvalue stream extraction should use perfect forwarding
void
test02()
{
  X x;
  std::istringstream is;
  auto&& ref1 = (std::move(is) >> x);
  VERIFY( &ref1 == &is );
  VERIFY( x.as_rvalue == false );
  auto&& ref2 = (std::move(is) >> std::move(x));
  VERIFY( &ref2 == &is );
  VERIFY( x.as_rvalue == true );

  char arr[2];
#if __cplusplus <= 201703L
  std::istringstream("x") >> &arr[0];
#endif
  std::istringstream("x") >> arr;
  VERIFY( std::string(arr) == "x" );
}

// LWG 1203 More useful rvalue stream insertion
void
test03()
{
  int i = 1203;
  std::string result = (std::ostringstream() << "i = " << i).str();
  VERIFY( result == "i = 1203" );

  std::ostringstream os;
  std::ostringstream&& ros = std::move(os) << result;
  VERIFY( &ros == &os );
  VERIFY( ros.str() == result );

  std::stringstream ss;
  std::stringstream&& rss = std::move(ss) << result;
  VERIFY( &rss == &ss );
  VERIFY( rss.str() == result );

  std::istringstream is("first second third");
  std::istringstream&& ris = std::move(is) >> result;
  VERIFY( &ris == &is );
  VERIFY( result == "first" );

  std::stringstream ss2("fourth fifth sixth");
  std::stringstream&& rss2 = std::move(ss2) >> result;
  VERIFY( &rss2 == &ss2 );
  VERIFY( result == "fourth" );
}

struct A { friend void operator<<(std::ios_base&, A) { } };

struct O : private std::ios_base { friend void operator<<(O&, int) { } };

template<typename Ostream, typename T, typename = void>
  struct is_insertable
  : std::false_type
  { };

template<typename> using void_t = void;

template<typename Ostream, typename T>
  using insert_result
    = decltype(std::declval<Ostream>() << std::declval<const T&>());

template<typename Ostream, typename T>
  struct is_insertable<Ostream, T, void_t<insert_result<Ostream, T>>>
  : std::true_type
  { };

// LWG 1203 negative tests
void
test04()
{
  static_assert( is_insertable<std::ios_base&, A>::value,
      "valid using the friend operator<<" );
  static_assert( !is_insertable<std::ios_base&&, A>::value,
      "ill-formed because ios_base is not derived from ios_base" );

  static_assert( is_insertable<O&, int>::value,
      "valid using the friend operator<<" );
  static_assert( !is_insertable<O&&, int>::value,
      "ill-formed because O is not publicly derived from ios_base" );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
