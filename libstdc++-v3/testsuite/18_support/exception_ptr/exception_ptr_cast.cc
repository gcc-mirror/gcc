// { dg-do run { target c++26 } }

// Copyright (C) 2025 Free Software Foundation, Inc.
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

// exception_ptr_cast.

#include <exception>
#include <testsuite_hooks.h>

#if __cpp_lib_exception_ptr_cast != 202506L
# error "__cpp_lib_exception_ptr_cast != 202506"
#endif

struct A { int a; };
struct B : A {};
struct C : B {};
struct D {};
struct E : virtual C { int e; constexpr virtual ~E () {} };
struct F : virtual E, virtual C { int f; };
struct G : virtual F, virtual C, virtual E {
  constexpr G () : g (4) { a = 1; e = 2; f = 3; } int g;
};

constexpr bool test01(bool x)
{
  auto a = std::make_exception_ptr(C{ 42 });
  auto b = std::exception_ptr_cast<C>(a);
  VERIFY( b != nullptr );
  VERIFY( b->a == 42 );
  auto c = std::exception_ptr_cast<B>(a);
  VERIFY( c == static_cast<const B*>(b) );
  auto d = std::exception_ptr_cast<A>(a);
  VERIFY( d == static_cast<const A*>(b) );
  auto e = std::exception_ptr_cast<D>(a);
  VERIFY( e == nullptr );
  auto f = std::make_exception_ptr(42L);
  auto g = std::exception_ptr_cast<long>(f);
  VERIFY( g != nullptr );
  VERIFY( *g == 42L );
  try
    {
      throw G ();
    }
  catch (...)
    {
      auto h = std::current_exception();
      auto i = std::exception_ptr_cast<G>(h);
      VERIFY( i != nullptr );
      VERIFY( i->a == 1 && i->e == 2 && i->f == 3 && i->g == 4 );
      auto j = std::exception_ptr_cast<A>(h);
      VERIFY( j == static_cast<const A*>(i) );
      auto k = std::exception_ptr_cast<C>(h);
      VERIFY( k == static_cast<const C*>(i) );
      auto l = std::exception_ptr_cast<E>(h);
      VERIFY( l == static_cast<const E*>(i) );
      auto m = std::exception_ptr_cast<F>(h);
      VERIFY( m == static_cast<const F*>(i) );
      auto n = std::exception_ptr_cast<G>(a);
      VERIFY( n == nullptr );
    }
  if (x)
    throw 1;
  return true;
}

#if _GLIBCXX_USE_CXX11_ABI
static_assert(test01(false));
#endif

int main()
{
  try
    {
      test01(true);
    }
  catch (...)
    {
    }
}
