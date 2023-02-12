// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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
// { dg-require-effective-target hosted }

#include <memory>
#include <testsuite_hooks.h>

template<typename T>
bool not_enabled(T& t)
{
#if __cpp_lib_enable_shared_from_this >= 201603
  return t.weak_from_this().expired();
#else
  try {
    t.shared_from_this();
    return false;
  } catch (const std::bad_weak_ptr&) {
    return true;
  }
#endif
}

struct A : std::enable_shared_from_this<A> { };
struct B : std::enable_shared_from_this<B> { };
struct D : A, B { };

void test01()
{
  auto d = std::make_shared<D>();
  VERIFY( not_enabled( static_cast<A&>(*d) ) );
  VERIFY( not_enabled( static_cast<const A&>(*d) ) );
  VERIFY( not_enabled( static_cast<B&>(*d) ) );
  VERIFY( not_enabled( static_cast<const B&>(*d) ) );
}

struct E : std::__enable_shared_from_this<E> { };
struct F : std::__enable_shared_from_this<F> { };
struct G : E, F { };

void test02()
{
  auto g = std::make_shared<G>();
  VERIFY( not_enabled( static_cast<E&>(*g) ) );
  VERIFY( not_enabled( static_cast<const E&>(*g) ) );
  VERIFY( not_enabled( static_cast<F&>(*g) ) );
  VERIFY( not_enabled( static_cast<const F&>(*g) ) );
}

struct H : D, G { };

void test03()
{
  auto h = std::make_shared<H>();
  VERIFY( not_enabled( static_cast<A&>(*h) ) );
  VERIFY( not_enabled( static_cast<const A&>(*h) ) );
  VERIFY( not_enabled( static_cast<B&>(*h) ) );
  VERIFY( not_enabled( static_cast<const B&>(*h) ) );
  VERIFY( not_enabled( static_cast<E&>(*h) ) );
  VERIFY( not_enabled( static_cast<const E&>(*h) ) );
  VERIFY( not_enabled( static_cast<F&>(*h) ) );
  VERIFY( not_enabled( static_cast<const F&>(*h) ) );
}

int
main()
{
  test01();
  test02();
  test03();
}
