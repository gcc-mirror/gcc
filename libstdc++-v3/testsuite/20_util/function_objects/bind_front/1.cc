// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <functional>
#include <testsuite_hooks.h>

#ifndef __cpp_lib_bind_front
# error "Feature test macro for bind_front is missing"
#elif __cpp_lib_bind_front < 201902L
# error "Feature test macro for bind_front has wrong value"
#endif

using std::bind_front;
using std::is_same_v;
using std::is_invocable_v;
using std::is_invocable_r_v;

void
test01()
{
  struct F { void operator()() {} };

  // Arguments should be decayed:
  static_assert(std::is_same_v<
      decltype(bind_front(std::declval<F>(), std::declval<int>())),
      decltype(bind_front(std::declval<F&>(), std::declval<int&>()))
      >);
  static_assert(std::is_same_v<
      decltype(bind_front(std::declval<F>(), std::declval<int>())),
      decltype(bind_front(std::declval<const F&>(), std::declval<const int&>()))
      >);

  // Reference wrappers should be handled:
  static_assert(!std::is_same_v<
      decltype(bind_front(std::declval<F>(), std::declval<int&>())),
      decltype(bind_front(std::declval<F>(), std::ref(std::declval<int&>())))
      >);
  static_assert(!std::is_same_v<
      decltype(bind_front(std::declval<F>(), std::declval<const int&>())),
      decltype(bind_front(std::declval<F>(), std::cref(std::declval<int&>())))
      >);
  static_assert(!std::is_same_v<
      decltype(bind_front(std::declval<F>(), std::ref(std::declval<int&>()))),
      decltype(bind_front(std::declval<F>(), std::cref(std::declval<int&>())))
      >);
}

void
test02()
{
  struct quals
  {
    bool as_const;
    bool as_lvalue;
  };

  struct F
  {
    quals operator()() & { return { false, true }; }
    quals operator()() const & { return { true, true }; }
    quals operator()() && { return { false, false }; }
    quals operator()() const && { return { true, false }; }
  };

  F f;
  auto g = bind_front(f);
  const auto& cg = g;
  quals q;

  // constness and value category should be forwarded to the target object:
  q = g();
  VERIFY( ! q.as_const && q.as_lvalue );
  q = std::move(g)();
  VERIFY( ! q.as_const && ! q.as_lvalue );
  q = cg();
  VERIFY( q.as_const && q.as_lvalue );
  q = std::move(cg)();
  VERIFY( q.as_const && ! q.as_lvalue );
}

void
test03()
{
  struct F
  {
    int& operator()(int& i, void*) { return i; }
    void* operator()(int, void* p) const { return p; }
  };

  int i = 5;
  void* vp = &vp; // arbitrary void* value

  auto g1 = bind_front(F{}, i); // call wrapper has bound arg of type int
  using G1 = decltype(g1);
  // Invoking G1& will pass g1's bound arg as int&, so calls first overload:
  static_assert(is_invocable_r_v<int&, G1&, void*>);
  // Invoking const G1& or G&& calls second overload:
  static_assert(is_invocable_r_v<void*, const G1&, void*>);
  static_assert(is_invocable_r_v<void*, G1&&, void*>);
  void* p1 = static_cast<G1&&>(g1)(vp);
  VERIFY( p1 == vp );

  auto g2 = bind_front(F{}, std::ref(i)); // bound arg of type int&
  using G2 = decltype(g2);
  // Bound arg always forwarded as int& even from G2&& or const G2&
  static_assert(is_invocable_r_v<int&, G2&, void*>);
  static_assert(is_invocable_r_v<int&, G2&&, void*>);
  // But cannot call first overload on const G2:
  static_assert(is_invocable_r_v<void*, const G2&, void*>);
  static_assert(is_invocable_r_v<void*, const G2&&, void*>);
  int& i2 = g2(vp);
  VERIFY( &i2 == &i );
  int& i2r = static_cast<G2&&>(g2)(vp);
  VERIFY( &i2r == &i );
  void* p2 = const_cast<const G2&>(g2)(vp);
  VERIFY( p2 == vp );

  auto g3 = bind_front(F{}, std::cref(i)); // bound arg of type const int&
  using G3 = decltype(g3);
  // Bound arg always forwarded as const int& so can only call second overload:
  static_assert(is_invocable_r_v<void*, G3&, void*>);
  static_assert(is_invocable_r_v<void*, G3&&, void*>);
  static_assert(is_invocable_r_v<void*, const G3&, void*>);
  static_assert(is_invocable_r_v<void*, const G3&&, void*>);

  auto g4 = bind_front(g2, nullptr);
  using G4 = decltype(g4);
  static_assert(is_invocable_r_v<int&, G4&>);
  static_assert(is_invocable_r_v<int&, G4&&>);
  static_assert(is_invocable_r_v<void*, const G4&>);
  static_assert(is_invocable_r_v<void*, const G4&&>);
}

int f(int i, int j, int k) { return i + j + k; }

void
test04()
{
  auto g = bind_front(f);
  VERIFY( g(1, 2, 3) == 6 );
  auto g1 = bind_front(f, 1);
  VERIFY( g1(2, 3) == 6 );
  VERIFY( bind_front(g, 1)(2, 3) == 6 );
  auto g2 = bind_front(f, 1, 2);
  VERIFY( g2(3) == 6 );
  VERIFY( bind_front(g1, 2)(3) == 6 );
  auto g3 = bind_front(f, 1, 2, 3);
  VERIFY( g3() == 6 );
  VERIFY( bind_front(g2, 3)() == 6 );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
