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

#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using test_allocator = __gnu_test::uneq_allocator<int>;

struct Arg { };

struct A
{
  A() : nargs(0) { }
  A(float&) : nargs(1) { }
  A(int, void*) : nargs(2) { }

  // These should not be used:
  A(const test_allocator& a);
  A(float&, const test_allocator& a);
  A(int, void*, const test_allocator& a);

  const int nargs;
  const int alloc_id = -1;

  // std::uses_allocator<A, test_allocator> should be false:
  using allocator_type = void*();
};

struct B
{
  // This means std::uses_allocator<B, test_allocator> is true:
  using allocator_type = test_allocator;

  B() : nargs(0) { }
  B(float&) : nargs(1) { }
  B(int, void*) : nargs(2) { }

  B(std::allocator_arg_t, const test_allocator& a)
  : nargs(0), alloc_id(a.get_personality()) { }
  B(std::allocator_arg_t, const test_allocator& a, float&)
  : nargs(1), alloc_id(a.get_personality()) { }
  B(std::allocator_arg_t, const test_allocator& a, int, void*)
  : nargs(2), alloc_id(a.get_personality()) { }
  B(std::allocator_arg_t, const test_allocator& a, B&& b)
  : nargs(b.nargs), alloc_id(a.get_personality()) { }

  // These should not be used:
  B(const test_allocator&);
  B(float&, const test_allocator&, float&);
  B(int, void*, const test_allocator&);
  B(const test_allocator&, float&);
  B(const test_allocator&, int, void*);
  B(B&&);
  B(B&&, const test_allocator&);

  const int nargs;
  const int alloc_id = -1;
};

struct C
{
  C() : nargs(0) { }
  C(float&) : nargs(1) { }
  C(int, void*) : nargs(2) { }

  C(const test_allocator& a)
  : nargs(0), alloc_id(a.get_personality()) { }
  C(float&, const test_allocator& a)
  : nargs(1), alloc_id(a.get_personality()) { }
  C(int, void*, const test_allocator& a)
  : nargs(2), alloc_id(a.get_personality()) { }
  C(C&& c, const test_allocator& a)
  : nargs(c.nargs), alloc_id(a.get_personality()) { }

  C(C&&);

  const int nargs;
  const int alloc_id = -1;
};

namespace std {
  // This means std::uses_allocator<C, test_allocator> is true:
  template<> struct uses_allocator<C, test_allocator> : std::true_type { };
}

test_allocator alloc1(1);
test_allocator alloc2(2);

void
test01()
{
  auto i0 = std::make_obj_using_allocator<int>(alloc1, 2);
  VERIFY( i0 == 2 );

  float f = 0.0f;

  auto a0 = std::make_obj_using_allocator<A>(alloc1);
  VERIFY( a0.nargs == 0 );
  VERIFY( a0.alloc_id == -1 );
  auto a1 = std::make_obj_using_allocator<A>(alloc1, f);
  VERIFY( a1.nargs == 1 );
  VERIFY( a1.alloc_id == -1 );
  auto a2 = std::make_obj_using_allocator<A>(alloc1, 123, nullptr);
  VERIFY( a2.nargs == 2 );
  VERIFY( a2.alloc_id == -1 );

  auto b0 = std::make_obj_using_allocator<B>(alloc1);
  VERIFY( b0.nargs == 0 );
  VERIFY( b0.alloc_id == 1 );
  auto b1 = std::make_obj_using_allocator<B>(alloc2, f);
  VERIFY( b1.nargs == 1 );
  VERIFY( b1.alloc_id == 2 );
  auto b2 = std::make_obj_using_allocator<B>(alloc1, 123, nullptr);
  VERIFY( b2.nargs == 2 );
  VERIFY( b2.alloc_id == 1 );

  auto c0 = std::make_obj_using_allocator<C>(alloc1);
  VERIFY( c0.nargs == 0 );
  VERIFY( c0.alloc_id == 1 );
  auto c1 = std::make_obj_using_allocator<C>(alloc2, f);
  VERIFY( c1.nargs == 1 );
  VERIFY( c1.alloc_id == 2 );
  auto c2 = std::make_obj_using_allocator<C>(alloc1, 123, nullptr);
  VERIFY( c2.nargs == 2 );
  VERIFY( c2.alloc_id == 1 );
}

void
test02()
{
  decltype(auto) b
    = std::make_obj_using_allocator<const B>(alloc1, 123, nullptr);
  static_assert( std::is_const_v<decltype(b)> );
  VERIFY( b.nargs == 2 );
  VERIFY( b.alloc_id == 1 );

  decltype(auto) c = std::make_obj_using_allocator<const C>(alloc1);
  static_assert( std::is_const_v<decltype(c)> );
  VERIFY( c.nargs == 0 );
  VERIFY( c.alloc_id == 1 );
}

void
test03()
{
  B b;
  decltype(auto) ref = std::make_obj_using_allocator<B&>(alloc1, b);
  static_assert( std::is_same_v<decltype(ref), B&> );
  VERIFY( &ref == &b );
  VERIFY( ref.nargs == 0 );
  VERIFY( ref.alloc_id == -1 );
  const B& cref = std::make_obj_using_allocator<const B&>(alloc1, b);
  static_assert( std::is_same_v<decltype(cref), const B&> );
  VERIFY( &cref == &b );
  VERIFY( cref.nargs == 0 );
  VERIFY( cref.alloc_id == -1 );
}

void
test04()
{
  struct D
  {
    D(std::allocator_arg_t) { }
    D(std::allocator_arg_t, int) { }

    // These should not be used:
    D(std::allocator_arg_t, const test_allocator&);
    D(std::allocator_arg_t, const test_allocator&, int);

    ~D() { }
  };

  D d1 = std::make_obj_using_allocator<D>(alloc1, std::allocator_arg);

  struct E
  {
    using allocator_type = test_allocator;

    E(std::allocator_arg_t, const test_allocator&) { }
    E(std::allocator_arg_t, int, const test_allocator&) { }

    // These should not be used:
    E(std::allocator_arg_t);
    E(std::allocator_arg_t, int);

    ~E() { }
  };

  E e1 = std::make_obj_using_allocator<E>(alloc1, std::allocator_arg);
  E e2 = std::make_obj_using_allocator<E>(alloc2, std::allocator_arg, 2);
}

void
test05()
{
  using std::pair;
  std::piecewise_construct_t p;
  std::tuple<> t0;
  float f = 0.0f;
  std::tuple<float&> t1(f);
  std::tuple<int, void*> t2{};

  auto aa00 = std::make_obj_using_allocator<pair<A, A>>(alloc1, p, t0, t0);
  VERIFY( aa00.first.nargs == 0 );
  VERIFY( aa00.first.alloc_id == -1 );
  VERIFY( aa00.second.nargs == 0 );
  VERIFY( aa00.second.alloc_id == -1 );
  auto ab00 = std::make_obj_using_allocator<pair<A, B>>(alloc1, p, t0, t0);
  VERIFY( ab00.first.nargs == 0 );
  VERIFY( ab00.first.alloc_id == -1 );
  VERIFY( ab00.second.nargs == 0 );
  VERIFY( ab00.second.alloc_id == 1 );
  auto bc00 = std::make_obj_using_allocator<pair<B, C>>(alloc2, p, t0, t0);
  VERIFY( bc00.first.nargs == 0 );
  VERIFY( bc00.first.alloc_id == 2 );
  VERIFY( bc00.second.nargs == 0 );
  VERIFY( bc00.second.alloc_id == 2 );
  auto cb00 = std::make_obj_using_allocator<pair<C, B>>(alloc2, p, t0, t0);
  VERIFY( cb00.first.nargs == 0 );
  VERIFY( cb00.first.alloc_id == 2 );
  VERIFY( cb00.second.nargs == 0 );
  VERIFY( cb00.second.alloc_id == 2 );
  auto cc00
    = std::make_obj_using_allocator<pair<C, const C>>(alloc1, p, t0, t0);
  VERIFY( cc00.first.nargs == 0 );
  VERIFY( cc00.first.alloc_id == 1 );
  VERIFY( cc00.second.nargs == 0 );
  VERIFY( cc00.second.alloc_id == 1 );

  auto aa21 = std::make_obj_using_allocator<pair<A, A>>(alloc1, p, t2, t1);
  VERIFY( aa21.first.nargs == 2 );
  VERIFY( aa21.first.alloc_id == -1 );
  VERIFY( aa21.second.nargs == 1 );
  VERIFY( aa21.second.alloc_id == -1 );
  auto ab21 = std::make_obj_using_allocator<pair<A, B>>(alloc1, p, t2, t1);
  VERIFY( ab21.first.nargs == 2 );
  VERIFY( ab21.first.alloc_id == -1 );
  VERIFY( ab21.second.nargs == 1 );
  VERIFY( ab21.second.alloc_id == 1 );
  auto bc11 = std::make_obj_using_allocator<pair<B, C>>(alloc2, p, t1, t1);
  VERIFY( bc11.first.nargs == 1 );
  VERIFY( bc11.first.alloc_id == 2 );
  VERIFY( bc11.second.nargs == 1 );
  VERIFY( bc11.second.alloc_id == 2 );
  auto cb12 = std::make_obj_using_allocator<pair<C, B>>(alloc2, p, t1, t2);
  VERIFY( cb12.first.nargs == 1 );
  VERIFY( cb12.first.alloc_id == 2 );
  VERIFY( cb12.second.nargs == 2 );
  VERIFY( cb12.second.alloc_id == 2 );
  auto cc22
    = std::make_obj_using_allocator<pair<C, const C>>(alloc1, p, t2, t1);
  VERIFY( cc22.first.nargs == 2 );
  VERIFY( cc22.first.alloc_id == 1 );
  VERIFY( cc22.second.nargs == 1 );
  VERIFY( cc22.second.alloc_id == 1 );
}

void
test06()
{
  using std::pair;
  float f = 0.0f;

  auto aa00 = std::make_obj_using_allocator<pair<A, A>>(alloc1);
  VERIFY( aa00.first.nargs == 0 );
  VERIFY( aa00.first.alloc_id == -1 );
  VERIFY( aa00.second.nargs == 0 );
  VERIFY( aa00.second.alloc_id == -1 );
  auto ab00 = std::make_obj_using_allocator<pair<A, B>>(alloc1);
  VERIFY( ab00.first.nargs == 0 );
  VERIFY( ab00.first.alloc_id == -1 );
  VERIFY( ab00.second.nargs == 0 );
  VERIFY( ab00.second.alloc_id == 1 );
  auto bc00 = std::make_obj_using_allocator<pair<B, C>>(alloc2);
  VERIFY( bc00.first.nargs == 0 );
  VERIFY( bc00.first.alloc_id == 2 );
  VERIFY( bc00.second.nargs == 0 );
  VERIFY( bc00.second.alloc_id == 2 );
  auto cb00 = std::make_obj_using_allocator<pair<C, B>>(alloc2);
  VERIFY( cb00.first.nargs == 0 );
  VERIFY( cb00.first.alloc_id == 2 );
  VERIFY( cb00.second.nargs == 0 );
  VERIFY( cb00.second.alloc_id == 2 );
  auto cc00 = std::make_obj_using_allocator<pair<C, const C>>(alloc1);
  VERIFY( cc00.first.nargs == 0 );
  VERIFY( cc00.first.alloc_id == 1 );
  VERIFY( cc00.second.nargs == 0 );
  VERIFY( cc00.second.alloc_id == 1 );

  auto aa11 = std::make_obj_using_allocator<pair<A, A>>(alloc1, f, f);
  VERIFY( aa11.first.nargs == 1 );
  VERIFY( aa11.first.alloc_id == -1 );
  VERIFY( aa11.second.nargs == 1 );
  VERIFY( aa11.second.alloc_id == -1 );
  auto aba1 = std::make_obj_using_allocator<pair<A, B>>(alloc1, A{}, f);
  VERIFY( aba1.first.nargs == 0 );
  VERIFY( aba1.first.alloc_id == -1 );
  VERIFY( aba1.second.nargs == 1 );
  VERIFY( aba1.second.alloc_id == 1 );
  auto bc11 = std::make_obj_using_allocator<pair<B, C>>(alloc2, f, f);
  VERIFY( bc11.first.nargs == 1 );
  VERIFY( bc11.first.alloc_id == 2 );
  VERIFY( bc11.second.nargs == 1 );
  VERIFY( bc11.second.alloc_id == 2 );
  auto cb1b = std::make_obj_using_allocator<pair<C, B>>(alloc2, f, B{});
  VERIFY( cb1b.first.nargs == 1 );
  VERIFY( cb1b.first.alloc_id == 2 );
  VERIFY( cb1b.second.nargs == 0 );
  VERIFY( cb1b.second.alloc_id == 2 );
  auto cccc
    = std::make_obj_using_allocator<pair<C, const C>>(alloc1, C{}, C{});
  VERIFY( cccc.first.nargs == 0 );
  VERIFY( cccc.first.alloc_id == 1 );
  VERIFY( cccc.second.nargs == 0 );
  VERIFY( cccc.second.alloc_id == 1 );

  pair<float&, A> p1a(f, A{});
  pair<float&, float&> p11(f, f);
  auto aa1a = std::make_obj_using_allocator<pair<A, A>>(alloc1, p1a);
  VERIFY( aa1a.first.nargs == 1 );
  VERIFY( aa1a.first.alloc_id == -1 );
  VERIFY( aa1a.second.nargs == 0 );
  VERIFY( aa1a.second.alloc_id == -1 );
  auto ab11 = std::make_obj_using_allocator<pair<A, B>>(alloc1, p11);
  VERIFY( ab11.first.nargs == 1 );
  VERIFY( ab11.first.alloc_id == -1 );
  VERIFY( ab11.second.nargs == 1 );
  VERIFY( ab11.second.alloc_id == 1 );
  auto cb11 = std::make_obj_using_allocator<pair<C, B>>(alloc2, p11);
  VERIFY( cb11.first.nargs == 1 );
  VERIFY( cb11.first.alloc_id == 2 );
  VERIFY( cb11.second.nargs == 1 );
  VERIFY( cb11.second.alloc_id == 2 );

  auto bcbc = std::make_obj_using_allocator<pair<B, C>>(alloc2, pair<B, C>());
  VERIFY( bcbc.first.nargs == 0 );
  VERIFY( bcbc.first.alloc_id == 2 );
  VERIFY( bcbc.second.nargs == 0 );
  VERIFY( bcbc.second.alloc_id == 2 );

  auto cc11 = std::make_obj_using_allocator<pair<C, B>>(alloc2, std::move(p11));
  VERIFY( cc11.first.nargs == 1 );
  VERIFY( cc11.first.alloc_id == 2 );
  VERIFY( cc11.second.nargs == 1 );
  VERIFY( cc11.second.alloc_id == 2 );
}

void
test07()
{
  using nested_pair = std::pair<const std::pair<B, const B>, C>;
  auto p = std::make_obj_using_allocator<const nested_pair>(alloc1);
  VERIFY( p.first.first.alloc_id == 1 );
  VERIFY( p.first.second.alloc_id == 1 );
  VERIFY( p.second.alloc_id == 1 );
}

void
test08()
{
  // LWG DR 3187.
  // P0591R4 reverted DR 2586 fixes to scoped_allocator_adaptor::construct()

  struct X {
    using allocator_type = std::allocator<X>;
    X(std::allocator_arg_t, allocator_type&&) { }
    X(const allocator_type&) { }
  };

  std::allocator<X> a;
  std::make_obj_using_allocator<X>(a);
}

constexpr bool
test_pr104542()
{
  // PR libstdc++/104542 - missing constexpr
  std::allocator<void> a;
  int i = std::make_obj_using_allocator<int>(a, 1);

  struct X {
    using allocator_type = std::allocator<long>;
    constexpr X(std::allocator_arg_t, std::allocator<int>, int i) : i(i+1) { }
    int i;
  };

  X x = std::make_obj_using_allocator<X>(a, i);

  struct Y {
    using allocator_type = std::allocator<char>;
    constexpr Y(X x, std::allocator<int>) : i(x.i+1) { }
    int i;
  };

  Y y = std::make_obj_using_allocator<Y>(a, x);

  return y.i == 3;
}

static_assert( test_pr104542() );

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
}
