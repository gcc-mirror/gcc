// { dg-options "-std=gnu++17" }
// { dg-do run }

// Copyright (C) 2016 Free Software Foundation, Inc.
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

#include <variant>
#include <string>
#include <vector>
#include <unordered_set>
#include <testsuite_hooks.h>

using namespace std;

struct AlwaysThrow
{
  AlwaysThrow() = default;

  AlwaysThrow(const AlwaysThrow&)
  { throw nullptr; }

  AlwaysThrow(AlwaysThrow&&)
  { throw nullptr; }

  AlwaysThrow& operator=(const AlwaysThrow&)
  {
    throw nullptr;
    return *this;
  }

  AlwaysThrow& operator=(AlwaysThrow&&)
  {
    throw nullptr;
    return *this;
  }
};

void default_ctor()
{
  bool test [[gnu::unused]] = true;

  variant<monostate, string> v;
  VERIFY(holds_alternative<monostate>(v));
}

void copy_ctor()
{
  bool test [[gnu::unused]] = true;

  variant<monostate, string> v("a");
  VERIFY(holds_alternative<string>(v));
  variant<monostate, string> u(v);
  VERIFY(holds_alternative<string>(u));
  VERIFY(get<string>(u) == "a");
}

void move_ctor()
{
  bool test [[gnu::unused]] = true;

  variant<monostate, string> v("a");
  VERIFY(holds_alternative<string>(v));
  variant<monostate, string> u(std::move(v));
  VERIFY(holds_alternative<string>(u));
  VERIFY(get<string>(u) == "a");
  VERIFY(holds_alternative<string>(v));
}

void arbitrary_ctor()
{
  bool test [[gnu::unused]] = true;

  variant<int, string> v("a");
  VERIFY(holds_alternative<string>(v));
  VERIFY(get<1>(v) == "a");
}

void copy_assign()
{
  bool test [[gnu::unused]] = true;

  variant<monostate, string> v("a");
  VERIFY(holds_alternative<string>(v));
  variant<monostate, string> u;
  u = v;
  VERIFY(holds_alternative<string>(u));
  VERIFY(get<string>(u) == "a");
}

void move_assign()
{
  bool test [[gnu::unused]] = true;

  variant<monostate, string> v("a");
  VERIFY(holds_alternative<string>(v));
  variant<monostate, string> u;
  u = std::move(v);
  VERIFY(holds_alternative<string>(u));
  VERIFY(get<string>(u) == "a");
  VERIFY(holds_alternative<string>(v));
}

void arbitrary_assign()
{
  bool test [[gnu::unused]] = true;

  variant<int, string> v;
  v = "a";

  VERIFY(holds_alternative<string>(variant<int, string>("a")));
  VERIFY(get<1>(v) == "a");
}

void dtor()
{
  bool test [[gnu::unused]] = true;

  struct A {
      A(int& called) : called(called) {}
      ~A() {
	  called++;
      }
      int& called;
  };
  {
    int called = 0;
    { variant<string, A> a(in_place<1>, called); }
    VERIFY(called == 1);
  }
  {
    int called = 0;
    { variant<string, A> a(in_place<0>); }
    VERIFY(called == 0);
  }
}

void in_place_index_ctor()
{
  bool test [[gnu::unused]] = true;

  {
    variant<int, string> v(in_place<1>, "a");
    VERIFY(holds_alternative<string>(v));
    VERIFY(get<1>(v) == "a");
  }
  {
    variant<int, string> v(in_place<1>, {'a', 'b'});
    VERIFY(holds_alternative<string>(v));
    VERIFY(get<1>(v) == "ab");
  }
}

void in_place_type_ctor()
{
  bool test [[gnu::unused]] = true;

  {
    variant<int, string> v(in_place<string>, "a");
    VERIFY(holds_alternative<string>(v));
    VERIFY(get<1>(v) == "a");
  }
  {
    variant<int, string> v(in_place<string>, {'a', 'b'});
    VERIFY(holds_alternative<string>(v));
    VERIFY(get<1>(v) == "ab");
  }
}

struct UsesAllocatable
{
  template<typename Alloc>
    UsesAllocatable(std::allocator_arg_t, const Alloc& a)
    : d(0), a(static_cast<const void*>(&a)) { }

  template<typename Alloc>
    UsesAllocatable(std::allocator_arg_t, const Alloc& a, const UsesAllocatable&)
    : d(1), a(static_cast<const void*>(&a)) { }

  template<typename Alloc>
    UsesAllocatable(std::allocator_arg_t, const Alloc& a, UsesAllocatable&&)
    : d(2), a(static_cast<const void*>(&a)) { }

  int d;
  const void* a;
};

namespace std
{
  template<>
    struct uses_allocator<UsesAllocatable, std::allocator<char>> : true_type { };
}

void uses_allocator_ctor()
{
  bool test [[gnu::unused]] = true;

  std::allocator<char> a;
  variant<UsesAllocatable> v(std::allocator_arg, a);
  VERIFY(get<0>(v).d == 0);
  VERIFY(get<0>(v).a == &a);
  {
    variant<UsesAllocatable> u(std::allocator_arg, a, v);
    VERIFY(get<0>(u).d == 1);
    VERIFY(get<0>(u).a == &a);
  }
  {
    variant<UsesAllocatable> u(std::allocator_arg, a, std::move(v));
    VERIFY(get<0>(u).d == 2);
    VERIFY(get<0>(u).a == &a);
  }
}

void emplace()
{
  bool test [[gnu::unused]] = true;

  variant<int, string> v;
  v.emplace<0>(1);
  VERIFY(get<0>(v) == 1);
  v.emplace<string>("a");
  VERIFY(get<string>(v) == "a");
  v.emplace<1>({'a', 'b'});
  VERIFY(get<1>(v) == "ab");
  v.emplace<string>({'a', 'c'});
  VERIFY(get<string>(v) == "ac");
  {
    variant<int, AlwaysThrow> v;
    AlwaysThrow a;
    try { v.emplace<1>(a); } catch (nullptr_t) { }
    VERIFY(v.valueless_by_exception());
  }
  {
    variant<int, AlwaysThrow> v;
    try { v.emplace<1>(AlwaysThrow{}); } catch (nullptr_t) { }
    VERIFY(v.valueless_by_exception());
  }
}

void test_get()
{
  bool test [[gnu::unused]] = true;

  VERIFY(get<1>(variant<int, string>("a")) == "a");
  VERIFY(get<string>(variant<int, string>("a")) == "a");
  {
    bool caught = false;

    try
      {
	get<0>(variant<int, string>("a"));
      }
    catch (const bad_variant_access&)
      {
	caught = true;
      }
    VERIFY(caught);
  }
  {
    bool caught = false;

    try
      {
	get<int>(variant<int, string>("a"));
      }
    catch (const bad_variant_access&)
      {
	caught = true;
      }
    VERIFY(caught);
  }
}

void test_relational()
{
  bool test [[gnu::unused]] = true;

  VERIFY((variant<int, string>(2) < variant<int, string>(3)));
  VERIFY((variant<int, string>(3) == variant<int, string>(3)));
  VERIFY((variant<int, string>(3) > variant<int, string>(2)));
  VERIFY((variant<int, string>(3) <= variant<int, string>(3)));
  VERIFY((variant<int, string>(2) <= variant<int, string>(3)));
  VERIFY((variant<int, string>(3) >= variant<int, string>(3)));
  VERIFY((variant<int, string>(3) >= variant<int, string>(2)));
  VERIFY((variant<int, string>(2) != variant<int, string>(3)));

  VERIFY((variant<int, string>(2) < variant<int, string>("a")));
  VERIFY((variant<string, int>(2) > variant<string, int>("a")));
}

void test_swap()
{
  bool test [[gnu::unused]] = true;

  variant<int, string> a("a"), b("b");
  a.swap(b);
  VERIFY(get<1>(a) == "b");
  VERIFY(get<1>(b) == "a");
  swap(a, b);
  VERIFY(get<1>(a) == "a");
  VERIFY(get<1>(b) == "b");
}

void test_visit()
{
  bool test [[gnu::unused]] = true;

  {
    struct Visitor
    {
      int operator()(int, float) {
	  return 0;
      }
      int operator()(int, double) {
	  return 1;
      }
      int operator()(char, float) {
	  return 2;
      }
      int operator()(char, double) {
	  return 3;
      }
      int operator()(int, float) const {
	  return 5;
      }
      int operator()(int, double) const {
	  return 6;
      }
      int operator()(char, float) const {
	  return 7;
      }
      int operator()(char, double) const {
	  return 8;
      }
    } visitor1;
    VERIFY(visit(visitor1, variant<int, char>(1), variant<float, double>(1.0f)) == 0);
    VERIFY(visit(visitor1, variant<int, char>(1), variant<float, double>(1.0)) == 1);
    VERIFY(visit(visitor1, variant<int, char>('a'), variant<float, double>(1.0f)) == 2);
    VERIFY(visit(visitor1, variant<int, char>('a'), variant<float, double>(1.0)) == 3);

    const auto& visitor2 = visitor1;
    VERIFY(visit(visitor2, variant<int, char>(1), variant<float, double>(1.0f)) == 5);
    VERIFY(visit(visitor2, variant<int, char>(1), variant<float, double>(1.0)) == 6);
    VERIFY(visit(visitor2, variant<int, char>('a'), variant<float, double>(1.0f)) == 7);
    VERIFY(visit(visitor2, variant<int, char>('a'), variant<float, double>(1.0)) == 8);
  }

  {
    struct Visitor
    {
      int operator()(int, float) && {
	  return 0;
      }
      int operator()(int, double) && {
	  return 1;
      }
      int operator()(char, float) && {
	  return 2;
      }
      int operator()(char, double) && {
	  return 3;
      }
    };
    VERIFY(visit(Visitor{}, variant<int, char>(1), variant<float, double>(1.0f)) == 0);
    VERIFY(visit(Visitor{}, variant<int, char>(1), variant<float, double>(1.0)) == 1);
    VERIFY(visit(Visitor{}, variant<int, char>('a'), variant<float, double>(1.0f)) == 2);
    VERIFY(visit(Visitor{}, variant<int, char>('a'), variant<float, double>(1.0)) == 3);
  }
}

void test_hash()
{
  bool test [[gnu::unused]] = true;

  unordered_set<variant<int, string>> s;
  VERIFY(s.emplace(3).second);
  VERIFY(s.emplace("asdf").second);
  VERIFY(s.emplace().second);
  VERIFY(s.size() == 3);
  VERIFY(!s.emplace(3).second);
  VERIFY(!s.emplace("asdf").second);
  VERIFY(!s.emplace().second);
  VERIFY(s.size() == 3);
  {
    struct A
    {
      operator int()
      {
        throw nullptr;
      }
    };
    variant<int, string> v;
    try
      {
        v.emplace<0>(A{});
      }
    catch (nullptr_t)
      {
      }
    VERIFY(v.valueless_by_exception());
    VERIFY(s.insert(v).second);
    VERIFY(s.size() == 4);
    VERIFY(!s.insert(v).second);
  }
}

void test_valueless_by_exception()
{
  bool test [[gnu::unused]] = true;

  {
    AlwaysThrow a;
    bool caught = false;
    try
      {
	variant<int, AlwaysThrow> v(a);
      }
    catch (nullptr_t)
      {
	caught = true;
      }
    VERIFY(caught);
  }
  {
    AlwaysThrow a;
    bool caught = false;
    try
      {
	variant<int, AlwaysThrow> v(a);
      }
    catch (nullptr_t)
      {
	caught = true;
      }
    VERIFY(caught);
  }
  {
    variant<int, AlwaysThrow> v;
    bool caught = false;
    try
      {
	AlwaysThrow a;
	v = a;
      }
    catch (nullptr_t)
      {
	caught = true;
      }
    VERIFY(caught);
    VERIFY(v.valueless_by_exception());
  }
  {
    variant<int, AlwaysThrow> v;
    bool caught = false;
    try
      {
	v = AlwaysThrow{};
      }
    catch (nullptr_t)
      {
	caught = true;
      }
    VERIFY(caught);
    VERIFY(v.valueless_by_exception());
  }
}

int main()
{
  default_ctor();
  copy_ctor();
  move_ctor();
  arbitrary_ctor();
  in_place_index_ctor();
  in_place_type_ctor();
  uses_allocator_ctor();
  copy_assign();
  move_assign();
  arbitrary_assign();
  dtor();
  emplace();
  test_get();
  test_relational();
  test_swap();
  test_visit();
  test_hash();
  test_valueless_by_exception();
}
