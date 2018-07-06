// { dg-do run { target c++11 } }

// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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

// 20.4.2.1 [tuple.cnstr] Allocator-extended constructors

#include <memory>
#include <tuple>
#include <testsuite_hooks.h>

struct MyAlloc { };

// type that can't be constructed with an allocator
struct CannotUse
{
  CannotUse(int = 0, int = 0) : ok(true) { }

  bool ok;
};

// type that can be constructed with an allocator
// but which has uses_allocator == false
struct DoesNotUse
{
  typedef MyAlloc allocator_type;

  DoesNotUse(int = 0) : ok(true) { }
  DoesNotUse(std::allocator_arg_t, MyAlloc, int = 0) : ok(false) { }
  DoesNotUse(MyAlloc) : ok(false) { }
  DoesNotUse(int, MyAlloc) : ok(false) { }

  DoesNotUse(const DoesNotUse&) : ok(true) { }
  DoesNotUse(std::allocator_arg_t, MyAlloc, const DoesNotUse&) : ok(false) { }
  DoesNotUse(const DoesNotUse&, MyAlloc) : ok(false) { }

  DoesNotUse(DoesNotUse&&) : ok(true) { }
  DoesNotUse(std::allocator_arg_t, MyAlloc, DoesNotUse&&) : ok(false) { }
  DoesNotUse(DoesNotUse&&, MyAlloc) : ok(false) { }

  bool ok;
};

namespace std
{
  template<typename A> 
    struct uses_allocator<DoesNotUse, A> : false_type { };
}

// type that can be constructed with an allocator as second argument
struct UsesWithTag
{
  typedef MyAlloc allocator_type;

  UsesWithTag(int = 0) : ok(false) { }
  UsesWithTag(std::allocator_arg_t, MyAlloc, int = 0) : ok(true) { }
  UsesWithTag(MyAlloc) : ok(false) {  }
  UsesWithTag(int, MyAlloc) : ok(false) {  }

  UsesWithTag(const UsesWithTag&) : ok(false) { }
  UsesWithTag(std::allocator_arg_t, MyAlloc, const UsesWithTag&) : ok(true) { }
  UsesWithTag(const UsesWithTag&, MyAlloc) : ok(false) {  }

  UsesWithTag(UsesWithTag&&) : ok(false) { }
  UsesWithTag(std::allocator_arg_t, MyAlloc, UsesWithTag&&) : ok(true) { }
  UsesWithTag(UsesWithTag&&, MyAlloc) : ok(false) {  }

  bool ok;
};

// type that can be constructed with an allocator as last argument
struct UsesWithoutTag
{
  typedef MyAlloc allocator_type;

  UsesWithoutTag(int = 0) : ok(false) { }
  UsesWithoutTag(MyAlloc) : ok(true) { }
  UsesWithoutTag(int, MyAlloc) : ok(true) { }

  UsesWithoutTag(const UsesWithoutTag&) : ok(false) { }
  UsesWithoutTag(const UsesWithoutTag&, MyAlloc) : ok(true) { }

  UsesWithoutTag(UsesWithoutTag&&) : ok(false) { }
  UsesWithoutTag(UsesWithoutTag&&, MyAlloc) : ok(true) { }

  bool ok;
};

void test01()
{
  using std::allocator_arg;
  using std::tuple;
  using std::make_tuple;
  using std::get;

  typedef CannotUse T1;
  typedef DoesNotUse T2;
  typedef UsesWithTag T3;
  typedef UsesWithoutTag T4;
  typedef tuple<T1, T2, T3, T4> test_type;

  MyAlloc a;

  // default construction
  test_type t1(allocator_arg, a);
  VERIFY( get<0>(t1).ok );
  VERIFY( get<1>(t1).ok );
  VERIFY( get<2>(t1).ok );
  VERIFY( get<3>(t1).ok );

  // copy construction
  test_type t2(allocator_arg, a, t1);
  VERIFY( get<0>(t2).ok );
  VERIFY( get<1>(t2).ok );
  VERIFY( get<2>(t2).ok );
  VERIFY( get<3>(t2).ok );

  // move construction
  test_type t3(allocator_arg, a, std::move(t1));
  VERIFY( get<0>(t3).ok );
  VERIFY( get<1>(t3).ok );
  VERIFY( get<2>(t3).ok );
  VERIFY( get<3>(t3).ok );

  // construction from int
  test_type t4(allocator_arg, a, 1, 2, 3, 4);
  VERIFY( get<0>(t4).ok );
  VERIFY( get<1>(t4).ok );
  VERIFY( get<2>(t4).ok );
  VERIFY( get<3>(t4).ok );

  auto ints = make_tuple(1, 2, 3, 4);

  // construction from lvalue tuple of ints
  test_type t5(allocator_arg, a, ints);
  VERIFY( get<0>(t5).ok );
  VERIFY( get<1>(t5).ok );
  VERIFY( get<2>(t5).ok );
  VERIFY( get<3>(t2).ok );

  // construction from rvalue tuple of ints
  test_type t6(allocator_arg, a, std::move(ints));
  VERIFY( get<0>(t6).ok );
  VERIFY( get<1>(t6).ok );
  VERIFY( get<2>(t6).ok );
  VERIFY( get<3>(t6).ok );

}

void test02()
{
  using std::allocator_arg;
  using std::tuple;
  using std::make_tuple;

  typedef tuple<> test_type;

  MyAlloc a;

  // default construction
  test_type t1(allocator_arg, a);
  // copy construction
  test_type t2(allocator_arg, a, t1);
  // move construction
  test_type t3(allocator_arg, a, std::move(t1));
  // make_tuple
  test_type empty = make_tuple();
}

int main()
{
  test01();
  test02();
  return 0;
}
