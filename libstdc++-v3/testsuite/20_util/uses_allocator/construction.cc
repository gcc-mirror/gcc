// { dg-do run { target c++11 } }

// Copyright (C) 2011-2023 Free Software Foundation, Inc.
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

// 20.6.7.2 uses-allocator construction

#include <memory>
#include <tuple>
#include <testsuite_hooks.h>

struct MyAlloc { };

// type that can't be constructed with an allocator
struct CannotUse
{
  CannotUse(int) : ok(true) { }

  bool ok;
};

// type that can be constructed with an allocator
// but which has uses_allocator == false
struct DoesNotUse
{
  typedef MyAlloc allocator_type;

  DoesNotUse(int) : ok(true) { }
  DoesNotUse(std::allocator_arg_t, MyAlloc, int) : ok(false) { }
  DoesNotUse(int, MyAlloc) : ok(false) { }

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

  UsesWithTag(int) : ok(false) { }
  UsesWithTag(std::allocator_arg_t, MyAlloc, int) : ok(true) { }
  UsesWithTag(int, MyAlloc) : ok(false) {  }

  bool ok;
};

// type that can be constructed with an allocator as last argument
struct UsesWithoutTag
{
  typedef MyAlloc allocator_type;

  UsesWithoutTag(int) : ok(false) { }
  UsesWithoutTag(int, MyAlloc) : ok(true) { }

  bool ok;
};


template<typename TestType, typename... T>
  bool test2(T... args)
  {
    using std::allocator_arg;
    using std::tuple;
    using std::get;

    tuple<TestType, T...> t(allocator_arg, MyAlloc(), 1, args...);

    return get<0>(t).ok;
  }

template<typename... T>
  void test(T... args)
  {
    VERIFY( test2<CannotUse>(args...) );
    VERIFY( test2<DoesNotUse>(args...) );
    VERIFY( test2<UsesWithTag>(args...) );
    VERIFY( test2<UsesWithoutTag>(args...) );
  }

int main()
{
  test();
  test(1);
  test(1, 2);
  return 0;
}
