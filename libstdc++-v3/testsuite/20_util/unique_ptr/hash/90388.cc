// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

#include <memory>

template<typename Func, typename Arg, typename = void>
  struct is_callable
  : std::false_type
  { };

template<typename Func, typename Arg>
  struct is_callable<Func, Arg,
    decltype((void)(std::declval<Func&>()(std::declval<Arg>())))>
  : std::true_type
  { };

void
test01()
{
  struct D {
    struct pointer { };
    void operator()(pointer) const noexcept { }
  };
  static_assert( !is_callable<std::hash<D::pointer>&, D::pointer>::value );

  using UP = std::unique_ptr<int, D>;
  // [unord.hash]
  // Disabled specializations of hash are not function object types
  static_assert( !is_callable<std::hash<UP>&, UP>::value );
  static_assert( !is_callable<std::hash<UP>&, UP&>::value );
  static_assert( !is_callable<std::hash<UP>&, const UP&>::value );
}

struct D {
  struct pointer { };
  void operator()(pointer) const noexcept { }
};

bool operator==(D::pointer, std::nullptr_t) { return false; }
bool operator!=(D::pointer, std::nullptr_t) { return true; }

namespace std {
  template<> struct hash<D::pointer> {
    size_t operator()(D::pointer) const { throw 1; }
  };
}

void
test02()
{
  using UP = std::unique_ptr<int, D>;
  UP p;
  std::hash<UP> h;
  try {
    // [util.smartptr.hash]
    // The member functions are not guaranteed to be noexcept.
    h(p);
    throw "should not reach here";
  } catch (int) {
    // Should catch exception here, rather than terminating.
  }

  // Should still be noexcept if the underlying hash object is:
  using UP2 = std::unique_ptr<int>;
  UP2 p2;
  std::hash<UP2> h2;
  static_assert( noexcept(h2(p2)), "operator() is noexcept" );
}

int
main()
{
  test02();
}
