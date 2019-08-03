// { dg-options "-std=gnu++17" }

// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>
#include "../../../../include/std/pmroptional"

class S{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

   S(std::allocator_arg_t,allocator_type)
   : S(){}

   S(std::allocator_arg_t,allocator_type, S const& other)
     : S(other){}

   S() = default;
   S(S const& other)  = default;

}; // No hash specialization

struct value_type
{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  value_type(std::allocator_arg_t,allocator_type)
  : value_type(){}


  value_type(std::allocator_arg_t,allocator_type, size_t i)
  : value_type(i){}

  value_type(std::allocator_arg_t,allocator_type, value_type const& other)
    : value_type(other){}

  value_type(){};

  value_type(size_t _i) : i(_i){};

  value_type(value_type const& other): i(other.i)
      {};

  value_type& operator=(value_type const& other)
  {
    i = other.i;
    return *this;
  }
  size_t i = 0;
};

namespace std {
  template <> struct hash<value_type>
  {
    size_t operator()(const value_type & x) const
    {
      return x.i;
    }
  };
}
template<class T>
auto f(int) -> decltype(std::hash<std::pmr::optional<T>>(), std::true_type());

template<class T>
auto f(...) -> decltype(std::false_type());

static_assert(!decltype(f<S>(0))::value, "");

template<typename T>
constexpr bool hashable()
{ return std::is_invocable_v<std::hash<T>&, const T&>; }

static_assert(!hashable<std::pmr::optional<S>>());
static_assert(!hashable<std::pmr::optional<const S>>());
static_assert(hashable<std::pmr::optional<value_type>>());
static_assert(hashable<std::pmr::optional<const value_type>>());

int main()
{
  int x = 42;
  std::pmr::optional<value_type> x2 = 42;
  VERIFY(std::hash<value_type>()(x) == std::hash<std::pmr::optional<value_type>>()(x2));

  // PR libstdc++/82262
  std::pmr::optional<const value_type> x3 = x2;
  VERIFY(std::hash<value_type>()(x) == std::hash<std::pmr::optional<const value_type>>()(x3));
}
