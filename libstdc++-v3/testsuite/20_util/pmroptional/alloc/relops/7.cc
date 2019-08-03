// TODO : make thi sinto somethign with aa aware type
// { dg-options "-std=gnu++17" }
// { dg-do run }

// Copyright (C) 2017-2018 Free Software Foundation, Inc.
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
#include <string>
#include "../../../../../include/std/pmroptional"
template<typename T>
struct value_type
{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  value_type(std::allocator_arg_t,allocator_type)
  : value_type(){}


  value_type(std::allocator_arg_t,allocator_type, T i)
  : value_type(i){}

  value_type(std::allocator_arg_t,allocator_type, value_type const& other)
    : value_type(other){}

  value_type(){};

  value_type(T _i) : i(_i){};

  value_type(value_type const& other): i(other.i)
      {};

  value_type& operator=(value_type const& other)
  {
    i = other.i;
    return *this;
  }
  T i = 0;
};
template<typename T>
bool
operator==(value_type<T> const& lhs, int const& rhs)
{ return lhs.i == rhs; }
template<typename T>
bool
operator!=(value_type<T> const& lhs, int const& rhs)
{ return lhs.i != rhs; }
template<typename T>
bool
operator<(value_type<T> const& lhs, int const& rhs)
{ return lhs.i < rhs; }
template<typename T>
bool
operator>(value_type<T> const& lhs, int const& rhs)
{ return lhs.i > rhs; }
template<typename T>
bool
operator<=(value_type<T> const& lhs, int const& rhs)
{ return lhs.i <= rhs; }
template<typename T>
bool
operator>=(value_type<T> const& lhs, int const& rhs)
{ return lhs.i >= rhs; }

template<typename T>
bool
operator==(int const& lhs, value_type<T>const& rhs)
{ return lhs == rhs.i; }
template<typename T>
bool
operator!=(int const& lhs, value_type<T>const& rhs)
{ return lhs != rhs.i; }
template<typename T>
bool
operator<(int const& lhs, value_type<T>const& rhs)
{ return lhs < rhs.i; }
template<typename T>
bool
operator>(int const& lhs, value_type<T>const& rhs)
{ return lhs > rhs.i; }
template<typename T>
bool
operator<=(int const& lhs, value_type<T>const& rhs)
{ return lhs <= rhs.i; }
template<typename T>
bool
operator>=(int const& lhs, value_type<T>const& rhs)
{ return lhs >= rhs.i; }

template<typename T, typename U>
bool
operator==(value_type<U>const& lhs, value_type<T>const& rhs)
{ return lhs.i == rhs.i; }
template<typename T, typename U>
bool
operator!=(value_type<U>const& lhs, value_type<T>const& rhs)
{ return lhs.i != rhs.i; }
template<typename T, typename U>
bool
operator<(value_type<U>const& lhs, value_type<T>const& rhs)
{ return lhs.i < rhs.i; }
template<typename T, typename U>
bool
operator>(value_type<U>const& lhs, value_type<T>const& rhs)
{ return lhs.i > rhs.i; }
template<typename T, typename U>
bool
operator<=(value_type<U>const& lhs, value_type<T>const& rhs)
{ return lhs.i <= rhs.i; }
template<typename T, typename U>
bool
operator>=(value_type<U>const& lhs, value_type<T>const& rhs)
{ return lhs.i >= rhs.i; }
int main()
{
  std::pmr::optional<value_type<int>> o = 42;
  std::pmr::optional<value_type<const int>> o2 = 666;
  VERIFY(o == 42);
  VERIFY(o != 43);
  VERIFY(o < 43);
  VERIFY(o > 41);
  VERIFY(o <= 43);
  VERIFY(o >= 41);
  VERIFY(o2 == 666);
  VERIFY(o2 != 667);
  VERIFY(o2 < 667);
  VERIFY(o2 > 665);
  VERIFY(o2 <= 667);
  VERIFY(o2 >= 665);
  VERIFY(42 == o);
  VERIFY(43 != o);
  VERIFY(41< o);
  VERIFY(43 > o);
  VERIFY(41 <= o);
  VERIFY(43 >= o);
  VERIFY(666 == o2);
  VERIFY(667 != o2);
  VERIFY(665 < o2);
  VERIFY(667 > o2);
  VERIFY(665 <= o2);
  VERIFY(667 >= o2);

  std::pmr::optional<value_type<int>> oi = 42;
  std::pmr::optional<value_type<long int>> ol = 666;
  VERIFY(!(oi == ol));
  VERIFY(!(ol == oi));
  VERIFY(oi != ol);
  VERIFY(ol != oi);
}
