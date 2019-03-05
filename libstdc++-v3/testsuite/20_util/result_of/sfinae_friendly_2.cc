// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2019 Free Software Foundation, Inc.
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

// Taken from N3436:

#include <type_traits>
#include <string>

struct eat { template<typename T> eat(T const &) {} };
struct not_incrementable {};

struct inc {
 template<typename T>
 auto operator()(T t) const -> decltype(t++)
 { return t++; }
};

template<typename A>
typename std::result_of<inc(A)>::type // sfinae here
try_inc(A a) {
  return inc()(a);
}

not_incrementable
try_inc(eat) {
  return not_incrementable();
}

template<typename>
struct never { static const bool value = false; };

template<typename T>
struct Fail
{
  static_assert(never<T>::value, "duh");
  typedef int type;
};

struct Fun
{
  template<typename T>
  typename Fail<T>::type operator()(T)
  { return 0; }
};

template<typename T>
typename std::result_of<Fun(T)>::type foo(T)
{ return 0; }

template<typename>
int foo(...)
{ return 0; }

void result_of_sfinae() {
  int x = try_inc(1); // OK
  not_incrementable y = try_inc(std::string("foo")); // OK, not_incrementable
  (void) x;
  (void) y;
}
