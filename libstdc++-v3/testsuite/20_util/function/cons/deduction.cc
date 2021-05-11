// Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <functional>

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void f0v();
void f0vn() noexcept;
int f0i();
int f0in() noexcept;
long f1l(int&);
long f1ln(double*) noexcept;

void
test01()
{
  std::function func1 = f0v;
  check_type<std::function<void()>>(func1);

  std::function func2 = f0vn;
  check_type<std::function<void()>>(func2);

  std::function func3 = f0i;
  check_type<std::function<int()>>(func3);

  std::function func4 = f0in;
  check_type<std::function<int()>>(func4);

  std::function func5 = f1l;
  check_type<std::function<long(int&)>>(func5);

  std::function func6 = f1ln;
  check_type<std::function<long(double*)>>(func6);

  std::function func5a = func5;
  check_type<std::function<long(int&)>>(func5a);

  std::function func6a = func6;
  check_type<std::function<long(double*)>>(func6a);
}

struct X {
  int operator()(const short&, void*);
};

struct Y {
  void operator()(int) const & noexcept;
};

void
test02()
{
  X x;
  std::function func1 = x;
  check_type<std::function<int(const short&, void*)>>(func1);

  Y y;
  std::function func2 = y;
  check_type<std::function<void(int)>>(func2);

  std::function func3 = [&x](float) -> X& { return x; };
  check_type<std::function<X&(float)>>(func3);
}
