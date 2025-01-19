// // Copyright (C) 2017-2025 Free Software Foundation, Inc.
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

#include <future>

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
  std::packaged_task task1{f0v};
  check_type<std::packaged_task<void()>>(task1);

  std::packaged_task task2{f0vn};
  check_type<std::packaged_task<void()>>(task2);

  std::packaged_task task3{f0i};
  check_type<std::packaged_task<int()>>(task3);

  std::packaged_task task4{f0in};
  check_type<std::packaged_task<int()>>(task4);

  std::packaged_task task5{f1l};
  check_type<std::packaged_task<long(int&)>>(task5);

  std::packaged_task task6{f1ln};
  check_type<std::packaged_task<long(double*)>>(task6);

  std::packaged_task task5a{std::move(task5)};
  check_type<std::packaged_task<long(int&)>>(task5a);

  std::packaged_task task6a{std::move(task6)};
  check_type<std::packaged_task<long(double*)>>(task6a);
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
  std::packaged_task task1{x};
  check_type<std::packaged_task<int(const short&, void*)>>(task1);

  Y y;
  std::packaged_task task2{y};
  check_type<std::packaged_task<void(int)>>(task2);

  std::packaged_task task3{[&x](float) -> X& { return x; }};
  check_type<std::packaged_task<X&(float)>>(task3);
}
