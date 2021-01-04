// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <chrono>

void
test01()
{
  using namespace std::chrono;
  using std::exa;

  // LWG 2094
  // duration conversion overflow shouldn't participate in overload resolution
  bool f(milliseconds);
  void f(seconds);
  duration<int,exa> r(1);
  f(r);
}

void
test02()
{
  struct Number
  {
    explicit
    Number(int t = 0) : i(t)
    { }

    int i = 0;

    Number& operator+=(Number n) { i += n.i; return *this; }
    Number& operator-=(Number n) { i -= n.i; return *this; }
    Number& operator*=(Number n) { i *= n.i; return *this; }
    Number& operator/=(Number n) { i /= n.i; return *this; }
    Number& operator%=(Number n) { i %= n.i; return *this; }

    Number operator+(Number n) { return Number{ i + n.i }; }
    Number operator-(Number n) { return Number{ i - n.i }; }
    Number operator*(Number n) { return Number{ i * n.i }; }
    Number operator/(Number n) { return Number{ i / n.i }; }
    Number operator%(Number n) { return Number{ i % n.i }; }
  };

  using std::chrono::duration;

  static_assert( ! std::is_constructible<duration<int>, duration<Number>>(),
      "duration(const duration<R2, P2>&) constrained on R2 -> R conversion" );
}
