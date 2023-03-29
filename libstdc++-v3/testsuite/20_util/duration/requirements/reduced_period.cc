// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// Test the changes introduced by P0548R1 "common_type and duration".
// Specifically, duration<R,P>::period should be the reduced ratio,
// and common_type<D1, D2>::type should be a duration using the
// reduced ratio.

#include <chrono>

using std::chrono::duration;
using std::ratio;
using std::common_type;
using std::is_same;

void
test01()
{
  using D1 = duration<int, ratio<10, 10>>;
  static_assert( is_same<D1::period, ratio<1, 1>>::value,
      "duration<R, P>::period is P::type, not P" );

  using C1 = common_type<D1>::type;
  static_assert( is_same<C1, duration<int, D1::period>>::value,
      "common_type_t<duration<R, P1> is duration<R, P1::type>");
  static_assert( is_same<common_type<D1, D1>::type, C1>::value,
      "common_type_t<D1, D1> is common_type_t<D1>" );
  static_assert( is_same<common_type<D1, D1, D1>::type, C1>::value,
      "common_type_t<D1, D1, D1> is common_type_t<D1>" );

  using D2 = duration<int, ratio<30, 15>>;
  static_assert( is_same<D2::period, ratio<2, 1>>::value,
      "duration<R, P2>::period is P2::type, not P2" );

  using C2 = common_type<D2>::type;
  static_assert( is_same<C2, duration<int, D2::period>>::value,
      "common_type_t<duration<R, P2> is duration<R, P2::type>");
  static_assert( is_same<common_type<D2, D2>::type, C2>::value,
      "common_type_t<D2, D2> is common_type_t<D2>" );
  static_assert( is_same<common_type<D2, D2, D2>::type, C2>::value,
      "common_type_t<D2, D2, D2> is common_type_t<D2>" );

  using D3 = duration<int, ratio<4, 12>>;
  static_assert( is_same<D3::period, ratio<1, 3>>::value,
      "duration<R, P3>::period is P3::type, not P3" );

  using C3 = common_type<D3>::type;
  static_assert( is_same<C3, duration<int, D3::period>>::value,
      "common_type_t<duration<R, P3> is duration<R, P3::type>");
  static_assert( is_same<common_type<D3, D3>::type, C3>::value,
      "common_type_t<D3, D3> is common_type_t<D3>" );
  static_assert( is_same<common_type<D3, D3, D3>::type, C3>::value,
      "common_type_t<D3, D3, D3> is common_type_t<D3>" );

  using C12 = common_type<D1, D2>::type;
  static_assert( is_same<C12, C1>::value,
      "common_type_t<D1, D2> uses the right period" );
  using C21 = common_type<D2, D1>::type;
  static_assert( is_same<C21, C12>::value,
      "common_type_t<D1, D2> is common_type_t<D2, D1>" );

  using C13 = common_type<D1, D3>::type;
  static_assert( is_same<C13, C3>::value,
      "common_type_t<D1, D3> uses the right period" );
  using C31 = common_type<D3, D1>::type;
  static_assert( is_same<C31, C13>::value,
      "common_type_t<D1, D3> is common_type_t<D3, D1>" );

  using C23 = common_type<D2, D3>::type;
  static_assert( is_same<C23, C3>::value,
      "common_type_t<D2, D3> uses the right period" );
  using C32 = common_type<D3, D2>::type;
  static_assert( is_same<C32, C23>::value,
      "common_type_t<D2, D3> is common_type_t<D3, D2>" );

  using C123 = common_type<D1, D2, D3>::type;
  static_assert( is_same<C123, C3>::value,
      "common_type of three durations uses the right period" );
  using C132 = common_type<D1, D3, D2>::type;
  static_assert( is_same<C132, C123>::value, "order doesn't matter" );
  using C312 = common_type<D3, D1, D2>::type;
  static_assert( is_same<C312, C123>::value, "order doesn't matter" );
  using C321 = common_type<D3, D2, D1>::type;
  static_assert( is_same<C321, C123>::value, "order doesn't matter" );

  using C = common_type<duration<short, ratio<1, 3>>,
			duration<unsigned, ratio<1, 2>>>::type;
  static_assert( is_same<C, duration<common_type<short, unsigned>::type,
				     ratio<1, 6>>>::value, "" );
}

void
test02()
{
  using D1 = duration<int, ratio<10, 10>>;
  D1 d1;
  static_assert( is_same<decltype(+d1), common_type<D1>::type>::value,
      "unary + returns the reduced duration" );
  static_assert( is_same<decltype(-d1), common_type<D1>::type>::value,
      "unary - returns the reduced duration" );

  using D2 = duration<int, ratio<30, 15>>;
  D2 d2;
  static_assert( is_same<decltype(+d2), common_type<D2>::type>::value,
      "unary + returns the reduced duration" );
  static_assert( is_same<decltype(-d2), common_type<D2>::type>::value,
      "unary - returns the reduced duration" );

  using D3 = duration<int, ratio<4, 12>>;
  D3 d3;
  static_assert( is_same<decltype(+d3), common_type<D3>::type>::value,
      "unary + returns the reduced duration" );
  static_assert( is_same<decltype(-d3), common_type<D3>::type>::value,
      "unary - returns the reduced duration" );
}

template<typename T>
struct Number
{
  explicit
  Number(T t = 0) : i(t)
  { }

  template<typename U, bool B = std::is_convertible<U, T>::value,
	   typename = typename std::enable_if<B>::type>
    explicit
    Number(Number<U> n) : i(n.i)
    { }

  T i = 0;

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

namespace std
{
  // Specialise common_type to give a different type
  template<>
    struct common_type<Number<int>, Number<int>>
    { using type = Number<long>; };
}

void
test03()
{

  using D4 = duration<Number<int>, ratio<49, 21>>;
  static_assert( is_same<common_type<D4>::type,
			 duration<Number<long>, ratio<7, 3>>>::value,
      "common_type_t<duration<R,P>> uses common_type_t<R>" );

  D4 d4;
  static_assert( is_same<decltype(+d4), common_type<D4>::type>::value,
      "unary + returns type with common_type_t<D4::rep> as rep" );
  static_assert( is_same<decltype(-d4), common_type<D4>::type>::value,
      "unary - returns type with common_type_t<D4::rep> as rep" );
}
