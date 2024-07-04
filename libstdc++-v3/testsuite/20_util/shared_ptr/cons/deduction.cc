// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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
// { dg-require-effective-target hosted }

#include <memory>

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void
test01()
{
  std::shared_ptr<long> s;
  std::shared_ptr s2 = s;
  check_type<std::shared_ptr<long>>(s2);

  std::weak_ptr<long> w;
  std::shared_ptr s3(w);
  check_type<std::shared_ptr<long>>(s3);

  struct D { void operator()(double*) { } };
  std::unique_ptr<double, D> u;
  std::shared_ptr s4 = std::move(u);
  check_type<std::shared_ptr<double>>(s4);
}
