// { dg-do run { target c++11 } }
//
// Copyright (C) 2011-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <tr1/cmath>

namespace a
{
  template<typename> class Mat { };

  template<typename T> struct Mat2 : Mat<T> { };

  template<typename T>
    int fdim(Mat<T>) { return 1; }

  template<typename T, typename U>
    int floor(Mat<T>, U) { return 1; }
  template<typename T, typename U>
    int floor(T, Mat<U>) { return 1; }

  template<typename T, typename U, typename V>
    int fma(Mat<T>, U, V) { return 1; }
  template<typename T, typename U, typename V>
    int fma(T, Mat<U>, V) { return 1; }
  template<typename T, typename U, typename V>
    int fma(T, U, Mat<V>) { return 1; }
}

int main()
{
  int __attribute__((unused)) i;

  using namespace std::tr1;

  a::Mat2<double> c;
  i = fdim(c);
  i = floor(c, 0.);
  i = floor(0., c);
  i = floor(c, 1);
  i = floor(1, c);
  i = fma(c, 0., 1.);
  i = fma(0., c, 1.);
  i = fma(0., 1., c);
  i = fma(c, 0., 1);
  i = fma(0., c, 1);
  i = fma(0., 1, c);
}
