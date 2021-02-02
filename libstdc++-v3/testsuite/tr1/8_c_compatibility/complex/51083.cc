// { dg-do run { target c++11 } }
//
// Copyright (C) 2011-2021 Free Software Foundation, Inc.
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

#include <tr1/complex>

namespace a
{
  template<typename> class Mat { };

  template<typename T> struct Mat2 : Mat<T> { };

  template<typename T> int arg(Mat<T>) { return 1; }
  template<typename T> int conj(Mat<T>) { return 1; }
  template<typename T> int imag(Mat<T>) { return 1; }
  template<typename T> int norm(Mat<T>) { return 1; }
  template<typename T> int proj(Mat<T>) { return 1; }
  template<typename T> int real(Mat<T>) { return 1; }

  template<typename T, typename U> int pow(Mat<T>, U) { return 1; }
  template<typename T, typename U> int pow(T, Mat<U>) { return 1; }
}

int main()
{
  int __attribute__((unused)) i;

  using namespace std::tr1;

  a::Mat2< std::complex<double> > c;
  i = arg(c);
  i = conj(c);
  i = imag(c);
  i = norm(c);
  i = proj(c);
  i = real(c);
  i = pow(std::complex<float>(), c);
  i = pow(c, std::complex<float>());
}
