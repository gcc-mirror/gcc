// { dg-do compile }

// Copyright (C) 2007-2017 Free Software Foundation, Inc.
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

#include <complex>

namespace std {
  template<class T> class complex;
  template<> class complex<float>;
  template<> class complex<double>;
  template<> class complex<long double>;

  // 26.2.6 operators:
  template<class T>
    complex<T> operator+(const complex<T>&, const complex<T>&);
  template<class T> complex<T> operator+(const complex<T>&, const T&);
  template<class T> complex<T> operator+(const T&, const complex<T>&);
  template<class T> complex<T> operator-
    (const complex<T>&, const complex<T>&);
  template<class T> complex<T> operator-(const complex<T>&, const T&);
  template<class T> complex<T> operator-(const T&, const complex<T>&);
  template<class T> complex<T> operator*
    (const complex<T>&, const complex<T>&);
  template<class T> complex<T> operator*(const complex<T>&, const T&);
  template<class T> complex<T> operator*(const T&, const complex<T>&);
  template<class T> complex<T> operator/
    (const complex<T>&, const complex<T>&);
  template<class T> complex<T> operator/(const complex<T>&, const T&);
  template<class T> complex<T> operator/(const T&, const complex<T>&);
  template<class T> complex<T> operator+(const complex<T>&);
  template<class T> complex<T> operator-(const complex<T>&);
  template<class T> _GLIBCXX_CONSTEXPR bool operator==
    (const complex<T>&, const complex<T>&);
  template<class T> _GLIBCXX_CONSTEXPR bool operator==
    (const complex<T>&, const T&);
  template<class T> _GLIBCXX_CONSTEXPR bool operator==
    (const T&, const complex<T>&);


  template<class T> _GLIBCXX_CONSTEXPR bool operator!=
    (const complex<T>&, const complex<T>&);
  template<class T> _GLIBCXX_CONSTEXPR bool operator!=
    (const complex<T>&, const T&);
  template<class T> _GLIBCXX_CONSTEXPR bool operator!=
    (const T&, const complex<T>&);
  template<class T, class charT, class traits>
  basic_istream<charT, traits>&
  operator>>(basic_istream<charT, traits>&, complex<T>&);
  template<class T, class charT, class traits>
  basic_ostream<charT, traits>&
  operator<<(basic_ostream<charT, traits>&, const complex<T>&);

  // 26.2.7 values:
  template<class T> _GLIBCXX_CONSTEXPR T real(const complex<T>&);
  template<class T> _GLIBCXX_CONSTEXPR T imag(const complex<T>&);
  template<class T> T abs(const complex<T>&);
  template<class T> T arg(const complex<T>&);
  template<class T> T norm(const complex<T>&);
  template<class T> complex<T> conj(const complex<T>&);
  template<class T> complex<T> polar(const T& rho, const T& theta);

  // 26.2.8 transcendentals:
  template<class T> complex<T> cos(const  complex<T>&);
  template<class T> complex<T> cosh(const complex<T>&);
  template<class T> complex<T> exp(const  complex<T>&);
  template<class T> complex<T> log(const  complex<T>&);
  template<class T> complex<T> log10(const complex<T>&);
  template<class T> complex<T> pow(const complex<T>&, int);
  template<class T> complex<T> pow(const complex<T>&, const T&);
  template<class T> complex<T> pow(const complex<T>&, const complex<T>&);
  template<class T> complex<T> pow(const T&, const complex<T>&);
  template<class T> complex<T> sin (const complex<T>&);
  template<class T> complex<T> sinh(const complex<T>&);
  template<class T> complex<T> sqrt(const complex<T>&);
  template<class T> complex<T> tan(const complex<T>&);
  template<class T> complex<T> tanh(const complex<T>&);
}
