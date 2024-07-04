// 2004-03-02  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2004-2024 Free Software Foundation, Inc.
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

// 27.4.3 fpos

// { dg-do compile }
// { dg-require-effective-target hosted }

#include <limits>
#include <iterator>

// libstdc++/14320

typedef std::istreambuf_iterator<char>::difference_type Distance;

#if __cplusplus >= 201103L
  static_assert( std::is_integral<Distance>::value, "integral type" );
  static_assert( std::is_signed<Distance>::value, "signed integral type" );
#else
template<typename> struct SignedInteger { enum { value = 0 }; };
// The C++ standard didn't originally have "long long", however that
// type is in the C++11 standard and testing for it allows
// ilp32 targets to pass this test when `Distance' is 64 bits.
template<> struct SignedInteger<long long int> { enum { value = 1 }; };
template<> struct SignedInteger<long int> { enum { value = 1 }; };
template<> struct SignedInteger<int> { enum { value = 1 }; };
template<> struct SignedInteger<short int> { enum { value = 1 }; };
template<> struct SignedInteger<char> {
  enum { value = std::numeric_limits<char>::is_signed };
};
template<> struct SignedInteger<wchar_t> {
  enum { value = std::numeric_limits<wchar_t>::is_signed };
};

char assertion[SignedInteger<Distance>::value ? 1 : -1];
#endif
