// { dg-do run { target c++11 } }

// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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

// C++11 26.6.10 valarray range access: [valarray.range]

#include <valarray>

void
test01()
{
  std::valarray<double> va{1.0, 2.0, 3.0};
  (void) std::begin(va);
  (void) std::end(va);
  const auto& cva = va;
  (void) std::begin(cva);
  (void) std::end(cva);

  using Iter = decltype(std::begin(va));
  using IterTraits = std::iterator_traits<Iter>;
  static_assert( std::is_same<Iter, decltype(std::end(va))>::value, "" );
  static_assert( std::is_same<IterTraits::iterator_category,
			      std::random_access_iterator_tag>::value, "" );
  static_assert( std::is_same<IterTraits::value_type, double>::value, "" );
  static_assert( std::is_same<IterTraits::reference, double&>::value, "" );
  using CIter = decltype(std::begin(cva));
  using CIterTraits = std::iterator_traits<CIter>;
  static_assert( std::is_same<CIter, decltype(std::end(cva))>::value, "" );
  static_assert( std::is_same<CIterTraits::iterator_category,
			      std::random_access_iterator_tag>::value, "" );
  static_assert( std::is_same<CIterTraits::value_type, double>::value, "" );
  static_assert( std::is_same<CIterTraits::reference, const double&>::value, "" );
#if __cplusplus >= 202002L
  static_assert( std::contiguous_iterator<Iter> );
  static_assert( std::contiguous_iterator<CIter> );
#endif
}

// PR libstdc++/103022
void
test02()
{
  std::valarray<double> va;
  (void) std::begin(va);
  (void) std::end(va);
  const auto& cva = va;
  (void) std::begin(cva);
  (void) std::end(cva);
}

int main()
{
  test01();
  test02();
}
