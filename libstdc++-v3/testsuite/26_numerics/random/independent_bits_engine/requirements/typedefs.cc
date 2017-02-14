// { dg-do compile { target c++11 } }
// { dg-require-cstdint "" }
//
// 2008-11-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2008-2017 Free Software Foundation, Inc.
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

// 26.4.4.2 class template independent_bits_engine [rand.adapt.ibits]
// 26.4.2.3 Concept RandomNumberEngineAdaptor [rand.concept.adapt]

#include <random>

void
test01()
{
  typedef std::independent_bits_engine
    <
      std::subtract_with_carry_engine
        <
          uint_fast64_t, 48, 5, 12
        >,
      48, uint_fast64_t
    > test_type;

  typedef test_type::result_type result_type;
}
