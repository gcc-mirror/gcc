// { dg-do compile { target c++14 } }

// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

// 26.6.10 valarray range access: [valarray.range]

#include <iterator>
#include <valarray>

// PR libstdc++/67374
void
test01()
{
  std::valarray<double> va{1.0, 2.0, 3.0};
  std::cbegin(va);
  std::cend(va);
  const auto& cva = va;
  std::cbegin(cva);
  std::cend(cva);
}
