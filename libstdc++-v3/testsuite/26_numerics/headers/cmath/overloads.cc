// Copyright (C) 2003-2014 Free Software Foundation, Inc.
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

// PR 3181
// Origin: pete@toyon.com

#include <cmath>

int main()
{
  int i = -1;
  int j = 9;
  double ans;
  ans = std::abs(i);
  ans = std::acos(i);
  ans = std::asin(i);
  ans = std::atan(i);
  ans = std::atan2(i, j);
  ans = std::cos(i);
  ans = std::cosh(i);
  ans = std::exp(i);
  ans = std::fabs(i);
  ans = std::floor(i);
  ans = std::log(i);
  ans = std::log10(i);
  ans = std::sqrt(i);
  ans = std::sin(i);
  ans = std::sinh(j);
  ans = std::tan(i);
  ans = std::tanh(i);
  ans = ans; // Suppress unused warnings.
}
