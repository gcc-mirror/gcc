// 2004-01-03  Jerry Quinn  <jlquinn@optonline.net>

// Copyright (C) 2004-2021 Free Software Foundation, Inc.
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


// PR 3247

// This is DR-253.  Test for accessible assignment-operators.
#include <valarray>
#include <testsuite_hooks.h>

bool check_array(std::valarray<double>& a, double b[])
{
  for (unsigned int i=0; i < a.size(); i++)
    if (a[i] != b[i]) return false;
  return true;
}

int main()
{
  std::valarray<double> val_d(10);  //  0 1 2 3 4 5 6 7 8 9 
  std::valarray<double> val_d1(10); // 10 9 8 7 6 5 4 3 2 1

  for (int i=0; i< 10; i++) { val_d[i] = 10;  val_d1[i] = i; }
  std::valarray<double> val_c(val_d);
  std::valarray<double> val_f(val_d);
  std::valarray<double> val_g(val_d);

  std::slice slc(1, 3, 3);	// 1 4 7
  val_d[slc] = val_d1[slc];

  double ans1[10] = {10, 1, 10, 10, 4, 10, 10, 7, 10, 10};
  VERIFY(check_array(val_d, ans1));

  std::valarray<std::size_t> val_size(2);
  std::valarray<std::size_t> val_stride(2);
  val_size[0] = 2;   val_size[1] = 3;
  val_stride[0] = 4; val_stride[1] = 1;

  std::gslice gslc(1, val_size, val_stride);
  val_c[gslc] = val_d1[gslc];

  double ans2[10] = {10, 1, 2, 3, 10, 5, 6, 7, 10, 10};
  VERIFY(check_array(val_c, ans2));

  std::valarray<bool> val_b(false, 10);
  val_b[2] = val_b[6] = val_b[9] = true;
  val_f[val_b] = val_d1[val_b];

  double ans3[10] = {10, 10, 2, 10, 10, 10, 6, 10, 10, 9};
  VERIFY(check_array(val_f, ans3));

  size_t addr[] = {1, 2, 3, 4, 5};
  size_t addr1[] = {2, 7, 1, 9, 4};
  std::valarray<std::size_t> val_indirect(addr, 5);
  std::valarray<std::size_t> val_indirect1(addr1, 5);
  val_g[val_indirect] = val_d1[val_indirect1];

  double ans4[10] = {10, 2, 7, 1, 9, 4, 10, 10, 10, 10};
  VERIFY(check_array(val_g, ans4));

  return 0;
}
