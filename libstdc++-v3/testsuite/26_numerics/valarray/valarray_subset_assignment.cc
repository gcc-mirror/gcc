// 2004-01-03  Jerry Quinn  <jlquinn@optonline.net>

// Copyright (C) 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// PR 3247

// This is DR-253.  Test for accessible assignment-operators.
#include <valarray>
#include <testsuite_hooks.h>

bool check_array(std::valarray<double>& a, double b[])
{
  for (int i=0; i < a.size(); i++)
    if (a[i] != b[i]) return false;
  return true;
}

int main()
{
  double dvar = 1.0;
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
};
