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

// 23.2.1.3 deque modifiers

#include <deque>
#include <testsuite_hooks.h>

void erase(size_t num_elm, size_t elm_strt, size_t elm_end)
{
  using __gnu_test::copy_tracker;
  using __gnu_test::assignment_operator;

  std::deque<copy_tracker> x(num_elm);
  copy_tracker::reset();
  
  x.erase(x.begin() + elm_strt, x.begin() + elm_end);
  
  const size_t min_num_cpy
    = elm_strt == elm_end ? 0 : std::min(elm_strt, num_elm - elm_end);

  VERIFY( assignment_operator::count() == min_num_cpy );
}

// http://gcc.gnu.org/ml/libstdc++/2007-01/msg00098.html
void test01()
{
  for (size_t num_elm = 0; num_elm <= 10; ++num_elm)
    for (size_t elm_strt = 0; elm_strt <= num_elm; ++elm_strt)
      for (size_t elm_end = elm_strt; elm_end <= num_elm; ++elm_end)
	erase(num_elm, elm_strt, elm_end);
}

int main()
{
  test01();
  return 0;
}
