// { dg-add-options ieee }

// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

// 18.2.1.1 template class numeric_limits

#include <limits>
#include <testsuite_hooks.h>

template<typename T>
  void 
  test_traps(T r = T(0))
  {
    typedef T value_type;
    volatile value_type i(5);
    volatile value_type j(0);
    
    if (!std::numeric_limits<value_type>::traps)
      r = i / j;
  }

// libstdc++/22203
int main()
{
  test_traps<int>();
  test_traps<unsigned int>();
  test_traps<long>();
  test_traps<unsigned long>();
  test_traps<long long>();
  test_traps<unsigned long long>();
 
  /*
    For floating points, trapping is a different, more complicated
    story.  If is_iecxxx is true, then division by zero would not trap
    (infinity).  If is_iecxxx is false, we don't know (VAX may trap for
    0/0 -- I have to check).  For most cases (i.e. IEE-754), trapping
    for floating points have to do with whether there is a support for
    signaling NaN.
    - Gaby.
  */
  //  test_traps<float>();
  //  test_traps<double>();
  //  test_traps<long double>();

  return 0;
}
