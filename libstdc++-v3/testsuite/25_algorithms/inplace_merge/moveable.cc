// { dg-do run { target c++11 } }

// Copyright (C) 2009-2018 Free Software Foundation, Inc.
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

// 25.3.4 [lib.alg.merge]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::rvalstruct;

typedef test_container<rvalstruct, bidirectional_iterator_wrapper> container;

void
test01()
{
  int array1[]={0,2,4,1,3,5};
  rvalstruct rv_array1[6];
  std::copy(array1, array1 + 6, rv_array1);
  container con1(rv_array1, rv_array1 + 6);
  std::inplace_merge(con1.begin(), con1.it(3), con1.end());
  VERIFY( rv_array1[0] == 0 && rv_array1[1] == 1 && rv_array1[2] == 2
	  && rv_array1[3] == 3 && rv_array1[4] == 4 && rv_array1[5] == 5 );

  int array2[]={0,2,4,5,1,3};
  rvalstruct rv_array2[6];
  std::copy(array2, array2 + 6, rv_array2);
  container con2(rv_array2, rv_array2 + 6);
  std::inplace_merge(con2.begin(), con2.it(4), con2.end());
  VERIFY( rv_array2[0] == 0 && rv_array2[1] == 1 && rv_array2[2] == 2
	  && rv_array2[3] == 3 && rv_array2[4] == 4 && rv_array2[5] == 5 );

  int array3[]={1,1,1,2,2,2};
  rvalstruct rv_array3[6];
  std::copy(array3, array3 + 6, rv_array3);
  container con3(rv_array3, rv_array3 + 6);
  std::inplace_merge(con3.begin(), con3.it(3), con3.end());
  VERIFY( rv_array3[0] == 1 && rv_array3[1] == 1 && rv_array3[2] == 1
	  && rv_array3[3] == 2 && rv_array3[4] == 2 && rv_array3[5] == 2 );

  int array4[]={1,1,1,1,2,2};
  rvalstruct rv_array4[6];
  std::copy(array4, array4 + 6, rv_array4);
  container con4(rv_array4, rv_array4 + 6);
  std::inplace_merge(con4.begin(), con4.it(4), con4.end());
  VERIFY( rv_array4[0] == 1 && rv_array4[1] == 1 && rv_array4[2] == 1
	  && rv_array4[3] == 1 && rv_array4[4] == 2 && rv_array4[5] == 2 );

  int array5[]={3,3,3,3};
  rvalstruct rv_array5[4];
  std::copy(array5, array5 + 4, rv_array5);
  container con5(rv_array5, rv_array5 + 4);
  std::inplace_merge(con5.begin(), con5.it(2), con5.end());
  VERIFY( rv_array5[0] == 3 && rv_array5[1] == 3 && rv_array5[2] == 3
	  && rv_array5[3] == 3 );

  int array6[]={3,3,3};
  rvalstruct rv_array6[3];
  std::copy(array6, array6 + 3, rv_array6);
  container con6(rv_array6, rv_array6 + 3);
  std::inplace_merge(con6.begin(), con6.it(0), con6.end());
  VERIFY( rv_array6[0] == 3 && rv_array6[1] == 3 && rv_array6[2] == 3 );

  int array7[]={3,3};
  rvalstruct rv_array7[2];
  std::copy(array7, array7 + 2, rv_array7);
  container con7(rv_array7, rv_array7 + 2);
  std::inplace_merge(con7.begin(), con7.it(2), con7.end());
  VERIFY( rv_array7[0] == 3 && rv_array7[1] == 3 );
}

int 
main()
{
  test01();
  return 0;
}
