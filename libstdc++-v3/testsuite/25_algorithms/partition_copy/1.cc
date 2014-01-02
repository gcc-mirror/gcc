// { dg-options "-std=gnu++0x" }

// 2008-06-26  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008-2014 Free Software Foundation, Inc.
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

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;

typedef test_container<int, input_iterator_wrapper> Icontainer; 
typedef test_container<int, output_iterator_wrapper> Ocontainer;
int array[] = {0, 5, 2, 1, 3, 4};

bool
pred(int i)
{ return i > 2; }

void
test1()
{
  bool test __attribute__((unused)) = true;

  int true_out[1] = { -1 };
  int false_out[1] = { -1 };
  Icontainer in_con(array, array);
  Ocontainer true_out_con(true_out, true_out);
  Ocontainer false_out_con(false_out, false_out);

  std::pair<output_iterator_wrapper<int>, output_iterator_wrapper<int> > res =
    std::partition_copy(in_con.begin(), in_con.end(), 
			true_out_con.begin(), false_out_con.begin(), pred);
  
  VERIFY( res.first.ptr == true_out );
  VERIFY( res.second.ptr == false_out );
}

void
test2()
{
  bool test __attribute__((unused)) = true;

  int true_out[1] = { -1 };
  int false_out[1] = { -1 };
  Icontainer in_con(array, array + 2);
  Ocontainer true_out_con(true_out, true_out + 1);
  Ocontainer false_out_con(false_out, false_out + 1);

  std::pair<output_iterator_wrapper<int>, output_iterator_wrapper<int> > res =
    std::partition_copy(in_con.begin(), in_con.end(), 
			true_out_con.begin(), false_out_con.begin(), pred);

  VERIFY( res.first.ptr == true_out + 1 );
  VERIFY( res.second.ptr == false_out + 1 );
  VERIFY( true_out[0] == 5 );
  VERIFY( false_out[0] == 0 );
}

void
test3()
{
  bool test __attribute__((unused)) = true;

  int true_out[3] = { -1, -1, -1 };
  int false_out[3] = { -1, -1, -1 };
  Icontainer in_con(array, array + 6);
  Ocontainer true_out_con(true_out, true_out + 3);
  Ocontainer false_out_con(false_out, false_out + 3);

  std::pair<output_iterator_wrapper<int>, output_iterator_wrapper<int> > res =
    std::partition_copy(in_con.begin(), in_con.end(), 
			true_out_con.begin(), false_out_con.begin(), pred);

  VERIFY( res.first.ptr == true_out + 3 );
  VERIFY( res.second.ptr == false_out + 3 );
  VERIFY( true_out[0] == 5 && true_out[1] == 3 && true_out[2] == 4 );
  VERIFY( false_out[0] == 0 && false_out[1] == 2 && false_out[2] == 1 );
}

int 
main()
{
  test1();
  test2();
  test3();
  return 0;
}
