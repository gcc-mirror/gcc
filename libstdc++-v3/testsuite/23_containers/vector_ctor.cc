// 1999-06-29 bkoz

// Copyright (C) 1999-2001 Free Software Foundation, Inc.
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

// 23.2.4.1 vector constructors, copy, and assignment

#include <vector>
#include <string>
#include <testsuite_hooks.h>

template<typename T>
  struct A { };

struct B { };

void test01()
{

  // 1
  bool test = true;
  std::vector< A<B> > vec01;
  std::vector< A<B> > vec02(5);
  typedef std::vector< A<B> >::size_type size_type;

  vec01 = vec02;

#ifdef DEBUG_ASSERT
  assert(test);
#endif
}

// 2
template class std::vector<double>;
template class std::vector< A<B> >;


// libstdc++/102
void test02()
{
  std::vector<int> v1;
  std::vector<int> v2 (v1);
}

// test range constructors and range-fill constructor
void
test03()
{
    const int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
    const int B[] = {7, 7, 7, 7, 7};
    const int N = sizeof(A) / sizeof(int);
    const int M = sizeof(B) / sizeof(int);
    bool test = true;

    std::vector<int> v3(A, A + N);
    VERIFY(std::equal(v3.begin(), v3.end(), A));

    std::vector<int> v4(v3.begin(), v3.end());
    VERIFY(std::equal(v4.begin(), v4.end(), A));

    std::vector<int> v5(M, 7);
    VERIFY(std::equal(v5.begin(), v5.end(), B));
    VERIFY(std::equal(B, B + M, v5.begin()));

#ifdef DEBUG_ASSERT
  assert(test);
#endif
}

// libstdc++/6513
void test04()
{
  bool test = true;
  const char* c_strings[5] = { "1", "2", "3", "4", "5" };
  std::vector<std::string> strings(c_strings, c_strings + 5);

#ifdef DEBUG_ASSERT
  assert(test);
#endif
}

int main()
{
  test01();
  test02(); 
  test03();
  test04();

  return 0;
}
