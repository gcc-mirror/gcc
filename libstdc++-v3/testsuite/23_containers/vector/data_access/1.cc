// 2005-08-29  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005, 2009 Free Software Foundation, Inc.
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

#include <vector>
#include <testsuite_hooks.h>

// libstdc++/23578
void test01() 
{ 
  bool test __attribute__((unused)) = true;
  typedef std::vector<int> vector_type;

  {
    const int A[] = { 0, 1, 2, 3, 4 };    
    vector_type v(A, A + 5);
    VERIFY( v.data() == &v.front() );
    int* pi = v.data();
    VERIFY( *pi == 0 );
  }

  {
    const int A[] = { 4, 3, 2, 1, 0 };    
    const vector_type cv(A, A + 5);
    VERIFY( cv.data() == &cv.front() );
    const int* pci = cv.data();
    VERIFY( *pci == 4 );
  }
}

int main()
{
  test01();
  return 0;
}
