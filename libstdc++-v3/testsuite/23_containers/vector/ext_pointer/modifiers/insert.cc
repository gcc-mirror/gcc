// Test for Container using non-standard pointer types.

// Copyright (C) 2008-2021 Free Software Foundation, Inc.
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


#include <vector>
#include <testsuite_hooks.h>
#include <ext/extptr_allocator.h>
#include <stdexcept>

void test01() 
{ 
  __gnu_cxx::_ExtPtr_allocator<int> alloc;
  std::vector<int, __gnu_cxx::_ExtPtr_allocator<int> > iv(alloc);
  VERIFY( iv.get_allocator() == alloc );
  VERIFY( iv.size() == 0 );
  
  int A[] = { 0, 1, 2, 3, 4 };
  int B[] = { 5, 5, 5, 5, 5 };
  int C[] = { 6, 7 };
  iv.insert(iv.end(), A, A+5 );
  VERIFY( iv.size() == 5 );
  iv.insert(iv.begin(), 5, 5 );
  iv.insert(iv.begin()+5, 7);
  iv.insert(iv.begin()+5, 6);
  VERIFY( std::equal(iv.begin(), iv.begin()+5, B ));
  VERIFY( std::equal(iv.begin()+5, iv.begin()+7, C));
  VERIFY( std::equal(iv.begin()+7, iv.end(), A));
  VERIFY( iv.size() == 12 );

  try
    {
      iv.insert(iv.end(), iv.max_size() + 1, 1);
    }
  catch(std::length_error&)
    {
      VERIFY( true );
    }
  catch(...)
    {
      VERIFY( false );
    }
}

int main()
{
  test01();
  return 0;
}
