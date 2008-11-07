// Test for Container using non-standard pointer types.

// Copyright (C) 2008
// Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <vector>
#include <testsuite_hooks.h>
#include <ext/extptr_allocator.h>

// General tests element access and manipulation
void test01() 
{
  bool test __attribute__((unused)) = true;

  int A[] = { 0, 1, 2, 3, 4 };
  __gnu_cxx::_ExtPtr_allocator<int> alloc;
  std::vector<int,__gnu_cxx::_ExtPtr_allocator<int> > mv( A, A+5, alloc );

  VERIFY( mv.size() == 5 );
  VERIFY( mv.front() == 0 );
  VERIFY( mv.back() == 4 );
  VERIFY( mv.at(2) == 2 );
  VERIFY( mv[3] == 3);
  mv.front() = 5;
  mv.back() = 6;
  mv.at(2) = 7;
  mv[3] = 8;
  VERIFY( mv.size() == 5 );
  VERIFY( mv.front() == 5 );
  VERIFY( mv.back() == 6 );
  VERIFY( mv.at(2) == 7 );
  VERIFY( mv[3] == 8 );

  try 
    {
  	mv.at(100) = 8;
    }
  catch(std::out_of_range&)
    {
      VERIFY( true );
    }
  catch(...)
    {
      VERIFY( false );
    }
  
  const std::vector<int,__gnu_cxx::_ExtPtr_allocator<int> > cmv( mv );
  VERIFY( cmv.get_allocator() == mv.get_allocator() );
  VERIFY( mv.size() == 5 );
  VERIFY( mv.front() == 5 );
  VERIFY( mv.back() == 6 );
  VERIFY( mv.at(2) == 7 );
  VERIFY( mv[3] == 8 );	
}


int main()
{
  test01();
  return 0;
}
