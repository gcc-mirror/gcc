// 2004-10-20  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 6.2.2 Class template array

#include <tr1/array>
#include <testsuite_hooks.h>

void
test01() 
{ 
  {
    const size_t len = 5;
    typedef std::tr1::array<int, len> array_type;
    bool test __attribute__((unused)) = true;
    array_type a = { 0, 1, 2, 3, 4 };
    
    VERIFY( a.size() == len );
  }

  {
    const size_t len = 0;
    typedef std::tr1::array<int, len> array_type;
    bool test __attribute__((unused)) = true;
    array_type a;
    
    VERIFY( a.size() == len );
  }
}

int main()
{
  test01();
  return 0;
}

