// 2004-02-24  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2014 Free Software Foundation, Inc.
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


// 27.4.3 template class fpos

#include <ios>
#include <testsuite_hooks.h>

// libstdc++/14252
void test01()
{
  bool test __attribute__((unused)) = true;

  std::streamoff off01 = 10;
  std::streamoff off02 = 2;
  std::streamoff off03 = 2004;
  std::streamoff off04 = 1;

  VERIFY( off01++ == 10 );
  VERIFY( off01 == 11 );
  
  VERIFY( ++off02 == 3 );
  VERIFY( off02 == 3 );

  VERIFY( off03-- == 2004 );
  VERIFY( off03 == 2003 );

  VERIFY( --off04 == 0 );
  VERIFY( off04 == 0 );
} 

int main() 
{
  test01();
  return 0;
}
