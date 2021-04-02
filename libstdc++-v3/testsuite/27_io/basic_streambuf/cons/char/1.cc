// 1999-10-11 bkoz

// Copyright (C) 1999-2021 Free Software Foundation, Inc.
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


// 27.5.2 template class basic_streambuf

#include <streambuf>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

void test01()
{
  __gnu_test::constraint_streambuf buf01;

  // 27.5.2.1 basic_streambuf ctors
  // default ctor initializes 
  // - all pointer members to null pointers
  // - locale to current global locale
  VERIFY( buf01.check_pointers() );
  VERIFY( buf01.getloc() == std::locale() );
}

int main() 
{
  test01();
  return 0;
}
