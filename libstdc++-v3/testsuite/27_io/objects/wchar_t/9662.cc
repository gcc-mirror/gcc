// 2003-04-30  Petur Runolfsson <peturr02@ru.is>

// Copyright (C) 2003 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>
#include <iostream>
#include <cstdio>
#include <cwchar>

// Check that operations on wcout can be mixed with wide operations
// on stdout.
void test01()
{
  bool test __attribute__((unused)) = true;

  std::wcout << L"Hello, ";
  VERIFY( std::fwide(stdout, 0) >= 0 );
  int ret = std::fputws(L"world!\n", stdout);
  VERIFY( ret >= 0 );
}

int main()
{
  test01();
  return 0;
}
