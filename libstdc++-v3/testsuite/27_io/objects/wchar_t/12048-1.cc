// Derived from libstdc++/12048 by LJR <ljrittle@acm.org> with
// reminder from Petur Runolfsson <peturr02@ru.is>.

// Copyright (C) 2003-2017 Free Software Foundation, Inc.
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

#include <iostream>
#include <cstdio>
#include <testsuite_hooks.h>

void
test01()
{
  VERIFY( std::freopen("cin_unget-1.txt", "r", stdin) );

  wchar_t c1;
  wchar_t c2;
  std::wcin.get(c1);
  std::wcin.unget();
  std::wcin.get(c2);
  VERIFY( std::wcin.good() );
  VERIFY( c1 == c2 );
}

int main(void)
{
  test01();
  return 0;
}
