// 2007-01-30  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007-2023 Free Software Foundation, Inc.
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

#include <new>
#include <typeinfo>
#include <exception>
#include <cstring>
#include <testsuite_hooks.h>

// libstdc++/14493
void test01() 
{
  using namespace std;

  bad_cast bc;
  VERIFY( !strcmp(bc.what(), "std::bad_cast") );
}

int main()
{
  test01();
  return 0;
}
