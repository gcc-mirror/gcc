// 2007-05-29 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2007-2020 Free Software Foundation, Inc.
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

#include <cstring>
#include <string>
#include <stdexcept>
#include <testsuite_hooks.h>

// Can construct and return 10k character error string.
void test01()
{
  typedef std::logic_error test_type;

  const std::string xxx(10000, 'x');
  test_type t(xxx);
  VERIFY( std::strcmp(t.what(), xxx.c_str()) == 0 );
}

int main(void)
{
  test01();
  return 0;
}
