// Copyright (C) 2004 Free Software Foundation, Inc.
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

// 21.3.6 string operations

#include <string>
#include <testsuite_hooks.h>

int test01(void)
{
  bool test __attribute__((unused)) = true;

  std::wstring empty;

  // data() for size == 0 is non-NULL.
  VERIFY( empty.size() == 0 );
  const std::wstring::value_type* p = empty.data();
  VERIFY( p != NULL );

  return 0;
}

int main()
{ 
  test01();
  return 0;
}
