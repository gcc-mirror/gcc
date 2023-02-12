// 2005-09-15  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2023 Free Software Foundation, Inc.
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

// 27.6.2.5.2  Arithmetic inserters

#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_character.h>

using __gnu_test::pod_ushort;

namespace std
{
  template<>
    basic_ostream<pod_ushort>&
    basic_ostream<pod_ushort>::
    operator<<(long)
    {
      VERIFY( false );
      return *this;
    }

  template<>
    basic_ostream<pod_ushort>&
    basic_ostream<pod_ushort>::
    operator<<(unsigned long)
    {
      VERIFY( false );
      return *this;
    }
  
  template<>
    basic_ostream<pod_ushort>&
    basic_ostream<pod_ushort>::
    operator<<(double)
    {
      VERIFY( false );
      return *this;
    }
}

// libstdc++/23875
void test01()
{
  std::basic_ostringstream<pod_ushort> ostr;

  short s = 1;
  ostr << s;

  unsigned short us = 1;
  ostr << us;

  int i = 1;
  ostr << i;

  unsigned int ui = 1;
  ostr << ui;

  float f = 1.0f;
  ostr << f;
}

int main()
{
  test01();
  return 0;
}
