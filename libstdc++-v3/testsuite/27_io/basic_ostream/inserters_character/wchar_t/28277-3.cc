// 2006-10-12  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006 Free Software Foundation
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

// 27.6.2.5.4 basic_ostream character inserters

#include <ostream>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/28277
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  wostringstream oss_01;

  oss_01.width(5000000);
  const streamsize width = oss_01.width();

  oss_01 << L'a';

  VERIFY( oss_01.good() );
  VERIFY( oss_01.str().size() == width );
}

int main()
{
  test01();
  return 0;
}
