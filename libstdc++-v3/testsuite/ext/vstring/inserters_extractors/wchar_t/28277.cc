// 2007-04-09  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007 Free Software Foundation
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

#include <ostream>
#include <sstream>
#include <ext/vstring.h>
#include <testsuite_hooks.h>

// libstdc++/28277
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  wostringstream oss_01;
  const __gnu_cxx::__wvstring str_01(50, L'a');

  oss_01.width(5000000);
  const streamsize width = oss_01.width();

  oss_01 << str_01;

  VERIFY( oss_01.good() );
  VERIFY( oss_01.str().size() == __gnu_cxx::__wvstring::size_type(width) );
}

int main()
{
  test01();
  return 0;
}
