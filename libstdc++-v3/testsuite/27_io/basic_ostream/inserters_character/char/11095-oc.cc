// 2003-06-05  Paolo Carlini  <pcarlini@unitus.it>

// Copyright (C) 2003-2021 Free Software Foundation, Inc.
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

// 27.6.2.5.4 basic_ostream character inserters

#include <ostream>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/11095
// operator<<(basic_ostream&, const basic_string&)
void
test03() 
{
  std::ostringstream oss_01;

  oss_01.width(-60);
  oss_01 << std::string("Consoli");
  VERIFY( oss_01.good() );
  VERIFY( oss_01.str() == "Consoli" );
}

int main()
{
  test03();
  return 0;
}
