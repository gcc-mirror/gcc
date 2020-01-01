// Copyright (C) 2004-2020 Free Software Foundation, Inc.
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

// 27.7.3.2 member functions (ostringstream_members)

#include <sstream>
#include <testsuite_hooks.h>

// user-reported error
class derived_oss: public std::wostringstream 
{
public:
  derived_oss() : std::wostringstream() { }
};

void
test04()
{
  derived_oss yy;
  yy << L"buena vista social club\n";
  VERIFY( yy.str() == std::wstring(L"buena vista social club\n") );
}

int main()
{
  test04();
  return 0;
}
