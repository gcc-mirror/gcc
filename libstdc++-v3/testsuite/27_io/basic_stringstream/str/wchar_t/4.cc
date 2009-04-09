// Copyright (C) 2004, 2009 Free Software Foundation, Inc.
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

// 27.7.6 member functions (stringstream_members)

#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/8466
void test04()
{
  bool test __attribute__((unused)) = true;

  const wchar_t* strlit00 = L"orvieto";
  const std::wstring str00 = strlit00;

  std::wostringstream oss;

  oss.str(str00);
  oss << L"cortona";
  VERIFY( str00 == strlit00 );
}

int main()
{
  test04();
  return 0;
}
