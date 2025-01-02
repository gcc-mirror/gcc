// Copyright (C) 2004-2025 Free Software Foundation, Inc.
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

// 27.6.1.2.2 arithmetic extractors

#include <istream>
#include <sstream>
#include <locale>
#include <cwchar>
#include <testsuite_hooks.h>

// In the presence of no fmtflags, the input operator should behave
// like strtol(x, y, 0)
// libstdc++/90
void test11()
{
  const wchar_t* cstrlit = L"0x2a";

  // sanity check via 'C' library call
  wchar_t* err;
  long l = std::wcstol(cstrlit, &err, 0);

  std::wistringstream iss(cstrlit);
  iss.setf(std::wios::fmtflags(0), std::ios::basefield);
  int i;
  iss >> i;

  VERIFY( !iss.fail() );
  VERIFY( l == i );
}

int main()
{
  test11();
  return 0;
}
