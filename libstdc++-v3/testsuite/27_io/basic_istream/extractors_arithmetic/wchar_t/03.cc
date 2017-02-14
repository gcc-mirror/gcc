// Copyright (C) 2004-2017 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

void test03()
{
  std::wstringbuf sbuf;
  std::wistream istr(&sbuf);
  std::wostream ostr(&sbuf);

  long l01;
  ostr << L"12220101";
  istr >> l01; // _M_in_end set completely incorrectly here.
  VERIFY( l01 == 12220101 );
  VERIFY( istr.rdstate() == std::ios_base::eofbit );
}

int main()
{
  test03();
  return 0;
}
