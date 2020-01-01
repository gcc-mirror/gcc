// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

// 22.3.3.1 Character classification [classification]

#include <locale>
#include <testsuite_hooks.h>

typedef char char_type;

void
test01()
{
  VERIFY( std::isblank(' ', std::locale::classic()) );
  VERIFY( std::isblank('\t', std::locale::classic()) );
}

void
test02()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  VERIFY( std::isblank(L' ', std::locale::classic()) );
  VERIFY( std::isblank(L'\t', std::locale::classic()) );
#endif
}

int main()
{
  test01();
  test02();
}
