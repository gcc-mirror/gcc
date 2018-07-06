// Copyright (C) 2018 Free Software Foundation, Inc.
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
// { dg-require-effective-target cxx11-abi }

// PR libstdc++/83328

#include <string>
#include <testsuite_hooks.h>

void
test01()
{
  std::wstring s = L"insert";
  auto iter = s.insert(s.cbegin() + 2, std::initializer_list<wchar_t>{});
  VERIFY( iter == s.begin() + 2 );

  iter = s.insert(s.cend(), { L'e', L'd' });
  std::wstring::iterator* check_type = &iter;
  VERIFY( iter == s.cend() - 2 );
  VERIFY( s == L"inserted" );

  iter = s.insert(s.begin() + 6, { L' ', L'r', L'e', L't', L'r', L'i' });
  VERIFY( iter == s.begin() + 6 );
  VERIFY( s == L"insert retried" );

}

int main()
{
  test01();
}
