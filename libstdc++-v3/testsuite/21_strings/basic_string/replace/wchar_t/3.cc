// 1999-06-10 bkoz

// Copyright (C) 1994, 1999, 2001, 2002, 2003 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 21.3.5.6 basic_string::replace

#include <string>
#include <testsuite_hooks.h>

// Some more miscellaneous tests
void
test03()
{
  bool test __attribute__((unused)) = true;
  const wchar_t* title01 = L"nine types of ambiguity";
  const wchar_t* title02 = L"ultra";
  std::wstring str01 = title01;
  std::wstring str02 = title02;

  str01.replace(0, 4, str02);
  VERIFY(str01 == L"ultra types of ambiguity");

  str01.replace(15, 9, str02, 2, 2);
  VERIFY(str01 == L"ultra types of tr");

  str01 = title01;
  str02.replace(0, 0, str01, 0, std::wstring::npos);
  VERIFY(str02 == L"nine types of ambiguityultra");

  str02.replace(11, 2, title02, 5);
  VERIFY(str02 == L"nine types ultra ambiguityultra");

  str02.replace(11, 5, title01, 2);
  VERIFY(str02 == L"nine types ni ambiguityultra");

  str01.replace(str01.size(), 0, title02);
  VERIFY(str01 == L"nine types of ambiguityultra");
  
  str01 = title01;
  str02 = title02;
  str01.replace(str01.begin(), str01.end(), str02);
  VERIFY(str01 == L"ultra");

  str01.replace(str01.begin(), str01.begin(), title01, 4);
  VERIFY(str01 == L"nineultra");

  str01.replace(str01.end(), str01.end(), title01 + 5, 5);
  VERIFY(str01 == L"nineultratypes");
  
  str01.replace(str01.begin(), str01.end(), title02);
  VERIFY(str01 == L"ultra");
}

int main()
{ 
  test03();
  return 0;
}
