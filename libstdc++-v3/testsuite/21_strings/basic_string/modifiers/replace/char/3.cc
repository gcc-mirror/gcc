// 1999-06-10 bkoz

// Copyright (C) 1994-2024 Free Software Foundation, Inc.
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

// 21.3.5.6 basic_string::replace

#include <string>
#include <testsuite_hooks.h>

// Some more miscellaneous tests
void
test03()
{
  const char* title01 = "nine types of ambiguity";
  const char* title02 = "ultra";
  std::string str01 = title01;
  std::string str02 = title02;

  str01.replace(0, 4, str02);
  VERIFY(str01 == "ultra types of ambiguity");

  str01.replace(15, 9, str02, 2, 2);
  VERIFY(str01 == "ultra types of tr");

  str01 = title01;
  str02.replace(0, 0, str01, 0, std::string::npos);
  VERIFY(str02 == "nine types of ambiguityultra");

  str02.replace(11, 2, title02, 5);
  VERIFY(str02 == "nine types ultra ambiguityultra");

  str02.replace(11, 5, title01, 2);
  VERIFY(str02 == "nine types ni ambiguityultra");

  str01.replace(str01.size(), 0, title02);
  VERIFY(str01 == "nine types of ambiguityultra");
  
  str01 = title01;
  str02 = title02;
  str01.replace(str01.begin(), str01.end(), str02);
  VERIFY(str01 == "ultra");

  str01.replace(str01.begin(), str01.begin(), title01, 4);
  VERIFY(str01 == "nineultra");

  str01.replace(str01.end(), str01.end(), title01 + 5, 5);
  VERIFY(str01 == "nineultratypes");
  
  str01.replace(str01.begin(), str01.end(), title02);
  VERIFY(str01 == "ultra");
}

int main()
{ 
  test03();
  return 0;
}
