// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

#include <string>
#include <testsuite_hooks.h>

#ifndef __cpp_lib_erase_if
# error "Feature-test macro for erase_if missing in <string>"
#elif __cpp_lib_erase_if < 202002
# error "Feature-test macro for erase_if has wrong value in <string>"
#endif

void
test01()
{
  auto is_vowel = [](const char c)
  {
    return c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u';
  };

  std::string str("cute fluffy kittens");
  auto num = std::erase_if(str, is_vowel);
  VERIFY( str == "ct flffy kttns" );
  VERIFY( num == 5 );
}

void
test02()
{
  std::string str = "cute fluffy kittens";
  auto num = std::erase(str, 'f');
  VERIFY( str == "cute luy kittens" );
  VERIFY( num == 3 );
  num = std::erase(str, 'z');
  VERIFY( str == "cute luy kittens" );
  VERIFY( num == 0 );
}

int
main()
{
  test01();
  test02();

  return 0;
}
