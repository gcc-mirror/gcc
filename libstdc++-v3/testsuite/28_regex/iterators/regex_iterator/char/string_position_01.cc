// { dg-do run { target c++11 } }

//
// 2013-07-25  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2013-2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 28.12.1 regex_iterator
// Tests iter->position() behavior

#include <regex>
#include <tuple>
#include <testsuite_hooks.h>

void
test01()
{
  std::regex re("asdf");
  std::string s("asdfasdfasdf");
  int i = 0;
  for (std::sregex_iterator it(s.begin(), s.end(), re);
       it != std::sregex_iterator();
       ++it, i++) {
      VERIFY( it->position() == 4 * i );
  }
}

// PR libstdc++/64239
void
test02()
{
  std::regex re("\\w+");
  std::string s("-a-b-c-");

  std::tuple<int, int, const char*> expected[] =
  {
    std::make_tuple(1, 1, "a"),
    std::make_tuple(3, 1, "b"),
    std::make_tuple(5, 1, "c"),
  };

  int i = 0;
  for (auto it1 = std::sregex_iterator(s.begin(), s.end(), re),
       end = std::sregex_iterator(); it1 != end; ++it1, i++)
    {
      auto it2 = it1;
      VERIFY(it1->position() == std::get<0>(expected[i]));
      VERIFY(it1->length() == std::get<1>(expected[i]));
      VERIFY(it1->str() == std::get<2>(expected[i]));
      VERIFY(it2->position() == std::get<0>(expected[i]));
      VERIFY(it2->length() == std::get<1>(expected[i]));
      VERIFY(it2->str() == std::get<2>(expected[i]));
    }
}

void
test03()
{
  std::smatch m;
  std::string s = "abcde";
  std::regex_search(s, m, std::regex("bcd"));
  VERIFY(m.position() == 1);
  VERIFY(m.position() == m.prefix().length());
}

int
main()
{
  test01();
  test02();
  test03();
  return 0;
}
