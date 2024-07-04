// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

//
// 2014-01-07  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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

// 28.3 Requirements [re.req]
// 28.2 (4) Table 127 - Regular expression traits class requirements
// 28.7 Class template regex_traits [re.traits]

#include <regex>
#include <string>
#include <testsuite_hooks.h>

using namespace std;

bool called_transform = false;
bool called_nocase = false;

template<typename CharT>
  class MyRegexTraits
  : public regex_traits<CharT>
  {
  public:
    CharT
    translate(CharT c) const
    {
      return c+1;
    }

    CharT
    translate_nocase(CharT c) const
    {
      called_nocase = true;
      return regex_traits<CharT>::translate_nocase(c);
    }

    template<typename FwdIt>
      basic_string<CharT>
      transform(FwdIt begin, FwdIt end) const
      {
	called_transform = true;
	return regex_traits<CharT>::transform(begin, end);
      }
  };

void
test01()
{
  {
    basic_regex<char, MyRegexTraits<char>> re(".");
    VERIFY(!regex_match("\n", re));
    VERIFY(!regex_match("\r", re));
  }
  {
    VERIFY(!called_transform);
    basic_regex<char, MyRegexTraits<char>> re("[a]", regex::collate);
    VERIFY(regex_match("a", re));
    VERIFY(called_transform);
    called_transform = false;
  }
  {
    VERIFY(!called_nocase);
    basic_regex<char, MyRegexTraits<char>> re("[a]", regex::icase);
    VERIFY(regex_match("A", re));
    VERIFY(called_nocase);
    called_nocase = false;
  }
  {
    basic_regex<char, MyRegexTraits<char>> re("[T-f]", regex::icase);
    VERIFY(regex_match("A", re));
    VERIFY(regex_match("F", re));
    VERIFY(regex_match("a", re));
    VERIFY(regex_match("f", re));

    VERIFY(!regex_match("G", re));
    VERIFY(!regex_match("S", re));
    VERIFY(!regex_match("g", re));
    VERIFY(!regex_match("s", re));

    VERIFY(regex_match("T", re));
    VERIFY(regex_match("Z", re));
    VERIFY(regex_match("t", re));
    VERIFY(regex_match("z", re));
  }
  // icase doesn't participate with the presence of collate and user-defined traits.
  {
    basic_regex<char, MyRegexTraits<char>> re("[T-f]", regex::icase | regex::collate);
    VERIFY(!regex_match("A", re));
    VERIFY(!regex_match("S", re));
    VERIFY(regex_match("T", re));
    VERIFY(regex_match("Z", re));
    VERIFY(regex_match("a", re));
    VERIFY(regex_match("f", re));
    VERIFY(!regex_match("g", re));
  }
}

int main()
{
  test01();
  return 0;
}
