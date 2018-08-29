// { dg-do run { target c++11 } }

// 2007-03-12  Stephen M. Webb  <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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

// [28.7] class template regex_traits (5) translate_nocase

#include <regex>
#include <testsuite_hooks.h>

// Verifies the workings of the regex_traits translate_nocase function.
void test01()
{
  typedef wchar_t CharT;

  std::regex_traits<CharT> t;
  CharT c = L'a';
  CharT C = L'A';

  VERIFY( t.translate_nocase(c) == t.translate_nocase(C) );
}

int
main()
{ 
  test01();
  return 0;
}
