// { dg-require-namedlocale "de_DE.ISO8859-15" }
// { dg-require-namedlocale "en_US.ISO8859-1" }
// { dg-require-namedlocale "fr_FR.ISO8859-15" }

// 2001-08-15 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2018 Free Software Foundation, Inc.
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

// 22.2.4.1.1 collate members

#include <testsuite_hooks.h>

#define main discard_main_1
#include "1.cc"
#undef main 

#define main discard_main_2
#include "2.cc"
#undef main 

int main()
{
  using namespace __gnu_test;
  func_callback two;
  two.push_back(&test01);
  two.push_back(&test02);
  run_tests_wrapped_env(ISO_8859(15,de_DE), "LANG", two);
  return 0;
}
