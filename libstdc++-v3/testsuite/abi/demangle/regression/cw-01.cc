// 2003-02-26  Carlo Wood  <carlo@alinoe.com>

// Copyright (C) 2003-2017 Free Software Foundation, Inc.
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

// IA 64 C++ ABI - 5.1 External Names (a.k.a. Mangling)

#include <testsuite_hooks.h>

// libcwd tests
int main()
{
  using namespace __gnu_test;

  /*
class libcw_app_ct {
public:
  void dummy(char const*) { }
  template<typename T>
    static void add_option(void (T::*)(char const*), char const*, char, 
                           char const*, char const*);
};

template<typename T>
  void libcw_app_ct::add_option(void (T::*)(char const*), char const*, char, 
                                char const*, char const*)
{ }

void fn(void)
{
  // Instantiation.
  libcw_app_ct::add_option(&libcw_app_ct::dummy, "", '\0', "", "");
}
*/
verify_demangle("_ZN12libcw_app_ct10add_optionIS_EEvMT_FvPKcES3_cS3_S3_",
       "void libcw_app_ct::add_option<libcw_app_ct>(void (libcw_app_ct::*)(char const*), char const*, char, char const*, char const*)");

  return 0;
}
