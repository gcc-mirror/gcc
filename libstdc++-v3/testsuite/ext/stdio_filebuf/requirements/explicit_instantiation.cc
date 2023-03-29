// 2003-02-11  Paolo Carlini  <pcarlini@unitus.it>

// Copyright (C) 2003-2023 Free Software Foundation, Inc.
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

// stdio_filebuf.h

#include <ext/stdio_filebuf.h>
#include <testsuite_hooks.h>
#include <testsuite_character.h>

// { dg-do compile }

// libstdc++/9320
namespace __gnu_cxx
{
  typedef short type_t;
  template class stdio_filebuf<type_t, std::char_traits<type_t> >;
  template class stdio_filebuf<__gnu_test::pod_char, std::char_traits<__gnu_test::pod_char> >;
} // __gnu_cxx
