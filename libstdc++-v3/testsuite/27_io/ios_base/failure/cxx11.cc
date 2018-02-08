// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

#include <ios>
#include <testsuite_hooks.h>

using test_type = std::ios_base::failure;

static_assert( std::is_base_of<std::system_error, test_type>::value, "base" );

void
test01()
{
  test_type e("io error");
  VERIFY(std::string(e.what()).find("io error") != std::string::npos);
  e = test_type("", make_error_code(std::io_errc::stream));
}

struct E : test_type
{
  E(const char* s) : test_type(s, make_error_code(std::io_errc::stream)) { }
};

void
test02()
{
  E e("io error");
  VERIFY(std::string(e.what()).find("io error") != std::string::npos);
}

int
main()
{
  test01();
  test02();
}
