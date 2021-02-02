// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <ios>
#include <testsuite_hooks.h>

void
test01()
{
  std::error_code ec, def_ec;
#if _GLIBCXX_USE_CXX11_ABI
  // For the new ABI code() should return the constructor argument.
  ec = std::make_error_code(std::errc::executable_format_error);
  def_ec = std::io_errc::stream;
#else
  // For the old ABI code() always returns a default-constructed error_code.
#endif
  std::ios_base::failure e1("string literal");
  VERIFY( e1.code() == def_ec );
  std::ios_base::failure e2(std::string("std::string"));
  VERIFY( e2.code() == def_ec );
  std::ios_base::failure e3("string literal", ec);
  VERIFY( e3.code() == ec );
  std::ios_base::failure e4(std::string("std::string"), ec);
  VERIFY( e4.code() == ec );
}

int
main()
{
  test01();
}
