// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

#include <system_error>
#include <locale>
#include <testsuite_hooks.h>

void
test01()
{
  const char* name = std::generic_category().name();
  VERIFY( name == std::string("generic") );
}

void
test02()
{
  const std::error_category& cat = std::generic_category();
  std::error_condition cond;

  cond = cat.default_error_condition(EBADF);
  VERIFY( cond.value() == EBADF );
  VERIFY( cond == std::errc::bad_file_descriptor );
  VERIFY( cond.category() == std::generic_category() );
  cond = cat.default_error_condition(EACCES);
  VERIFY( cond.value() == EACCES );
  VERIFY( cond == std::errc::permission_denied );
  VERIFY( cond.category() == std::generic_category() );

  // PR libstdc++/60555
  VERIFY( std::error_code(EBADF, cat) == std::errc::bad_file_descriptor );
  VERIFY( std::error_code(EACCES, cat) == std::errc::permission_denied );
}

void
test03()
{
  // set "C" locale to get expected message
  auto loc = std::locale::global(std::locale::classic());

  std::string msg = std::generic_category().message(EBADF);
  VERIFY( msg.find("file") != std::string::npos );

  std::locale::global(loc);
}

int
main()
{
  test01();
  test02();
  test03();
}
