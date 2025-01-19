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
  const char* name = std::system_category().name();
  VERIFY( name == std::string("system") );
}

void
test02()
{
  const std::error_category& cat = std::system_category();
  std::error_condition cond;

#if defined __MING32__ || defined __MINGW64__
  cond = cat.default_error_condition(8); // ERROR_NOT_ENOUGH_MEMORY
  VERIFY( cond.value() == ENOMEM );
  VERIFY( cond.category() == std::generic_category() );
  VERIFY( cond == std::errc::not_enough_memory );

  cond = cat.default_error_condition(5); // ERROR_ACCESS_DENIED
  VERIFY( cond.value() == EACCES );
  VERIFY( cond.category() == std::generic_category() );
  VERIFY( cond == std::errc::permission_denied );
  return;
#endif

  // As of 2011, ISO C only defines EDOM, EILSEQ and ERANGE:
  cond = cat.default_error_condition(EDOM);
  VERIFY( cond.value() == EDOM );
  VERIFY( cond == std::errc::argument_out_of_domain );
  VERIFY( cond.category() == std::generic_category() );
  cond = cat.default_error_condition(EILSEQ);
  VERIFY( cond.value() == EILSEQ );
  VERIFY( cond == std::errc::illegal_byte_sequence );
  VERIFY( cond.category() == std::generic_category() );
  cond = cat.default_error_condition(ERANGE);
  VERIFY( cond.value() == ERANGE );
  VERIFY( cond == std::errc::result_out_of_range );
  VERIFY( cond.category() == std::generic_category() );

  // EBADF and EACCES are defined on all targets,
  // according to config/os/*/error_constants.h
  cond = cat.default_error_condition(EBADF);
  VERIFY( cond.value() == EBADF );
  VERIFY( cond == std::errc::bad_file_descriptor );
  VERIFY( cond.category() == std::generic_category() );
  cond = cat.default_error_condition(EACCES);
  VERIFY( cond.value() == EACCES );
  VERIFY( cond == std::errc::permission_denied );
  VERIFY( cond.category() == std::generic_category() );

  // All POSIX errno values are positive:
  cond = cat.default_error_condition(-1);
  VERIFY( cond.value() == -1 );
  VERIFY( cond.category() == cat );
  cond = cat.default_error_condition(-99);
  VERIFY( cond.value() == -99 );
  VERIFY( cond.category() == cat );

  // PR libstdc++/60555
  VERIFY( std::error_code(EDOM, cat) == std::errc::argument_out_of_domain );
  VERIFY( std::error_code(EILSEQ, cat) == std::errc::illegal_byte_sequence );
  VERIFY( std::error_code(ERANGE, cat) == std::errc::result_out_of_range );
  VERIFY( std::error_code(EBADF, cat) == std::errc::bad_file_descriptor );
  VERIFY( std::error_code(EACCES, cat) == std::errc::permission_denied );

  // As shown at https://gcc.gnu.org/ml/libstdc++/2018-08/msg00018.html
  // these two error codes might have the same value on AIX, but we still
  // expect both to be matched by system_category and so use generic_category:
#ifdef EEXIST
  cond = cat.default_error_condition(EEXIST);
  VERIFY( cond.value() == EEXIST );
  VERIFY( cond == std::errc::file_exists );
  VERIFY( cond.category() == std::generic_category() );
  VERIFY( std::error_code(EEXIST, cat) == std::errc::file_exists );
#endif
#ifdef ENOTEMPTY
  cond = cat.default_error_condition(ENOTEMPTY);
  VERIFY( cond.value() == ENOTEMPTY );
  VERIFY( cond == std::errc::directory_not_empty );
  VERIFY( cond.category() == std::generic_category() );
  VERIFY( std::error_code(ENOTEMPTY, cat) == std::errc::directory_not_empty );
#endif
}

void
test03()
{
  // set "C" locale to get expected message
  auto loc = std::locale::global(std::locale::classic());

#if defined __MING32__ || defined __MINGW64__
  std::string msg = std::system_category().message(5); // ERROR_ACCESS_DENIED
  VERIFY(msg == "Access denied");
#else
  std::string msg = std::system_category().message(EBADF);
  VERIFY( msg.find("file") != std::string::npos );
#endif

  std::locale::global(loc);
}

int
main()
{
  test01();
  test02();
  test03();
}
