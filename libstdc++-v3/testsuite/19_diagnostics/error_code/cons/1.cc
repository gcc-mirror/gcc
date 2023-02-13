// { dg-do run { target c++11 } }
// { dg-additional-options "-static-libstdc++" { target *-*-mingw* } }
// 2007-08-22 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2007-2023 Free Software Foundation, Inc.
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

#include <system_error>
#include <testsuite_error.h>

namespace adl
{
  struct Error { };

  const Error err;

  struct category : std::error_category
  {
    const char* name() const noexcept override { return "adl"; }
    std::string message(int) const { return ""; }
  };

  const category cat;

  std::error_code
  make_error_code(Error) { return std::error_code(999, cat); }
}

template<> struct std::is_error_code_enum<adl::Error> : std::true_type { };

int main()
{
  // 1 error_code()
  std::error_code e1;
  VERIFY( e1.value() == 0 );
  VERIFY( e1.category() == std::system_category() );

  // 2 error_code(int, const error_category&)
  const __gnu_test::test_category cat;
  std::error_code e2(e1.value(), cat);
  VERIFY( e2.value() == e1.value() );
  VERIFY( e2.category() == cat );

  // 3 error_code(const error_code&)
  std::error_code e3(std::make_error_code(std::errc::operation_not_supported));
  VERIFY( e3.value() == int(std::errc::operation_not_supported) );
  VERIFY( e3.category() == std::generic_category() );

  // 4 error_code(ErrorCodeEnum)
  std::error_code e4(adl::err);
  VERIFY( e4.value() == 999 );
  VERIFY( e4.category() == adl::cat );
}
