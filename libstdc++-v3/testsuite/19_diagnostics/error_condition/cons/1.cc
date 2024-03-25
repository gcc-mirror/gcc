// { dg-do run { target c++11 } }
// { dg-additional-options "-static-libstdc++" { target *-*-mingw* } }

// Copyright (C) 2008-2024 Free Software Foundation, Inc.
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

  std::error_condition
  make_error_condition(Error) { return std::error_condition(999, cat); }
}

template<> struct std::is_error_condition_enum<adl::Error> : std::true_type { };

void test01()
{
  // 1 error_condition()
  std::error_condition e1;
  VERIFY( e1.value() == 0 );
  VERIFY( e1.category() == std::generic_category() );

  // 2 error_condition(int, const error_category&)
  const __gnu_test::test_category cat;
  std::error_condition e2(e1.value(), cat);
  VERIFY( e2.value() == e1.value() );
  VERIFY( e2.category() == cat );

  // 3 error_condition(const error_condition&)
  std::error_condition e3(std::errc::operation_not_supported);
  VERIFY( e3.value() == int(std::errc::operation_not_supported) );
  VERIFY( e3.category() == std::generic_category() );

  // 4 error_condition(ErrorConditionEnum)
  std::error_condition e4(adl::err);
  VERIFY( e4.value() == 999 );
  VERIFY( e4.category() == adl::cat );
}

int main()
{
  test01();
  return 0;
}
