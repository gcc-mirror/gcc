// { dg-do run { target c++11 } }

// Copyright (C) 2009-2020 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

enum my_errc { my_err = 0 };

class my_error_category_impl
: public std::error_category
{
public:
  const char* name() const noexcept { return ""; }
  std::string message(int) const { return ""; }
} my_error_category_instance;

std::error_condition
make_error_condition(my_errc e)
{
  return std::error_condition(static_cast<int>(e),
			      my_error_category_instance);
}

namespace std
{
  template<>
    struct is_error_condition_enum<my_errc>
    : public true_type { };
}

// libstdc++/39881
void test01()
{
  std::error_condition ec2;
  ec2 = my_err;
  VERIFY( ec2 == make_error_condition(my_err) );
}

int main()
{
  test01();
  return 0;
}
