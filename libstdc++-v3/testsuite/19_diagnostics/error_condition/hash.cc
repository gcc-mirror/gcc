// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

#include <system_error>
#include <testsuite_hooks.h>

struct error_cat : std::error_category
{
  error_cat(std::string s) : _name(s) { }
  std::string _name;
  const char* name() const noexcept override { return _name.c_str(); }
  std::string message(int) const override { return "error"; }
};

void
test01()
{
  std::hash<std::error_condition> h;
  error_cat kitty("kitty"), moggy("moggy");
  std::error_condition cond1(99, kitty);
  VERIFY( h(cond1) == h(cond1) );
  std::error_condition cond2(99, kitty);
  VERIFY( h(cond1) == h(cond2) );
  std::error_condition cond3(88, kitty);
  VERIFY( h(cond1) != h(cond3) );
  std::error_condition cond4(99, moggy);
  VERIFY( h(cond1) != h(cond4) );
}

int
main()
{
  test01();
}
