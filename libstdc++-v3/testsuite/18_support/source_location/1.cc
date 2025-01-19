// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// Example from C++ Standard Working Draft N4842, November 2019 Mailing
// Adapted for testing.

// { dg-options "-include string -include stdexcept" }
// { dg-do run { target c++20 } }
// { dg-add-options no_pch }

#include <source_location>

#ifndef __cpp_lib_source_location
# error "Feature-test macro for source_location missing in <source_location>"
#elif __cpp_lib_source_location != 201907L
# error "Feature-test macro for source_location has wrong value in <source_location>"
#endif

#include <string_view>
#include <testsuite_hooks.h>
#include "srcloc.h"


struct s {
  std::source_location member = std::source_location::current();
  int other_member = 1;

  s(std::source_location loc = std::source_location::current())
    : member(loc) // values of member refer to calling function
  { }

  s(int blather) : // values of member refer to this location
    other_member(blather)
  { }
};

std::source_location
f(std::source_location a = std::source_location::current());

std::source_location
f(std::source_location a)
{ return a; }

auto
g()
{
  struct srcloc_and_line
  {
    std::source_location sl;
    unsigned line;
  };

  std::source_location c = std::source_location::current();
  return srcloc_and_line{ f(c), __LINE__ - 1 };
}

int main ()
{
  std::source_location main_sl = std::source_location::current();
  unsigned main_sl_line = __LINE__ - 1;
  std::source_location f_arg_sl = f(main_sl);
  unsigned f_arg_sl_line = main_sl_line;
  auto [g_sl, g_sl_line] = g();
  std::source_location f_sl = f();
  unsigned f_sl_line = __LINE__ - 1;
  std::source_location h_sl = h(); // defined in ./srcloc.h
  s member_main_sl(main_sl);
  s member_defaulted_sl(1);
  s member_sl = s{};
  const unsigned member_sl_line = __LINE__ - 1;

  using namespace std::string_view_literals;

  std::string_view main_sl_fn_name(main_sl.function_name());
  std::string_view main_sl_fi_name(main_sl.file_name());
  VERIFY(main_sl.line() == main_sl_line);
  // opening paren of call
  VERIFY(main_sl.column() == 63);
  VERIFY(main_sl_fn_name.ends_with("main()"sv));
  VERIFY(main_sl_fi_name.ends_with("1.cc"sv));

  std::string_view f_arg_sl_fn_name(f_arg_sl.function_name());
  std::string_view f_arg_sl_fi_name(f_arg_sl.file_name());
  VERIFY(f_arg_sl.line() == f_arg_sl_line);
  // opening paren of call
  VERIFY(f_arg_sl.column() == 63);
  VERIFY(f_arg_sl_fn_name.ends_with("main()"sv));
  VERIFY(f_arg_sl_fi_name.ends_with("1.cc"sv));

  std::string_view g_sl_fn_name(g_sl.function_name());
  std::string_view g_sl_fi_name(g_sl.file_name());
  VERIFY(g_sl.line() == g_sl_line);
  VERIFY(g_sl.column() == 57); // opening paren of call
  VERIFY(g_sl_fn_name.ends_with("g()"sv));
  VERIFY(g_sl_fi_name.ends_with("1.cc"sv));

  std::string_view h_sl_fn_name(h_sl.function_name());
  std::string_view h_sl_fi_name(h_sl.file_name());
  VERIFY(h_sl.line() == 23);
  VERIFY(h_sl.column() == 57); // opening paren of call
  VERIFY(h_sl_fn_name.ends_with("h()"sv));
  VERIFY(h_sl_fi_name.ends_with("srcloc.h"sv));

  std::string_view member_main_sl_fn_name(member_main_sl.member.function_name());
  std::string_view member_main_sl_fi_name(member_main_sl.member.file_name());
  VERIFY(member_main_sl.member.line() == main_sl_line);
  VERIFY(member_main_sl.member.column() == 63);
  VERIFY(member_main_sl_fn_name.ends_with("main()"sv));
  VERIFY(member_main_sl_fi_name.ends_with("1.cc"sv));

  std::string_view member_defaulted_sl_fi_name(
    member_defaulted_sl.member.file_name());
  std::string_view member_defaulted_sl_fn_name(
    member_defaulted_sl.member.function_name());
  VERIFY(member_defaulted_sl.member.line() == 47);
  // closing paren of constructor declaration
  VERIFY(member_defaulted_sl.member.column() == 25);
  VERIFY(member_defaulted_sl_fn_name.starts_with("s::s(int)"sv));
  VERIFY(member_defaulted_sl_fi_name.ends_with("1.cc"sv));

  std::string_view member_sl_fi_name(
    member_sl.member.file_name());
  std::string_view member_sl_fn_name(
    member_sl.member.function_name());
  VERIFY(member_sl.member.line() == member_sl_line);
  // closing brace/paren of constructor
  VERIFY(member_sl.member.column() == 19);
  VERIFY(member_sl_fn_name.starts_with("int main()"sv));
  VERIFY(member_sl_fi_name.ends_with("1.cc"sv));

  std::string_view f_sl_fi_name(f_sl.file_name());
  std::string_view f_sl_fn_name(f_sl.function_name());
  VERIFY(f_sl.line() == f_sl_line);
  // opening paren of call
  VERIFY(f_sl.column() == 32);
  VERIFY(f_sl_fn_name.ends_with("main()"sv));
  VERIFY(f_sl_fi_name.ends_with("1.cc"sv));

  return 0;
}
