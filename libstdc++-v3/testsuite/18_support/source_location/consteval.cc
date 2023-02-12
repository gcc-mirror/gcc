// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <source_location>
#include <string_view>

struct s {
  std::source_location member = std::source_location::current();
  int other_member = 1;

  constexpr s(std::source_location loc = std::source_location::current())
    : member(loc) // values of member refer to calling function
  { }

  constexpr s(int blather) : // values of member refer to this location
    other_member(blather)
  { }
};

constexpr std::source_location
f(std::source_location a = std::source_location::current())
{ return a; }

constexpr auto
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

#include "srcloc.h"

int main ()
{
  constexpr std::source_location main_sl = std::source_location::current();
  constexpr unsigned main_sl_line = __LINE__ - 1;
  constexpr std::source_location f_arg_sl = f(main_sl);
  constexpr unsigned f_arg_sl_line = main_sl_line;
  constexpr std::source_location g_sl = g().sl;
  constexpr unsigned g_sl_line = g().line;
  constexpr std::source_location f_sl = f();
  constexpr unsigned f_sl_line = __LINE__ - 1;
  constexpr std::source_location h_sl = h(); // defined in ./srcloc.h
  constexpr s member_main_sl(main_sl);
  constexpr s member_defaulted_sl(1);
  constexpr s member_sl = s{};
  constexpr unsigned member_sl_line = __LINE__ - 1;

  using namespace std::string_view_literals;

  static_assert (std::source_location::current ().line () == __LINE__);
  static_assert (std::source_location::current ().column () == 48);


  constexpr std::string_view main_sl_fn_name(main_sl.function_name());
  constexpr std::string_view main_sl_fi_name(main_sl.file_name());
  static_assert(main_sl.line() == main_sl_line);
  // opening paren of call
  static_assert(main_sl.column() == 73);
  static_assert(main_sl_fn_name.ends_with("main()"sv));
  static_assert(main_sl_fi_name.ends_with("consteval.cc"sv));

  constexpr std::string_view f_arg_sl_fn_name(f_arg_sl.function_name());
  constexpr std::string_view f_arg_sl_fi_name(f_arg_sl.file_name());
  static_assert(f_arg_sl.line() == f_arg_sl_line);
  // opening paren of call
  static_assert(f_arg_sl.column() == 73);
  static_assert(f_arg_sl_fn_name.ends_with("main()"sv));
  static_assert(f_arg_sl_fi_name.ends_with("consteval.cc"sv));

  constexpr std::string_view g_sl_fn_name(g_sl.function_name());
  constexpr std::string_view g_sl_fi_name(g_sl.file_name());
  static_assert(g_sl.line() == g_sl_line);
  static_assert(g_sl.column() == 57); // opening paren of call
  static_assert(g_sl_fn_name.ends_with("g()"sv));
  static_assert(g_sl_fi_name.ends_with("consteval.cc"sv));

  constexpr std::string_view h_sl_fn_name(h_sl.function_name());
  constexpr std::string_view h_sl_fi_name(h_sl.file_name());
  static_assert(h_sl.line() == 23);
  static_assert(h_sl.column() == 57); // opening paren of call
  static_assert(h_sl_fn_name.ends_with("h()"sv));
  static_assert(h_sl_fi_name.ends_with("srcloc.h"sv));

  constexpr std::string_view member_main_sl_fn_name(member_main_sl.member.function_name());
  constexpr std::string_view member_main_sl_fi_name(member_main_sl.member.file_name());
  static_assert(member_main_sl.member.line() == main_sl_line);
  static_assert(member_main_sl.member.column() == 73);
  static_assert(member_main_sl_fn_name.ends_with("main()"sv));
  static_assert(member_main_sl_fi_name.ends_with("consteval.cc"sv));

  constexpr std::string_view member_defaulted_sl_fi_name(
    member_defaulted_sl.member.file_name());
  constexpr std::string_view member_defaulted_sl_fn_name(
    member_defaulted_sl.member.function_name());
  static_assert(member_defaulted_sl.member.line() == 36);
  // closing paren of constructor declaration
  static_assert(member_defaulted_sl.member.column() == 25);
  static_assert(member_defaulted_sl_fn_name.ends_with("s::s(int)"sv));
  static_assert(member_defaulted_sl_fi_name.ends_with("consteval.cc"sv));

  constexpr std::string_view member_sl_fi_name(
    member_sl.member.file_name());
  constexpr std::string_view member_sl_fn_name(
    member_sl.member.function_name());
  static_assert(member_sl.member.line() == member_sl_line);
  // closing brace/paren of constructor
  static_assert(member_sl.member.column() == 29);
  static_assert(member_sl_fn_name.starts_with("int main()"sv));
  static_assert(member_sl_fi_name.ends_with("consteval.cc"sv));

  constexpr std::string_view f_sl_fi_name(f_sl.file_name());
  constexpr std::string_view f_sl_fn_name(f_sl.function_name());
  static_assert(f_sl.line() == f_sl_line);
  // opening paren of call
  static_assert(f_sl.column() == 42);
  static_assert(f_sl_fn_name.ends_with("main()"sv));
  static_assert(f_sl_fi_name.ends_with("consteval.cc"sv));

  return 0;
}
