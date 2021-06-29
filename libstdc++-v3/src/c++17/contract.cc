// -*- C++ -*- std::contract_violation and friends
// Copyright (C) 1994-2018 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <contract>
#include <exception>
#include <iostream>
#include <cstdlib>
#include <cstdio>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
contract_violation::contract_violation (int line_number,
  string_view file_name,
  string_view function_name,
  string_view comment,
  string_view assertion_level,
  string_view assertion_role,
  contract_violation_continuation_mode continuation_mode)
  : line_number_(line_number),
    file_name_(file_name),
    function_name_(function_name),
    comment_(comment),
    assertion_level_(assertion_level),
    assertion_role_(assertion_role),
    continuation_mode_(continuation_mode)
  { }

int
contract_violation::line_number () const noexcept
{
  return this->line_number_;
}

string_view
contract_violation::file_name () const noexcept
{
  return this->file_name_;
}

string_view
contract_violation::function_name () const noexcept
{
  return this->function_name_;
}

string_view
contract_violation::comment () const noexcept
{
  return this->comment_;
}

string_view
contract_violation::assertion_level () const noexcept
{
  return this->assertion_level_;
}

string_view
contract_violation::assertion_role () const noexcept
{
  return this->assertion_role_;
}

contract_violation_continuation_mode
contract_violation::continuation_mode () const noexcept
{
  return this->continuation_mode_;
}

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std


__attribute__ ((weak)) void
handle_contract_violation (const std::contract_violation &violation)
{
  std::cerr << "default std::handle_contract_violation called: " << std::endl
    << " " << violation.file_name()
    << " " << violation.line_number()
    << " " << violation.function_name()
    << " " << violation.comment()
    << " " << violation.assertion_level()
    << " " << violation.assertion_role()
    << " " << (int)violation.continuation_mode()
    << std::endl;
}

// We take POD types here to make synthesis easier
int
__on_contract_violation (bool continue_,
                         int line_number,
                         const char *file_name,
                         const char *function_name,
                         const char *comment,
                         const char *assertion_level,
                         const char *assertion_role,
                         int continuation_mode)
{
  using cvmc = std::contract_violation_continuation_mode;
  std::contract_violation violation (line_number,
				     file_name,
				     function_name,
				     comment,
				     assertion_level,
				     assertion_role,
				     static_cast<cvmc>(continuation_mode));
  handle_contract_violation (violation);

  if (!continue_)
    std::abort ();

  return 0;
}

