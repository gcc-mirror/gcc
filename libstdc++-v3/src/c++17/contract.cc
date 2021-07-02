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
#include <iostream>
#include <cstdlib>

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
