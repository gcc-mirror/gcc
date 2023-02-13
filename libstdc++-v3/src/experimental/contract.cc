// -*- C++ -*- std::experimental::contract_violation and friends

// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

#include <experimental/contract>
#if _GLIBCXX_HOSTED && _GLIBCXX_VERBOSE
# include <iostream>
#endif

__attribute__ ((weak)) void
handle_contract_violation (const std::experimental::contract_violation &violation)
{
#if _GLIBCXX_HOSTED && _GLIBCXX_VERBOSE
  bool level_default_p = violation.assertion_level() == "default";
  bool role_default_p = violation.assertion_role() == "default";
  bool cont_mode_default_p = violation.continuation_mode()
    == std::experimental::contract_violation_continuation_mode::never_continue;

  const char* modes[]{ "off", "on" }; // Must match enumerators in header.
  std::cerr << "contract violation in function " << violation.function_name()
    << " at " << violation.file_name() << ':' << violation.line_number()
    << ": " << violation.comment();

  const char* delimiter = "\n[";

  if (!level_default_p)
    {
      std::cerr << delimiter << "level:" << violation.assertion_level();
      delimiter = ", ";
    }
  if (!role_default_p)
    {
      std::cerr << delimiter << "role:" << violation.assertion_role();
      delimiter = ", ";
    }
  if (!cont_mode_default_p)
    {
      std::cerr << delimiter << "continue:"
		<< modes[(int)violation.continuation_mode() & 1];
      delimiter = ", ";
    }

  if (delimiter[0] == ',')
    std::cerr << ']';

  std::cerr << std::endl;
#endif
}
