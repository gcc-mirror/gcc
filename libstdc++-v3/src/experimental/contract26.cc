// -*- C++ -*- std::experimental::contract_violation and friends

// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

#include "../../include/std/contracts"
#if _GLIBCXX_HOSTED && _GLIBCXX_VERBOSE
# include <iostream>
# include <cxxabi.h>
#endif

void __handle_contract_violation(const std::contracts::contract_violation &violation) noexcept
{
#if _GLIBCXX_HOSTED && _GLIBCXX_VERBOSE

  std::cerr << "contract violation in function " << violation.location().function_name()
    << " at " << violation.location().file_name() << ':' << violation.location().line()
    << ": " << violation.comment();

  const char* delimiter = "\n[";

  std::cerr << delimiter << "assertion_kind:";
   switch (violation.kind())
   {
     case std::contracts::assertion_kind::pre:
       std::cerr << " pre";
       break;
     case std::contracts::assertion_kind::post:
       std::cerr << " post";
       break;
     case std::contracts::assertion_kind::assert:
       std::cerr << " assert";
       break;
     case std::contracts::assertion_kind::cassert:
       std::cerr << " cassert";
       break;
     case std::contracts::assertion_kind::manual:
       std::cerr << " manual";
       break;
     default:
       std::cerr << " unknown: " << (int) violation.semantic();
   }
   delimiter = ", ";

  std::cerr << delimiter << "semantic:";
  switch (violation.semantic())
  {
    case std::contracts::evaluation_semantic::enforce:
      std::cerr << " enforce";
      break;
    case std::contracts::evaluation_semantic::observe:
      std::cerr << " observe";
      break;
    default:
      std::cerr << " unknown: " << (int) violation.semantic();
  }
  delimiter = ", ";

  std::cerr << delimiter << "mode:";
  switch (violation.mode())
  {
    case std::contracts::detection_mode::predicate_false:
      std::cerr << " predicate_false";
      break;
    case std::contracts::detection_mode::evaluation_exception:
      std::cerr << " evaluation_exception";
      break;
    case std::contracts::detection_mode::unspecified:
      std::cerr << " unspecified";
      break;
    default:
      std::cerr << " unknown";
  }
  delimiter = ", ";

  if (violation.evaluation_exception ())
    {
      /* Based on the impl. in vterminate.cc.  */
      std::type_info *t = __cxxabiv1::__cxa_current_exception_type();
      if (t)
	{
	  int status = -1;
	  char *dem = 0;
	  // Note that "name" is the mangled name.
	  char const *name = t->name();
	  dem = __cxxabiv1::__cxa_demangle(name, 0, 0, &status);
	  std::cerr << ": threw an instance of '";
	  std::cerr << ( status == 0 ? dem : name) << "'";
	}
      else
	std::cerr << ": threw an unknown type";
    }
  std::cerr << delimiter << "terminating:"
	    << (violation.is_terminating () ? " yes" : " no");

  if (delimiter[0] == ',')
    std::cerr << ']';

  std::cerr << std::endl;
#endif
}

__attribute__ ((weak)) void
handle_contract_violation (const std::contracts::contract_violation &violation)
{
  return __handle_contract_violation(violation);
}

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

namespace contracts
{

void invoke_default_contract_violation_handler(const std::contracts::contract_violation& violation) noexcept
{
  __handle_contract_violation(violation);
}

// From P3290
[[noreturn]] void handle_enforced_contract_violation(
      const char* __comment,
      const std::source_location &__location)
{
  contract_violation __violation{evaluation_semantic::enforce, __location, __comment};
  handle_contract_violation(__violation);
  std::terminate();
}

[[noreturn]] void handle_enforced_contract_violation(
    const std::nothrow_t&,
    const char* __comment,
    const std::source_location &__location) noexcept
{
  contract_violation __violation{evaluation_semantic::enforce, __location, __comment};
  handle_contract_violation(__violation);
  std::terminate();
}


void handle_observed_contract_violation(
      const char* __comment,
      const std::source_location &__location)
{
  contract_violation __violation{evaluation_semantic::observe, __location, __comment};
  handle_contract_violation(__violation);
}

void handle_observed_contract_violation(
    const std::nothrow_t&,
    const char* __comment,
    const std::source_location &__location) noexcept
{
  contract_violation __violation{evaluation_semantic::observe, __location, __comment};
  handle_contract_violation(__violation);
}

[[noreturn]] void handle_quick_enforced_contract_violation(
    const char*,
    const std::source_location &) noexcept
{
  std::terminate();
}

}
}


#if _GLIBCXX_INLINE_VERSION
// The compiler expects the contract_violation class to be in an unversioned
// namespace, so provide a forwarding function with the expected symbol name.
extern "C" void
_Z25handle_contract_violationRKNSt9contracts18contract_violationE
(const std::contracts::contract_violation &violation)
{ handle_contract_violation(violation); }

extern "C" void
_Z27__handle_contract_violationRKNSt9contracts18contract_violationE
(const std::contracts::contract_violation &violation)
{ __handle_contract_violation(violation); }

extern "C" void
_Z41invoke_default_contract_violation_handlerRKNSt9contracts18contract_violationE
(const std::contracts::contract_violation &violation)
{ invoke_default_contract_violation_handler(violation); }

#endif
