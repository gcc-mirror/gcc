// Copyright (C) 2025 Free Software Foundation, Inc.
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

// { dg-options "-g0 -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=quick_enforce" }
// { dg-do run { target c++2a } }

#include <exception>
#include <cstdlib>
#include <testsuite_hooks.h>

#include "../../../include/std/contracts"

void my_term()
{
  std::exit(0);
}


int main()
{
  std::set_terminate (my_term);

  std::contracts::handle_enforced_contract_violation(std::nothrow, "test comment");
  // We should not get here
  return 1;
}
// { dg-output "contract violation in function int main.* at .*:37: test comment.*" }
// { dg-output "assertion_kind: manual, semantic: enforce, mode: unspecified, terminating: yes" }
