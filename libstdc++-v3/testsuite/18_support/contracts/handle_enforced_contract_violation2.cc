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

// { dg-options "-g0 -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=observe" }
// { dg-do run { target c++2a } }

#include <contracts>
#include <exception>
#include <testsuite_hooks.h>

struct MyException{};

void handle_contract_violation(const std::contracts::contract_violation& v)
{
  invoke_default_contract_violation_handler(v);
  throw MyException{};
}


int main()
{
  bool exception_thrown = false;
  try {
      std::contracts::handle_enforced_contract_violation("test comment");
  }
  catch(MyException)
  {
      exception_thrown = true;
  }
  VERIFY( exception_thrown == true);
}
// { dg-output "contract violation in function int main.* at .*:38: test comment.*" }
// { dg-output "assertion_kind: manual, semantic: enforce, mode: unspecified, terminating: yes" }
