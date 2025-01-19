// { dg-do run { target c++23 } }
// Copyright (C) 2023-2025 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>
#include <generator>

std::generator<int>
foo()
{
  co_yield 0;
  throw 3; /* dice roll */
}

std::generator<int>
foo_delegator()
{
  co_yield 1;
  co_yield std::ranges::elements_of { foo() };
}

bool catchy_caught = false;

std::generator<int>
foo_catchy_delegator()
{
  try
    {
      co_yield std::ranges::elements_of { foo() };
      VERIFY(false);
    }
  catch (int i)
    {
      catchy_caught = true;
      VERIFY(i == 3);
    }
}

int
main()
{
  {
    auto gen = foo();
    try
      {
	auto it = gen.begin();
	VERIFY(*it == 0);
	it++;
	VERIFY(false);
      }
    catch (int x)
      {
	VERIFY(x == 3);
      }
  }

  {
    auto gen = foo_delegator();
    auto it = gen.begin();
    VERIFY(*it == 1);
    it++;

    try
      {
	VERIFY(*it == 0);
	it++;
	VERIFY(false);
      }
    catch (int x)
      {
	VERIFY(x == 3);
      }
  }

  for (auto x : foo_catchy_delegator())
    VERIFY(x == 0);
  VERIFY(catchy_caught);
}
