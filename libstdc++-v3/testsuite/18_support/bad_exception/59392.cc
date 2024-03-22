// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

// { dg-options "-Wno-deprecated" }
// { dg-do run { target c++14_down } }
// { dg-require-effective-target hosted }

#include <exception>
#include <cstdlib>

class expected {};
class unexpected {};
class from_handler {};

static void func_with_exception_spec() throw(expected)
{
  throw unexpected();
}

static void unexpected_handler()
{
  throw from_handler();
}

static void terminate_handler()
{
  exit(0);
}

// libstdc++/59392
int main()
{
  std::set_unexpected(unexpected_handler);
  std::set_terminate(terminate_handler);
  try {
    func_with_exception_spec();
  } catch (expected&) {
    abort();
  }
  abort();
}
