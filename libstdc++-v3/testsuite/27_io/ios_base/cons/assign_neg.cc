// { dg-do compile }

// Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

#include <ios>

// Library defect report
//50.  Copy constructor and assignment operator of ios_base
class test_base : public std::ios_base { }; // { dg-error "within this context|deleted" } 

void test01()
{
  // assign
  test_base io1;
  test_base io2;
  io1 = io2; // { dg-error "synthesized|deleted" }
}

// { dg-prune-output "include" }
