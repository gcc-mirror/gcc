// { dg-do run { target c++14 } }

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

// 27.7.6 - Quoted manipulators		[quoted.manip]

#include <sstream>
#include <iomanip>
#include <testsuite_hooks.h>

void
test01()
{
  std::wostringstream ssx;
  ssx << L"[" << std::left << std::setfill(L'x') << std::setw(20) << LR"("AB \"CD\" EF")" << L"]";
  VERIFY( ssx.str() == LR"(["AB \"CD\" EF"xxxxxx])" );

  std::wostringstream ssy;
  ssy << L"[" << std::left << std::setfill(L'y') << std::setw(20) << std::quoted(LR"(GH "IJ" KL)") << L"]";
  VERIFY( ssy.str() == LR"(["GH \"IJ\" KL"yyyyyy])" );

  std::wostringstream ssz;
  ssz << L"[" << std::right << std::setfill(L'z') << std::setw(20) << std::quoted(LR"(PQ "RS" TU)") << L"]";
  VERIFY( ssz.str() == LR"([zzzzzz"PQ \"RS\" TU"])" );
}

int
main()
{
  test01();
  return 0;
}
