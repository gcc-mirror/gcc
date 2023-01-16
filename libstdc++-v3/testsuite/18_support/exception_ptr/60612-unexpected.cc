// { dg-options "-Wno-deprecated-declarations" }
// { dg-do run { target { c++11_only || c++14_only } } }

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

// PR libstdc++/60612

#include <exception>
#include <stdlib.h>

#if _GLIBCXX_USE_C99_STDLIB
void unexpected() { _Exit(0); }

void f() throw()
{
  try {
    throw 1;
  } catch (...) {
    std::set_unexpected(unexpected);
    std::rethrow_exception(std::current_exception());
  }
}
#endif

int main()
{
#if _GLIBCXX_USE_C99_STDLIB
  f();
#endif
}
