// { dg-do run { target c++11 } }
//
// 2013-02-11  Jason  Merrill
//
// Copyright (C) 2013-2017 Free Software Foundation, Inc.
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

// 18.5 - Start and termination

#if defined(_GLIBCXX_HAVE_AT_QUICK_EXIT) && defined(_GLIBCXX_HAVE_QUICK_EXIT)
#include <cstdlib>

void handler()
{
  std::_Exit(0);
}

void wrong_handler()
{
  std::abort();
}

int main()
{
  std::at_quick_exit (handler);
  std::atexit (wrong_handler);
  std::quick_exit (1);
}
#else
int main() {}
#endif
