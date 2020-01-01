// 2015-01-02  Bernd Edlinger  <bernd.edlinger@hotmail.de>

// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

/* { dg-do link } */
/* { dg-options "-O0" } */

#include <string>

int
main ()
{
  std::string x;
  x.insert (x.begin (), 10, 'x');
  const char test[] = "test";
  x.replace (x.begin (), x.end (), test, 4);
  x.replace (x.begin (), x.end (), test);
  char best[] = "best";
  x.replace (x.begin (), x.end (), best, best + 4);
  x.replace (x.begin (), x.end (), x);
  x.erase (x.begin (), x.end ());
#ifdef _GLIBCXX_USE_WCHAR_T
  std::wstring w;
  w.insert (w.begin (), 10, L'x');
  const wchar_t west[] = L"west";
  w.replace (w.begin (), w.end (), west, 4);
  w.replace (w.begin (), w.end (), west);
  wchar_t rest[] = L"rest";
  w.replace (w.begin (), w.end (), rest, rest + 4);
  w.replace (w.begin (), w.end (), w);
  w.erase (w.begin (), w.end ());
#endif
  return 0;
}
