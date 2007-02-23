// 1999-05-21 bkoz
// 2000-05-21 Benjamin Kosnik  <bkoz@redhat.com>
// 2001-01-17 Loren J. Rittle  <ljrittle@acm.org>

// Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006
// Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 27.4.2.4 ios_base static members
// @require@ %-*.tst
// @diff@ %-*.tst %-*.txt

// This test fails on platforms using a wrapper, because this test
// redirects stdout to a file and so the exit status printed by the
// wrapper is not visibile to DejaGNU.  DejaGNU then assumes that the
// test exited with a non-zero exit status.
// { dg-do run { xfail { ! unwrapped } } }

#include <cstdio>
#include <sstream>
#include <iostream>
#include <testsuite_hooks.h>

// N.B. Once we have called sync_with_stdio(false), we can never go back.

void
test01()
{
  std::ios_base::sync_with_stdio();
  std::freopen("ios_base_members_static-1.txt", "w", stderr);
 
  for (int i = 0; i < 2; i++)
    {
      std::fprintf(stderr, "1");
      std::cerr << "2";
      std::putc('3', stderr); 
      std::cerr << '4';
      std::fputs("5", stderr);
      std::cerr << 6;
      std::putc('7', stderr);
      std::cerr << 8 << '9';
      std::fprintf(stderr, "0\n");
    }
}

int main(void)
{
  test01();
  return 0;
}
