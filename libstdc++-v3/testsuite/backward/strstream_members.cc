// { dg-options "-Wno-deprecated" }
// Copyright (C) 2002-2021 Free Software Foundation, Inc.
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

// backward strstream members

#include <strstream>
#include <testsuite_hooks.h>

void test01()
{
   std::strstream s;
   for (unsigned i=0 ; i!= 1000 ; ++i)
      s << i << std::endl;
   s << std::ends;
}


void test02()
{
  std::ostrstream buf;
  buf << std::ends;
  char *s = buf.str ();
  delete [] s;
}

int main()
{
  test01();
  test02();
  return 0;
}
