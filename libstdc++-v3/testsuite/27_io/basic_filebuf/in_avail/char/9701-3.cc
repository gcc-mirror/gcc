// Copyright (C) 2003-2017 Free Software Foundation, Inc.
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

// 27.5.2.2.3 Get area

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

class Derived_fbuf : public std::filebuf
{
public:
  const char_type* pub_egptr() const
  { return egptr(); }

  const char_type* pub_gptr() const
  { return gptr(); }
};

// libstdc++/9701 (in_avail)
void test01()
{
  using namespace std;
  const char* name = "tmp_file1";

  Derived_fbuf df2;
  df2.open(name, ios_base::in | ios_base::out | ios_base::trunc);

  df2.sputn("Comomoc", 7);

  df2.pubseekoff(0, ios_base::beg);
  df2.sbumpc();
  df2.sputbackc('t');

  VERIFY( df2.pub_gptr() < df2.pub_egptr() );
  VERIFY( df2.in_avail() == df2.pub_egptr() - df2.pub_gptr() );
}

int
main()
{
  test01();
  return 0;
}
