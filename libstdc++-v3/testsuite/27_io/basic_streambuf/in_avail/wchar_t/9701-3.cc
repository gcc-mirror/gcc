// Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 27.5.2.2.3 Get area

#include <fstream>
#include <testsuite_hooks.h>

class Derived_fbuf : public std::wfilebuf
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
  bool test __attribute__((unused)) = true;
  const char* name = "tmp_file1_w";

  Derived_fbuf df2;
  df2.open(name, ios_base::in | ios_base::out | ios_base::trunc);

  df2.sputn(L"Comomoc", 7);

  df2.pubseekoff(0, ios_base::beg);
  df2.sbumpc();
  df2.sputbackc(L't');

  VERIFY( df2.pub_gptr() < df2.pub_egptr() );
  VERIFY( df2.in_avail() == df2.pub_egptr() - df2.pub_gptr() );
}

int
main()
{
  test01();
  return 0;
}
