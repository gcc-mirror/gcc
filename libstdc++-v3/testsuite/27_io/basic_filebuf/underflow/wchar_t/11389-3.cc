// Copyright (C) 2003 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <testsuite_hooks.h>

const char name_03[] = "tmp_11389-3";

void test03()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  filebuf fbout;
  fbout.open(name_03, ios_base::out);
  fbout.sputc('a');
  fbout.close();
  
  wfilebuf fbin;
  locale loc(__gnu_test::try_named_locale("en_US.UTF-8"));
  fbin.pubimbue(loc);
  fbin.pubsetbuf(0, 0);
  fbin.open(name_03, ios_base::in);
  VERIFY( fbin.sbumpc() == L'a' );
  VERIFY( fbin.sgetc() == wfilebuf::traits_type::eof() );
  fbin.close();
}

int main()
{
  test03();
  return 0;
}
