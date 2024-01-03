// 2003-05-13 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2003-2024 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <locale>
#include <testsuite_hooks.h>

class state_codecvt : public std::codecvt<wchar_t, char, std::mbstate_t>
{
protected:
  int
  do_encoding() const throw()
  { return -1; }
};

void test03()
{
  using namespace std;

  locale loc_s(locale::classic(), new state_codecvt);
  wfilebuf ob;
  ob.pubimbue(loc_s);
  VERIFY( ob.getloc() == loc_s );

  // 2 "if encoding of current locale is state dependent" and
  // not at the beginning of the file fails...
  locale loc_c = locale::classic();
  locale ret = ob.pubimbue(loc_s);
  VERIFY( ob.getloc() == loc_s );
}

int main() 
{
  test03();
  return 0;
}
