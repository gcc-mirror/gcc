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
#include <locale>
#include <testsuite_hooks.h>

class Cvt : public std::codecvt<char, char, std::mbstate_t>
{
protected:
  int do_encoding() const throw()
  { return -1; }

  bool do_always_noconv() const throw()
  { return false; }
};

// libstdc++/13171
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  filebuf fb;
  fb.pubimbue(locale(__gnu_test::try_named_locale("en_US"),
		     new Cvt));
  fb.open("tmp_13171-4", ios_base::out);
  fb.pubimbue(__gnu_test::try_named_locale("fr_FR"));
  fb.sputc('N');
  fb.pubsync();
  fb.close();
}

int main()
{
  test01();
  return 0;
}
