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

#include <ostream>
#include <fstream>
#include <locale>
#include <string>
#include <testsuite_hooks.h>

// libstdc++/12868
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  locale loc_is(__gnu_test::try_named_locale("is_IS.UTF-8"));
  
  {
    wofstream out("tmp_12868");
    out << L"<? xml version=\"1.0\" encoding=\"UTF-8\" ?>\n";
    out.imbue(loc_is);
    VERIFY( out.rdbuf()->getloc() == loc_is );
    out << L"<greeting>Hall\u00f3 heimur</greeting>\n";
  }

  {
    wifstream in("tmp_12868");
    wstring str;
    getline(in, str);
    if (str.find(L"encoding=\"UTF-8\"") != wstring::npos)
      {
	in.imbue(loc_is);
	VERIFY( in.rdbuf()->getloc() == loc_is );
      }
    getline(in, str);
    VERIFY( str == L"<greeting>Hall\u00f3 heimur</greeting>" );
  }
}

int main()
{
  test01();
}
