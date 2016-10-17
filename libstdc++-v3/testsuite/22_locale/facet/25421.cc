// 2005-12-15  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2016 Free Software Foundation, Inc.
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

// 22.1.1.1.2 - class locale::facet [lib.locale.facet]

#include <stdexcept>
#include <locale>
#include <testsuite_hooks.h>

// libstdc++/25421
void test01()
{
  using namespace std;

  try
    {
      new codecvt_byname<char, char, mbstate_t>("invalid-loc");
      new collate_byname<char>("invalid-loc");
      new ctype_byname<char>("invalid-loc");
      new messages_byname<char>("invalid-loc");
      new moneypunct_byname<char, true>("invalid-loc");
      new numpunct_byname<char>("invalid-loc");

#ifdef _GLIBCXX_USE_WCHAR_T
      new codecvt_byname<wchar_t, char, mbstate_t>("invalid-loc");
      new collate_byname<wchar_t>("invalid-loc");
      new ctype_byname<wchar_t>("invalid-loc");
      new messages_byname<wchar_t>("invalid-loc");
      new moneypunct_byname<wchar_t, true>("invalid-loc");
      new numpunct_byname<wchar_t>("invalid-loc");
#endif
    }
  catch(const std::runtime_error&)
    {
      // named locale not valid
      VERIFY( true );
    }
  catch(...)
    {
      // some other error
      VERIFY( false );
    }
}

int main()
{
  test01();
  return 0;
}
