// 2003-11-09 Paolo Carlini <pcarlini@suse.de>

// Copyright (C) 2003 Free Software Foundation
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

// 22.2.6.2.1 money_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/12971
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  typedef ostreambuf_iterator<wchar_t> iterator_type;

  long double amount = 10.8L;
  
  // cache the money_put facet
  wostringstream oss;
  const money_put<wchar_t>& mon_put = use_facet<money_put<wchar_t> >(oss.getloc()); 

  iterator_type os_it01 = mon_put.put(oss.rdbuf(), true, oss, L' ', amount);
  wstring result = oss.str();
  VERIFY( result == L"11" );
}

int main()
{
  test01();
  return 0;
}
