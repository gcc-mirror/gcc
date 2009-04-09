// 2001-08-23  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2003, 2009 Free Software Foundation
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

// 22.2.6.3  Template class moneypunct

#include <locale>
#include <testsuite_hooks.h>

// Should be able to instantiate this for other types besides char, wchar_t
class gnu_moneypunct_f: public std::moneypunct<char, false> 
{ };

void test03()
{ 
  bool test __attribute__((unused)) = true;
  gnu_moneypunct_f facet02;
  VERIFY (facet02.intl == false);
}

int main()
{
  test03();
  return 0;
}
