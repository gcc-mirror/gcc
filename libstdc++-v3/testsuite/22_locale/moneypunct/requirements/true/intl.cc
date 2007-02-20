// 2001-08-23  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2003 Free Software Foundation
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 22.2.6.3  Template class moneypunct

#include <locale>
#include <testsuite_hooks.h>

// Should be able to instantiate this for other types besides char, wchar_t
class gnu_moneypunct_t: public std::moneypunct<char, true> 
{ };

void test03()
{ 
  bool test __attribute__((unused)) = true;
  gnu_moneypunct_t facet01;
  VERIFY (facet01.intl == true);
}

int main()
{
  test03();
  return 0;
}
