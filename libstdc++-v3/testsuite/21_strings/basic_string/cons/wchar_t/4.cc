// 1999-06-04 bkoz

// Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
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

// 21.3.1 basic_string constructors.

#include <string>
#include <testsuite_hooks.h>

// http://gcc.gnu.org/ml/libstdc++/2002-06/msg00025.html
void test04()
{
  bool test __attribute__((unused)) = true;

  std::wstring str01(L"portofino");

  std::wstring::reverse_iterator i1 = str01.rbegin();
  std::wstring::reverse_iterator i2 = str01.rend();
  std::wstring str02(i1, i2);
  VERIFY( str02 == L"onifotrop" );
}

int main()
{ 
  test04();
  return 0;
}
