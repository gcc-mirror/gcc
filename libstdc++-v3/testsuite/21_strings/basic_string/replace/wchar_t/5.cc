// 1999-06-10 bkoz

// Copyright (C) 1994, 1999, 2001, 2002, 2003 Free Software Foundation, Inc.
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

// 21.3.5.6 basic_string::replace

#include <string>
#include <testsuite_hooks.h>

// We wrongly used __n1 instead of __foldn1 in the length_error
// check at the beginning of replace(__pos, __n1, __s, __n2)
void
test05()
{
  bool test __attribute__((unused)) = true;
  std::wstring str01 = L"londinium";
  std::wstring str02 = L"cydonia";

  str01.replace(0, 20, str02.c_str(), 3);
  VERIFY(str01 == L"cyd");
}

int main()
{ 
  test05();
  return 0;
}
