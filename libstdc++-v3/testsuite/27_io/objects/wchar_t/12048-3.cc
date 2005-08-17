// Derived from libstdc++/12048 by LJR <ljrittle@acm.org> with
// reminder from Petur Runolfsson <peturr02@ru.is>.

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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <iostream>
#include <cwchar>
#include <testsuite_hooks.h>

void
test01()
{
  std::freopen("cin_unget-1.txt", "r", stdin);

  wchar_t buf[2];
  VERIFY( std::wcin.rdbuf()->sgetn(buf, 2) == 2 );
  std::wint_t c1 = std::wcin.rdbuf()->sungetc();
  std::wint_t c2 = std::wcin.rdbuf()->sbumpc();
  VERIFY( c1 == std::char_traits<wchar_t>::to_int_type(buf[1]) );
  VERIFY( c2 == c1 );
}

int main(void)
{
  test01();
  return 0;
}
