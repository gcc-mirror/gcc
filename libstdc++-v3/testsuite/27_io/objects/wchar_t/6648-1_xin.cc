// 2003-05-01 Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
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

#include <iostream>

// libstdc++/6648
// Interactive tests: each one (run alone) must terminate upon a single '\n'.
void test08()
{
  bool test __attribute__((unused)) = true;
  wchar_t buff[2048];
  std::wcout << "Enter name: ";
  std::wcin.getline(buff, 2048);
}

int 
main()
{
  test08();
  return 0;
}
