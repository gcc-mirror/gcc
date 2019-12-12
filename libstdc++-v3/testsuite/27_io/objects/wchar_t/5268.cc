// 2003-05-01 Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2001-2019 Free Software Foundation, Inc.
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


#include <sstream>
#include <iostream>

// libstdc++/5268
void test04()
{
  std::wstringbuf b1;
  std::wcout.rdbuf( &b1 );
  std::wcout << L"hello\n";
  std::wcout.rdbuf(0);
}

int main()
{
  test04();
  return 0;
}
