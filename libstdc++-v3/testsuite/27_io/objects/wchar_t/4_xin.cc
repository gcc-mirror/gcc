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

// http://gcc.gnu.org/ml/libstdc++/2002-08/msg00060.html
// Should only have to hit enter once.
void
test10()
{
  using namespace std;
  wcout << L"Press ENTER once\n";
  wcin.ignore(1);
  wcout << L"_M_gcount: " << wcin.gcount() << endl;
}

int 
main()
{
  test10();
  return 0;
}
