// 2004-01-11  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2004-2021 Free Software Foundation, Inc.
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

#include <iostream>
#include <string>
#include <locale>

// libstdc++/13582
void test01()
{
  using namespace std;

  ios_base::sync_with_stdio(false);
  wcout << "Type in 12345\n";
  
  wstring str;
  wchar_t c;
  
  if (wcin.get(c) && !isspace(c, wcin.getloc()))
    {
      str.push_back(c);
      wcin.imbue(locale(ISO_8859(1,en_US)));
    }

  if (wcin.get(c) && !isspace(c, wcin.getloc()))
    {
      str.push_back(c);
      wcin.imbue(locale(ISO_8859(15,fr_FR)));
    }

  while (wcin.get(c) && !isspace(c, wcin.getloc()))
    {
      str.push_back(c);
    }
  
  wcout << str << endl;
}

int main()
{
  test01();
  return 0;
}
