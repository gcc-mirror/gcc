// 2003-05-01 Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2000-2013 Free Software Foundation, Inc.
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

// libstdc++/5280
// Interactive test: input "1234^D^D" for i should terminate for EOF.
void test06()
{
  using namespace std;
  int i;
  wcin >> i;
  if (!wcin.good()) 
    {
      wcerr << endl;
      wcerr << L"i == " << i << endl;
      wcerr << L"wcin.rdstate() == " << wcin.rdstate() << endl;
      wcerr << L"wcin.bad() == " << wcin.bad() << endl;      
      wcerr << L"wcin.fail() == " << wcin.fail() << endl;      
      wcerr << L"wcin.eof() == " << wcin.eof() << endl;
    }   
  else
    wcerr << L"i == " << i << endl;
}

int 
main()
{
  test06();
  return 0;
}
