// 1999-10-11 bkoz

// Copyright (C) 1999-2020 Free Software Foundation, Inc.
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


// 27.5.2 template class basic_streambuf

#include <streambuf>
#include <string>
#include <testsuite_hooks.h>

class nullsetpbuf : public std::streambuf
{
  char foo[64];
public:
  nullsetpbuf()
  {
    setp(foo, foo + 64);
    setp(0, 0);
  }
};

// libstdc++/1057
void test05()
{
  std::string text1 = "abcdefghijklmn";
  
  nullsetpbuf nsp;
  // Immediate crash as xsputn writes to null pointer
  nsp.sputn(text1.c_str(), text1.length());
}

int main() 
{
  test05();
  return 0;
}
