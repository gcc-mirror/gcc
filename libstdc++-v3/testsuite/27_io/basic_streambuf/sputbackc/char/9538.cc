// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

#include <streambuf>
#include <locale>
#include <testsuite_hooks.h>

class MyTraits : public std::char_traits<char>
{
public:
  static bool eq(char c1, char c2)
  {
    bool test __attribute__((unused)) = true;
    VERIFY( c1 != 'X' );
    VERIFY( c2 != 'X' );
    return std::char_traits<char>::eq(c1, c2);
  }
};

class MyBuf : public std::basic_streambuf<char, MyTraits>
{
  char buffer[8];

public:
  MyBuf()
  {
    std::memset(buffer, 'X', sizeof(buffer));
    std::memset(buffer + 2, 'f', 4);
    setg(buffer + 2, buffer + 2, buffer + 6);
  }
};

// libstdc++/9538
void test08()
{
  bool test __attribute__((unused)) = true;

  MyBuf mb;
  mb.sputbackc('a');  
}

int main() 
{
  test08();
  return 0;
}
