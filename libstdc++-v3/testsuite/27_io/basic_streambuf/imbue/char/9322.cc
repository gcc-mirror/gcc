// { dg-require-namedlocale "en_US.ISO8859-1" }
// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 1999-10-11 bkoz

// Copyright (C) 1999-2025 Free Software Foundation, Inc.
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
#include <locale>
#include <testsuite_hooks.h>

class testbuf : public std::streambuf
{
public:
  typedef std::streambuf::traits_type traits_type;

  testbuf() : std::streambuf() { }
};

// libstdc++/9322
void test08()
{
  using std::locale;

  locale loc;
  testbuf ob;
  VERIFY( ob.getloc() == loc );

  locale::global(locale(ISO_8859(1,en_US)));
  VERIFY( ob.getloc() == loc );

  locale loc_de = locale(ISO_8859(15,de_DE));
  locale ret = ob.pubimbue(loc_de);
  VERIFY( ob.getloc() == loc_de );
  VERIFY( ret == loc );

  locale::global(loc);
  VERIFY( ob.getloc() == loc_de );
}

int main() 
{
  test08();
  return 0;
}
