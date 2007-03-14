// 2003-05-03  Petur Runolfsson  <peturr02@ru.is>

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

// 27.8.1.4 Overridden virtual functions

// { dg-require-fileio "" }

#include <fstream>
#include <string>
#include <testsuite_hooks.h>

class MyTraits : public std::char_traits<char>
{
};

// libstdc++/10096
void test01()
{
  using namespace std;

  const char* name = "filebuf_virtuals-1.txt";

  string str;
  filebuf fb1;
  fb1.open(name, ios_base::in);
  
  filebuf::int_type c1;
  while ((c1 = fb1.sbumpc()) != filebuf::traits_type::eof())
    str.push_back(filebuf::traits_type::to_char_type(c1));
  fb1.close();

  basic_filebuf<char, MyTraits> fb;
  VERIFY( fb.sgetc() == MyTraits::eof() );

  fb.open(name, ios_base::in);
  VERIFY( fb.is_open() );

  for (string::iterator i = str.begin(); i != str.end(); ++i)
    {
      MyTraits::int_type c2 = fb.sbumpc();
      VERIFY( c2 != MyTraits::eof() );
      VERIFY( c2 == MyTraits::to_int_type(*i) );
    }

  VERIFY( fb.sgetc() == MyTraits::eof() );
  fb.close();
  VERIFY( !fb.is_open() );
  VERIFY( fb.sgetc() == MyTraits::eof() );
}

int main()
{
  test01();
  return 0;
}
