// { dg-require-namedlocale "en_US.ISO8859-1" }

// 2004-04-16  Petur Runolfsson  <peturr02@ru.is>

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

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <locale>
#include <testsuite_hooks.h>

class Buf : public std::filebuf
{
protected:
  virtual int_type
  overflow(int_type c = traits_type::eof())
  {
    return traits_type::eq_int_type(c, traits_type::eof()) ?
      traits_type::eof() : std::filebuf::overflow(c);
  }
};

// libstdc++/14975
void test01()
{
  using namespace std;

  Buf fb;
  locale loc_us = locale(ISO_8859(1,en_US));
  fb.pubimbue(loc_us);
  fb.open("tmp_14975-1", ios_base::out);
  
  try
    {
      fb.sputc('a');
      fb.sputc('b');
      fb.pubimbue(locale::classic());
      fb.sputc('c');
      fb.pubsync();
      fb.close();
    }
  catch (std::exception&)
    {
    }
}

int main()
{
  test01();
  return 0;
}
