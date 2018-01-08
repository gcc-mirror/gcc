// 2003-05-03  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2018 Free Software Foundation, Inc.
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
#include <string>
#include <testsuite_hooks.h>

// Check that basic_filebuf::underflow() handles
// codecvt::always_noconv() == false and codecvt::in() == noconv.
class NoconvCvt : public std::codecvt<char, char, std::mbstate_t>
{
protected:
  virtual bool
  do_always_noconv() const throw()
  { return false; }

  virtual result
  do_in(state_type&, const char* from, const char*, const char*& from_next,
	char* to, char*, char*& to_next)
  {
    from_next = from;
    to_next = to;
    return noconv;
  }
};

void test01()
{
  using namespace std;
  const char* name = "filebuf_virtuals-1.txt";

  string str;
  filebuf fb;
  filebuf::int_type c1;

  if (fb.open(name, ios_base::in))
    {  
      while ((c1 = fb.sbumpc()) != EOF)
	str.push_back(filebuf::traits_type::to_char_type(c1));
      fb.close();
    }

  locale loc(locale::classic(), new NoconvCvt);
  fb.pubimbue(loc);

  if (fb.open(name, ios_base::in))
    {
      for (string::iterator i = str.begin(); i != str.end(); ++i)
	{
	  c1 = fb.sbumpc();
	  VERIFY( c1 != filebuf::traits_type::eof() );
	  VERIFY( c1 == filebuf::traits_type::to_int_type(*i) );
	}
      VERIFY( fb.sgetc() == filebuf::traits_type::eof() );
      fb.close();
    }
}

int main()
{
  test01();
  return 0;
}
