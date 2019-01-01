// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

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

// 27.8.1.4 Overridden virtual functions

// { dg-require-fileio "" }

#include <fstream>
#include <locale>
#include <testsuite_hooks.h>

const char name_07[] = "filebuf_virtuals-7.txt"; // empty file, need to create

class errorcvt : public std::codecvt<char, char, mbstate_t>
{
protected:
  std::codecvt_base::result
  do_out(mbstate_t&, const char* from, const char*,
	 const char*& from_next, char* to, char*,
	 char*& to_next) const
  {
    from_next = from;
    to_next = to;
    return std::codecvt<char, char, mbstate_t>::error;
  }
  
  virtual bool do_always_noconv() const throw()
  {
    return false;
  }
};

// libstdc++/9182
// basic_filebuf<>::sync and overflow do not write out any characters
// if codecvt<>::out returns error
void test14()
{
  using namespace std;

  locale loc =  locale::classic();
  loc = locale(loc, new errorcvt);
  
  filebuf fbuf1;
  fbuf1.pubimbue(loc);
  fbuf1.pubsetbuf(0, 0);
  fbuf1.open(name_07, ios_base::out | ios_base::trunc);

  try
    {
      fbuf1.sputn("onne", 4);
      fbuf1.close();
      VERIFY( false );
    }
  catch (std::exception&)
    {
    }
}

int main() 
{
  test14();
  return 0;
}
