// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2017 Free Software Foundation, Inc.
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

const char name_04[] = "filebuf_virtuals-4.txt"; // empty file, need to create

class Cvt_to_upper : public std::codecvt<char, char, mbstate_t>
{
  bool do_always_noconv() const throw()
  {
    return false;
  }
};

// libstdc++/9169
// filebuf output fails if codecvt<>::out returns noconv
void test10()
{
  using namespace std;

  locale c_loc = locale::classic();
  locale loc(c_loc, new Cvt_to_upper);

  string str("abcdefghijklmnopqrstuvwxyz");
  string tmp;

  {
    ofstream out;
    out.imbue(loc);
    out.open(name_04);
    copy(str.begin(), str.end(), ostreambuf_iterator<char>(out));
  }

  {
    ifstream in;
    in.open(name_04);
    copy(istreambuf_iterator<char>(in), istreambuf_iterator<char>(),
	 back_inserter(tmp));
  }

  VERIFY( tmp.size() == str.size() );
  VERIFY( tmp == str );
}

int main() 
{
  test10();
  return 0;
}
