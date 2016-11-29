// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }

// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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

#include <ext/vstring.h>
#include <unordered_map>
#include <testsuite_hooks.h>

// libstdc++/47773
void test01()
{
  typedef __gnu_cxx::__vstring vstring_t;
  typedef std::unordered_map<vstring_t, int> map_t;

  map_t mymap;

  mymap.insert(std::make_pair("hello", 10));
  mymap.insert(std::make_pair("hi", 20));

  VERIFY( mymap.size() == 2 );

  map_t::const_iterator imap1 = mymap.begin();
  map_t::const_iterator imap2 = mymap.begin();
  ++imap2;

  VERIFY( ((imap1->first == "hello" && imap1->second == 10
	    && imap2->first == "hi" && imap2->second == 20)
	   || (imap1->first == "hi" && imap1->second == 20
	       && imap2->first == "hello" && imap2->second == 10)) );
}

int main()
{
  test01();
  return 0;
}
