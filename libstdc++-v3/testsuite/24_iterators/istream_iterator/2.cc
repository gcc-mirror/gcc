// 2001-06-25  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2003, 2009 Free Software Foundation, Inc.
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

// 24.5.1 Template class istream_iterator

#include <iterator>
#include <sstream>
#include <testsuite_hooks.h>

void test02()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  string st("R.Rorty");

  string re_01, re_02, re_03;
  re_02 = ",H.Putnam";
  re_03 = "D.Dennett,xxx,H.Putnam";
  
  stringbuf sb_01(st);
  istream is_01(&sb_01);
  istream_iterator<char> inb_01(is_01);
  istream_iterator<char> ine_01;
  re_01.assign(inb_01, ine_01);
  VERIFY( re_01 == "R.Rorty" );

  stringbuf sb_02(st);
  istream is_02(&sb_02);
  istream_iterator<char> inb_02(is_02);
  istream_iterator<char> ine_02;
  re_02.insert(re_02.begin(), inb_02, ine_02);
  VERIFY( re_02 == "R.Rorty,H.Putnam" );

  stringbuf sb_03(st);
  istream is_03(&sb_03);
  istream_iterator<char> inb_03(is_03);
  istream_iterator<char> ine_03;
  re_03.replace(re_03.begin() + 10, re_03.begin() + 13,
		inb_03, ine_03);
  VERIFY( re_03 == "D.Dennett,R.Rorty,H.Putnam" );
}

int main() 
{ 
  test02();
  return 0;
}
