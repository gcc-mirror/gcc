// 2001-09-12 Benjamin Kosnik  <bkoz@redhat.com>

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

// 22.2.6.1.1 money_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// We were appending to the string val passed by reference, instead
// of constructing a temporary candidate, eventually copied into
// val in case of successful parsing.
void test07()
{
  using namespace std;

  typedef istreambuf_iterator<char> InIt;
  InIt iend1, iend2, iend3;

  locale loc_c = locale::classic();
  string buffer1("123");
  string buffer2("456");
  string buffer3("Golgafrincham"); // From Nathan's original idea.

  string val;

  ios_base::iostate err;

  const money_get<char, InIt>& mg = use_facet<money_get<char, InIt> >(loc_c);

  istringstream fmt1(buffer1);
  fmt1.imbue(loc_c);
  InIt ibeg1(fmt1);
  mg.get(ibeg1, iend1, false, fmt1, err, val);
  VERIFY( val == buffer1 );

  istringstream fmt2(buffer2);
  fmt2.imbue(loc_c);
  InIt ibeg2(fmt2);
  mg.get(ibeg2, iend2, false, fmt2, err, val);
  VERIFY( val == buffer2 );

  val = buffer3;
  istringstream fmt3(buffer3);
  fmt3.imbue(loc_c);
  InIt ibeg3(fmt3);
  mg.get(ibeg3, iend3, false, fmt3, err, val);
  VERIFY( val == buffer3 );
}

int main()
{
  test07();
  return 0;
}
