// 2005-06-28  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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

struct My_money_io : public std::moneypunct<char, false>
{
  std::string do_grouping() const { return "\1"; }
  char_type do_thousands_sep() const { return '#'; }
  
  pattern do_neg_format() const
  {
    pattern pat = { { symbol, none, sign, value } };
    return pat;
  }
};

// libstdc++/22131
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<char> InIt;

  locale loc(locale::classic(), new My_money_io);

  string buffer1("00#0#1");
  string buffer2("000##1");

  bool intl = false;

  InIt iend1, iend2;
  ios_base::iostate err1, err2;
  string val1, val2;

  const money_get<char,InIt>& mg =
    use_facet<money_get<char, InIt> >(loc);

  istringstream fmt1(buffer1);
  fmt1.imbue(loc);
  InIt ibeg1(fmt1);
  err1 = ios_base::goodbit;
  mg.get(ibeg1, iend1, intl, fmt1, err1, val1);
  VERIFY( err1 == (ios_base::eofbit | ios_base::failbit) );
  VERIFY( val1 == "1" );

  istringstream fmt2(buffer2);
  fmt2.imbue(loc);
  InIt ibeg2(fmt2);
  err2 = ios_base::goodbit;
  ibeg2 = mg.get(ibeg2, iend2, intl, fmt2, err2, val2);
  VERIFY( err2 == ios_base::failbit );
  VERIFY( *ibeg2 == '#' );
  VERIFY( val2 == "" );
}

int main()
{
  test01();
  return 0;
}
