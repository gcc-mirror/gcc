// 2001-08-27 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003 Free Software Foundation
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 22.2.6.2.1 money_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

struct My_money_io : public std::moneypunct<char,false>
{
  char_type do_decimal_point() const { return '.'; }
  char_type do_thousands_sep() const { return ','; }
  std::string do_grouping() const { return "\003"; }
  
  std::string do_negative_sign() const { return "()"; }
  
  int do_frac_digits() const { return 2; }

  pattern do_neg_format() const
  {
    pattern pat = { { symbol, space, sign, value } };
    return pat;
  }
};

// libstdc++/5708
void test05()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  typedef ostreambuf_iterator<char> OutIt;

  locale loc(locale::classic(), new My_money_io);

  bool intl = false;

  string val("-123456");
  const money_put<char,OutIt>& mp  =
    use_facet<money_put<char, OutIt> >(loc);

  ostringstream fmt;
  fmt.imbue(loc);
  OutIt out(fmt);
  mp.put(out,intl,fmt,'*',val);
  VERIFY( fmt.str() == "*(1,234.56)" );
}

int main()
{
  test05();
  return 0;
}
