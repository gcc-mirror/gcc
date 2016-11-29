// 2001-08-27 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2016 Free Software Foundation, Inc.
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

// 22.2.6.2.1 money_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

struct My_money_io : public std::moneypunct<wchar_t,false>
{
  char_type do_decimal_point() const { return L'.'; }
  char_type do_thousands_sep() const { return L','; }
  std::string do_grouping() const { return "\003"; }
  
  std::wstring do_negative_sign() const { return L"()"; }
  
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
  typedef ostreambuf_iterator<wchar_t> OutIt;

  locale loc(locale::classic(), new My_money_io);

  bool intl = false;

  wstring val(L"-123456");
  const money_put<wchar_t, OutIt>& mp  =
    use_facet<money_put<wchar_t, OutIt> >(loc);

  wostringstream fmt;
  fmt.imbue(loc);
  OutIt out(fmt);
  mp.put(out, intl, fmt, L'*', val);
  VERIFY( fmt.str() == L"*(1,234.56)" );
}

int main()
{
  test05();
  return 0;
}
