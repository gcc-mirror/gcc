// 2001-09-12 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2018 Free Software Foundation, Inc.
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

struct My_money_io_a : public std::moneypunct<wchar_t,false>
{
  char_type do_decimal_point() const { return '.'; }
  std::string do_grouping() const { return "\004"; }
  
  std::wstring do_curr_symbol() const { return L"$"; }
  std::wstring do_positive_sign() const { return L"()"; }
  
  int do_frac_digits() const { return 2; }

  pattern do_neg_format() const
  {
    pattern pat = { { sign, value, space, symbol } };
    return pat;
  }
};

struct My_money_io_b : public std::moneypunct<wchar_t,false>
{
  char_type do_decimal_point() const { return '.'; }
  std::string do_grouping() const { return "\004"; }
  
  std::wstring do_curr_symbol() const { return L"$"; }
  std::wstring do_positive_sign() const { return L"()"; }
  
  int do_frac_digits() const { return 2; }

  pattern do_neg_format() const
  {
    pattern pat = { { sign, value, symbol, none } };
    return pat;
  }
};

// This one exercises patterns of the type { X, Y, Z, symbol } and
// { X, Y, symbol, none } for a two character long sign. Therefore
// the optional symbol (showbase is false by default) must be consumed
// if present, since "rest of the sign" is left to read.
void test08()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> InIt;

  bool intl = false;
  ios_base::iostate err;

  locale loc_a(locale::classic(), new My_money_io_a);

  wstring buffer_a(L"(1234.56 $)");
  wstring buffer_a_ns(L"(1234.56 )");

  InIt iend_a, iend_a_ns;
  wstring val_a, val_a_ns;

  const money_get<wchar_t,InIt>& mg_a  = use_facet<money_get<wchar_t, InIt> >(loc_a);

  wistringstream fmt_a(buffer_a);
  fmt_a.imbue(loc_a);
  InIt ibeg_a(fmt_a);
  mg_a.get(ibeg_a,iend_a,intl,fmt_a,err,val_a);
  VERIFY( val_a == L"123456" );

  wistringstream fmt_a_ns(buffer_a_ns);
  fmt_a_ns.imbue(loc_a);
  InIt ibeg_a_ns(fmt_a_ns);
  mg_a.get(ibeg_a_ns,iend_a_ns,intl,fmt_a_ns,err,val_a_ns);
  VERIFY( val_a_ns == L"123456" );

  locale loc_b(locale::classic(), new My_money_io_b);

  wstring buffer_b(L"(1234.56$)");
  wstring buffer_b_ns(L"(1234.56)");

  InIt iend_b, iend_b_ns;
  wstring val_b, val_b_ns;

  const money_get<wchar_t,InIt>& mg_b = use_facet<money_get<wchar_t, InIt> >(loc_b);

  wistringstream fmt_b(buffer_b);
  fmt_b.imbue(loc_b);
  InIt ibeg_b(fmt_b);
  mg_b.get(ibeg_b,iend_b,intl,fmt_b,err,val_b);
  VERIFY( val_b == L"123456" );

  wistringstream fmt_b_ns(buffer_b_ns);
  fmt_b_ns.imbue(loc_b);
  InIt ibeg_b_ns(fmt_b_ns);
  mg_b.get(ibeg_b_ns,iend_b_ns,intl,fmt_b_ns,err,val_b_ns);
  VERIFY( val_b_ns == L"123456" );
}

int main()
{
  test08();
  return 0;
}
