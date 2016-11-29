// 2001-09-12 Benjamin Kosnik  <bkoz@redhat.com>

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

// 22.2.6.1.1 money_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

struct My_money_io : public std::moneypunct<char,false>
{
  char_type do_decimal_point() const { return '.'; }
  std::string do_grouping() const { return "\004"; }
  
  std::string do_curr_symbol() const { return "$"; }
  std::string do_positive_sign() const { return ""; }
  std::string do_negative_sign() const { return "-"; }
  
  int do_frac_digits() const { return 2; }

  pattern do_neg_format() const
  {
    pattern pat = { { symbol, none, sign, value } };
    return pat;
  }
};

// libstdc++/5579
void test06()
{
  using namespace std;
  typedef istreambuf_iterator<char> InIt;

  locale loc(locale::classic(), new My_money_io);

  string bufferp("$1234.56");
  string buffern("$-1234.56");
  string bufferp_ns("1234.56");
  string buffern_ns("-1234.56");

  bool intl = false;

  InIt iendp, iendn, iendp_ns, iendn_ns;
  ios_base::iostate err;
  string valp, valn, valp_ns, valn_ns;

  const money_get<char,InIt>& mg  =
    use_facet<money_get<char, InIt> >(loc);

  istringstream fmtp(bufferp);
  fmtp.imbue(loc);
  InIt ibegp(fmtp);
  mg.get(ibegp,iendp,intl,fmtp,err,valp);
  VERIFY( valp == "123456" );

  istringstream fmtn(buffern);
  fmtn.imbue(loc);
  InIt ibegn(fmtn);
  mg.get(ibegn,iendn,intl,fmtn,err,valn);
  VERIFY( valn == "-123456" );

  istringstream fmtp_ns(bufferp_ns);
  fmtp_ns.imbue(loc);
  InIt ibegp_ns(fmtp_ns);
  mg.get(ibegp_ns,iendp_ns,intl,fmtp_ns,err,valp_ns);
  VERIFY( valp_ns == "123456" );

  istringstream fmtn_ns(buffern_ns);
  fmtn_ns.imbue(loc);
  InIt ibegn_ns(fmtn_ns);
  mg.get(ibegn_ns,iendn_ns,intl,fmtn_ns,err,valn_ns);
  VERIFY( valn_ns == "-123456" );
}

int main()
{
  test06();
  return 0;
}
