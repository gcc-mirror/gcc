// 2004-03-15  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004 Free Software Foundation
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 22.2.6.1.1 money_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

struct My_money_io_01 : public std::moneypunct<char, false>
{
  std::string do_curr_symbol() const { return "$"; }
  std::string do_positive_sign() const { return ""; }
  std::string do_negative_sign() const { return ""; }

  pattern do_neg_format() const
  {
    pattern pat = { { value, symbol, none, sign } };
    return pat;
  }
};

struct My_money_io_02 : public std::moneypunct<char, false>
{
  std::string do_curr_symbol() const { return "%"; }
  std::string do_positive_sign() const { return ""; }
  std::string do_negative_sign() const { return "-"; }

  pattern do_neg_format() const
  {
    pattern pat = { { value, symbol, sign, none } };
    return pat;
  }
};

struct My_money_io_03 : public std::moneypunct<char, false>
{
  std::string do_curr_symbol() const { return "&"; }
  std::string do_positive_sign() const { return ""; }
  std::string do_negative_sign() const { return ""; }

  pattern do_neg_format() const
  {
    pattern pat = { { value, space, symbol, sign } };
    return pat;
  }
};

// When both do_positive_sign and do_negative_sign return an empty
// string, patterns of the forms { value, symbol, none, sign },
// { value, symbol, sign, none } and { X, Y, symbol, sign } imply
// that the symbol is not consumed since no other characters are
// needed to complete the format.
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<char> iterator_type;

  bool test __attribute__((unused)) = true;

  // basic construction
  locale loc_01(locale::classic(), new My_money_io_01);
  locale loc_02(locale::classic(), new My_money_io_02);
  locale loc_03(locale::classic(), new My_money_io_03);

  iterator_type end, end01, end02, end03;
  istringstream iss_01, iss_02, iss_03;
  iss_01.imbue(loc_01);
  iss_02.imbue(loc_02);
  iss_03.imbue(loc_03);
  // cache the money_get facet
  const money_get<char>& mon_get_01 =
    use_facet<money_get<char> >(iss_01.getloc());
  const money_get<char>& mon_get_02 =
    use_facet<money_get<char> >(iss_02.getloc());
  const money_get<char>& mon_get_03 =
    use_facet<money_get<char> >(iss_03.getloc());

  iss_01.str("10$"); 
  iterator_type is_it01(iss_01);
  string result01;
  ios_base::iostate err01 = ios_base::goodbit;
  end01 = mon_get_01.get(is_it01, end, false, iss_01, err01, result01);
  VERIFY( err01 == ios_base::goodbit );
  VERIFY( *end01 == '$' );

  iss_02.str("50%");
  iterator_type is_it02(iss_02);
  string result02;
  ios_base::iostate err02 = ios_base::goodbit;
  end02 = mon_get_02.get(is_it02, end, false, iss_02, err02, result02);
  VERIFY( err02 == ios_base::goodbit );
  VERIFY( *end02 == '%' );

  iss_03.str("7 &");
  iterator_type is_it03(iss_03);
  string result03;
  ios_base::iostate err03 = ios_base::goodbit;
  end03 = mon_get_03.get(is_it03, end, false, iss_03, err03, result03);
  VERIFY( err03 == ios_base::goodbit );
  VERIFY( *end03 == '&' );
}

int main()
{
  test01();
  return 0;
}
