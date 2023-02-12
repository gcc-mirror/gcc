// 2001-08-27 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

struct My_money_io_2 : public std::moneypunct<wchar_t,false>
{
  char_type do_thousands_sep() const { return L','; }
  std::string do_grouping() const { return "\001"; }
};

// Make sure we can output a very big amount of money (with grouping too).
void test06()
{
  using namespace std;
  typedef ostreambuf_iterator<wchar_t> OutIt;

  locale loc(locale::classic(), new My_money_io_2);

  bool intl = false;

  long double val = 1.0e50L;
  const money_put<wchar_t, OutIt>& mp  = 
    use_facet<money_put<wchar_t, OutIt> >(loc);

  wostringstream fmt;
  fmt.imbue(loc);
  OutIt out(fmt);
  mp.put(out, intl, fmt, L'*', val);
  VERIFY( fmt.good() );
}

int main()
{
  test06();
  return 0;
}
