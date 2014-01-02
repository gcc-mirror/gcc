// 2004-02-21  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2014 Free Software Foundation, Inc.
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

struct My_money : public std::moneypunct<wchar_t, false>
{
  std::wstring do_positive_sign() const { return L"+"; }
  std::wstring do_negative_sign() const { return L""; }
};

void test01()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> InIt;

  bool test __attribute__((unused)) = true;

  locale loc(locale::classic(), new My_money);

  wstring buffer(L"69");

  InIt iend;
  ios_base::iostate err;
  wstring val;

  const money_get<wchar_t, InIt>& mg  =
    use_facet<money_get<wchar_t, InIt> >(loc);

  wistringstream fmt(buffer);
  fmt.imbue(loc);
  InIt ibeg(fmt);
  mg.get(ibeg, iend, false, fmt, err, val);
  VERIFY( val == L"-69" );
}

int main()
{
  test01();
  return 0;
}
