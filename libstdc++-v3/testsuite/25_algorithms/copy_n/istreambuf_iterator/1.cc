// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <algorithm>
#include <sstream>
#include <iterator>

#include <testsuite_hooks.h>

void test01()
{
  std::stringstream ss("12345");

  std::string ostr(5, '0');
  typedef std::istreambuf_iterator<char> istrb_ite;
  auto res = std::copy_n(istrb_ite(ss), 0, ostr.begin());
  VERIFY( res == ostr.begin() );
  VERIFY( ostr.front() == '0' );

  res = std::copy_n(istrb_ite(ss), 2, ostr.begin());
  VERIFY( res == ostr.begin() + 2 );
  VERIFY( ostr == "12000" );

  res = std::copy_n(istrb_ite(ss), 3, ostr.begin() + 2);
  VERIFY( res == ostr.begin() + 5 );
  VERIFY( ostr == "12345" );
}

void test02()
{
  std::stringstream ss("12345");

  std::string ostr(5, '0');
  typedef std::istreambuf_iterator<char> istrb_ite;

  istrb_ite ibfit(ss);
  auto res = std::copy_n(ibfit, 3, std::copy_n(ibfit, 2, ostr.begin()));
  VERIFY( res == ostr.begin() + 5 );
  VERIFY( ostr == "12345" );
}

void test03()
{
  std::string ostr(5, '0');
  typedef std::istreambuf_iterator<char> istrb_ite;

  auto res = std::copy_n(istrb_ite(), 0, ostr.begin());
  VERIFY( res == ostr.begin() );
}

int main()
{
  test01();
  test02();
  test03();
  return 0;
}
