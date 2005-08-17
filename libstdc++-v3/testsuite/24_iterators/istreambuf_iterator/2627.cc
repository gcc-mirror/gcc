// 1999-06-28 bkoz

// Copyright (C) 1999, 2001, 2003 Free Software Foundation, Inc.
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

// 24.5.3 template class istreambuf_iterator

#include <sstream>
#include <iterator>
#include <testsuite_hooks.h>

// libstdc++/2627
void test03()
{
  bool test __attribute__((unused)) = true;
  const std::string s("free the vieques");

  // 1
  std::string res_postfix;
  std::istringstream iss01(s);
  std::istreambuf_iterator<char> isbufit01(iss01);
  for (std::size_t j = 0; j < s.size(); ++j, isbufit01++)
    res_postfix += *isbufit01;

  // 2
  std::string res_prefix;
  std::istringstream iss02(s);
  std::istreambuf_iterator<char> isbufit02(iss02);
  for (std::size_t j = 0; j < s.size(); ++j, ++isbufit02)
    res_prefix += *isbufit02;

  // 3 mixed
  std::string res_mixed;
  std::istringstream iss03(s);
  std::istreambuf_iterator<char> isbufit03(iss03);
  for (std::size_t j = 0; j < (s.size() / 2); ++j)
    {
      res_mixed += *isbufit03;
      ++isbufit03;
      res_mixed += *isbufit03;
      isbufit03++;
    }

  VERIFY ( res_postfix == res_prefix );
  VERIFY ( res_mixed == res_prefix );
}

int main()
{
  test03();
  return 0;
}
