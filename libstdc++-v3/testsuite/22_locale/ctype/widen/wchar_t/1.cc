// 2002-05-24 bkoz

// Copyright (C) 2002, 2003, 2009 Free Software Foundation, Inc.
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


// 22.2.1.3.2 ctype<char> members

#include <locale>
#include <vector>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  typedef wchar_t wide_type;

  bool test __attribute__((unused)) = true;
  const locale loc_c = locale::classic();
  const ctype<wide_type>& ctype_c = use_facet<ctype<wide_type> >(loc_c); 

  basic_string<wide_type> 	wide(L"drusilla, louvinia, bayard");
  basic_string<char> 		narrow("drusilla, louvinia, bayard");
  vector<wide_type> 		wide_chars(narrow.length() + 1);
  
  // widen(char c) const
  for (size_t i = 0; i < narrow.length(); ++i)
    {
      char c = ctype_c.widen(narrow[i]);
      VERIFY( c == wide[i] );
    }

  // widen(const char* low, const char* high, charT* dest) const
  ctype_c.widen(&narrow[0], &narrow[0] + narrow.length(), &wide_chars[0]);  
  for (size_t i = 0; i < narrow.length(); ++i)
    VERIFY( wide_chars[i] == wide[i] );
}

int main() 
{
  test01();
  return 0;
}
